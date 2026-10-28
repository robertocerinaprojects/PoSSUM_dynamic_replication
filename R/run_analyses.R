# Run from the repository root.
# Preview: source("R/run_analyses.R"); run_analyses()
# One pooled run: run_analyses("pooled_high", dry_run = FALSE)
# All 12 configurations: run_analyses(dry_run = FALSE)
# Repeat the same command to reuse completed fits and predictions.

# Small bookkeeping helpers. These do not change the statistical calculations.
analysis_file_hashes <- function(paths) {
  missing <- paths[!file.exists(paths)]
  if (length(missing)) stop("Missing input: ", paste(missing, collapse = ", "))
  tools::md5sum(paths)
}

analysis_hash <- function(settings) {
  path <- tempfile()
  on.exit(unlink(path))
  saveRDS(settings, path, version = 2)
  unname(tools::md5sum(path))
}

analysis_stage_complete <- function(run_dir, stage, signature) {
  marker <- file.path(run_dir, paste0(stage, "_complete.rds"))
  if (!file.exists(marker)) return(FALSE)
  saved <- tryCatch(readRDS(marker), error = function(e) NULL)
  if (is.null(saved) || !identical(saved$signature, signature)) return(FALSE)
  paths <- file.path(run_dir, names(saved$files))
  all(file.exists(paths)) && identical(unname(tools::md5sum(paths)), unname(saved$files))
}

analysis_mark_complete <- function(run_dir, stage, signature, filenames) {
  hashes <- analysis_file_hashes(file.path(run_dir, filenames))
  names(hashes) <- filenames
  saveRDS(list(signature = signature, files = hashes),
          file.path(run_dir, paste0(stage, "_complete.rds")))
}

run_analyses <- function(ids = NULL, dry_run = TRUE) {
# 1. Choose configurations. The default is all 12 configurations.
config <- new.env(parent = baseenv())
  sys.source("R/analysis_config.R", envir = config)
  if (is.null(ids)) ids <- config$analysis_runs$run_id[config$analysis_runs$ready]
  if (!length(ids) || anyNA(ids) || anyDuplicated(ids) ||
      any(!ids %in% config$analysis_runs$run_id)) stop("Choose distinct run_id values from analysis_config.R.")
  plan <- config$analysis_runs[match(ids, config$analysis_runs$run_id), ]
  rownames(plan) <- NULL
  if (any(!plan$ready | !plan$model %in% names(config$model_files))) {
    stop("Every selected analysis must have an enabled model configuration.")
  }
  input_periods <- lapply(seq_len(nrow(plan)), function(i) config$input_waves(plan[i, ]))
  model_files <- unname(config$model_files[plan$model])
  print(plan[, c("run_id", "survey_period", "speculation", "model")], row.names = FALSE)
  if (dry_run) {
    message("Preview only: no downloads or fits. Use dry_run = FALSE to run this selection.")
    return(invisible(plan))
  }

  # 2. Prepare common inputs once. Keep this separate from the user's R objects.
  # Only one batch may use the shared data/processed/ workspace at a time.
  dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
  lock <- "data/processed/analysis_batch.lock"
  if (!dir.create(lock, showWarnings = FALSE)) {
    stop("Another batch may be running. If it has stopped, remove data/processed/analysis_batch.lock and retry.")
  }
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  shared <- new.env(parent = globalenv())
  sys.source("R/00_download_data.R", envir = shared)
  sys.source("R/01_prepare_context.R", envir = shared)
  rm(shared)
  invisible(gc())
  if (nzchar(Sys.getenv("CMDSTAN"))) cmdstanr::set_cmdstan_path(Sys.getenv("CMDSTAN"))

  # File hashes identify the exact code and inputs. Hash the large frame once.
  shared_inputs <- analysis_file_hashes(c(
    "data/raw/electoral.college_shape.RData", "data/raw/area_level.RData",
    "data/raw/ballot_access.RData", "data/downloaded/SF_extended_simple.rds",
    "data/processed/context.RData"
  ))
  common_fit_code <- analysis_file_hashes(c(
    "R/01_prepare_context.R", "R/02_prepare_survey.R", "R/03_prepare_stan_data.R",
    "R/04_fit_model.R", "models/functions.stan"
  ))
  model_hashes <- analysis_file_hashes(unique(model_files))
  prediction_code <- analysis_file_hashes(c("R/05_prepare_prediction_frame.R", "R/06_poststratify.R"))
  packages <- c("data.table", "sf", "geostan", "spdep", "mltools", "cmdstanr")
  versions <- list(R = as.character(getRversion()),
    CmdStan = as.character(cmdstanr::cmdstan_version()),
    packages = setNames(vapply(packages, function(p) as.character(utils::packageVersion(p)), ""), packages))
  # Console refresh and concurrency do not determine which posterior is fitted.
  sampler <- config$sampling_settings[setdiff(names(config$sampling_settings), c("parallel_chains", "refresh"))]
  plan$run_dir <- NA_character_

  # 3. Fit and poststratify one configuration at a time.
  for (i in seq_len(nrow(plan))) {
    id <- plan$run_id[i]
    survey_files <- file.path("data/downloaded",
      paste0("survey_object.clean_", input_periods[[i]], ".csv"))
    fit_code <- c(common_fit_code, model_hashes[model_files[i]])
    fit_signature <- list(
      analysis = as.list(plan[i, c("run_id", "survey_period", "speculation", "model")]),
      outcome = config$dep.var, sampling = sampler,
      code = fit_code, inputs = c(shared_inputs, analysis_file_hashes(survey_files)),
      versions = versions
    )
    # Different settings or inputs get a separate folder; earlier fits are kept.
    run_dir <- file.path("output", "batch", id, analysis_hash(fit_signature))
    dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
    plan$run_dir[i] <- run_dir
    work <- new.env(parent = globalenv())
    work$batch_config <- config
    work$analysis_id <- id
    work$batch_run_dir <- run_dir
    work$run_dir <- run_dir
    message("\n", id, " -> ", run_dir)

    if (analysis_stage_complete(run_dir, "fit", fit_signature)) {
      message("Using completed fit.")
    } else {
      unlink(file.path(run_dir, c("fit_complete.rds", "prediction_complete.rds")))
      if (!identical(analysis_file_hashes(names(fit_code)), fit_code)) stop("Fit code changed during this batch. Restart the runner.")
      sys.source("R/02_prepare_survey.R", envir = work)
      sys.source("R/03_prepare_stan_data.R", envir = work)
      sys.source("R/04_fit_model.R", envir = work)
      if (!identical(analysis_file_hashes(names(fit_code)), fit_code)) stop("Fit code changed during this batch. Restart the runner.")
      saveRDS(fit_signature, file.path(run_dir, "run_settings.rds"))
      analysis_mark_complete(run_dir, "fit", fit_signature,
        c("fit_object.rds", "training_data.RData", "sampling_settings.rds",
          "diagnostics.rds", "parameter_summary.csv"))
    }

    prediction_signature <- list(fit = fit_signature, code = prediction_code,
      seed = config$prediction_seed, posterior = as.character(utils::packageVersion("posterior")))
    if (analysis_stage_complete(run_dir, "prediction", prediction_signature)) {
      message("Using completed poststratification.")
    } else {
      unlink(file.path(run_dir, "prediction_complete.rds"))
      if (!identical(analysis_file_hashes(names(prediction_code)), prediction_code)) stop("Prediction code changed during this batch. Restart the runner.")
      sys.source("R/05_prepare_prediction_frame.R", envir = work)
      sys.source("R/06_poststratify.R", envir = work)
      if (!identical(analysis_file_hashes(names(prediction_code)), prediction_code)) stop("Prediction code changed during this batch. Restart the runner.")
      analysis_mark_complete(run_dir, "prediction", prediction_signature, "poststratification.RData")
    }
    # One small index per configuration, for the later evaluation scripts.
    write.csv(plan[i, ], file.path("output", "batch", id, "latest_run.csv"), row.names = FALSE)
    rm(work)
    invisible(gc())
  }
  message("Finished. Fits, diagnostics and poststratification draws are in the folders below.")
  print(plan[, c("run_id", "run_dir")], row.names = FALSE)
  invisible(plan)
}
