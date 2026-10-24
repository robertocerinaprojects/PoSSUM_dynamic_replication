# Run from the repository root, after scripts 01-03.
# Sections 1-2 prepare the model; section 4 starts sampling.

# 1. Load the prepared data and select CmdStan --------------------------------
library(cmdstanr)
config <- if (exists("batch_config", inherits = FALSE)) batch_config else new.env(parent = baseenv())
if (!exists("batch_config", inherits = FALSE)) sys.source("R/analysis_config.R", envir = config)
sampling_settings <- config$sampling_settings
load("data/processed/training_data.RData")
set_cmdstan_path(Sys.getenv("CMDSTAN"))  # Set by the Codespaces configuration.

# 2. Compile the Stan program and select its inputs ---------------------------
# Compilation turns the Stan program into an executable. No model is fitted yet.
model <- cmdstan_model(model_file)

# Pass only variables declared in Stan's data block; keep labels and education
# metadata in train_data_list for later use.
stan_data <- train_data_list[names(model$variables()$data)]

# 3. Create a folder for this run --------------------------------------------
# The batch runner supplies its own folder. Interactive fits retain the
# existing timestamped folder convention and do not reuse an old run_dir.
run_dir <- if (exists("batch_run_dir", inherits = FALSE)) batch_run_dir else file.path(
  "output", dep.var, paste0("speculation.", speculation), survey_period,
  format(Sys.time(), "%Y%m%d-%H%M%S")
)
dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

# Save a snapshot of the Stan code alongside each fit:
save(train_data_list, frame_levels, LP_labs, survey_period, dep.var,
     speculation, model_file, model_type, input_periods, poll_levels,
     prediction_poll_id, file = file.path(run_dir, "training_data.RData"))
file.copy(c(model_file, "models/functions.stan"), run_dir)
writeLines(capture.output(sessionInfo()), file.path(run_dir, "sessionInfo.txt"))
saveRDS(sampling_settings, file.path(run_dir, "sampling_settings.rds"))

# 4. Fit the model -----------------------------------------------------------
# Read the settings in R/analysis_config.R. do.call() passes each named setting
# to model$sample(), just as if the arguments were written out here.
fit <- do.call(model$sample, c(
  list(data = stan_data, output_dir = run_dir), sampling_settings
))
if (any(fit$return_codes() != 0L)) stop("At least one Stan chain failed.")

# 5. Save the fitted object and inspect sampler diagnostics ------------------
# save_object() includes the draws and diagnostics in the saved R object.
# The per-chain CSV files also remain in run_dir.
fit$save_object(file.path(run_dir, "fit_object.rds"))
saveRDS(fit$time(), file.path(run_dir, "timing.rds"))
diagnostics <- fit$diagnostic_summary()
print(diagnostics)
saveRDS(diagnostics, file.path(run_dir, "diagnostics.rds"))

# Save R-hat and effective sample sizes for all model parameters.
parameter_summary <- fit$summary(variables = names(model$variables()$parameters))
write.csv(parameter_summary, file.path(run_dir, "parameter_summary.csv"),
          row.names = FALSE)
message("Saved model fit to ", run_dir)
