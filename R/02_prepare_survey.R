# Run from the repository root. Output: data/processed/survey.RData
# Interactive example: analysis_id <- "pooled_high", then run this script.
# Without a selection, use the final wave with High speculation.

library(data.table)
load("data/processed/context.RData")  # Includes OMNI_pred_scale from script 01.

# 1. Identify this analysis ---------------------------------------------------
# The runner supplies fixed settings for its batch. Interactive work rereads
# the configuration file, so your edits take effect when you rerun this section.
config <- if (exists("batch_config", inherits = FALSE)) batch_config else new.env(parent = baseenv())
if (!exists("batch_config", inherits = FALSE)) sys.source("R/analysis_config.R", envir = config)
if (!exists("analysis_id", inherits = FALSE)) analysis_id <- "wave5_high"
analysis <- config$analysis_runs[config$analysis_runs$run_id == analysis_id, ]
if (nrow(analysis) != 1L) stop("Unknown analysis_id: ", analysis_id)
if (!analysis$ready || !analysis$model %in% names(config$model_files)) {
  stop("This analysis has no enabled model configuration.")
}
survey_period <- analysis$survey_period
speculation <- analysis$speculation
dep.var <- config$dep.var
model_type <- analysis$model
model_file <- unname(config$model_files[model_type])

# 2. Read the published synthetic survey ------------------------------------
input_periods <- config$input_waves(analysis)
survey_parts <- lapply(input_periods, function(period) {
  survey_file <- file.path("data/downloaded", paste0("survey_object.clean_", period, ".csv"))
  survey <- as.data.table(read.csv(survey_file, na.strings = c("", "NA"),
                                 stringsAsFactors = FALSE, check.names = FALSE))
  # Every record keeps its source wave. Do not average waves or remove records
  # here: these are the already-published, cleaned synthetic surveys.
  if (model_type == "temporal") survey[, poll_lab := period]
  survey
})
survey_object.clean <- rbindlist(survey_parts, use.names = TRUE, fill = TRUE)
rm(survey_parts)
survey_object.clean$date <- as.Date(survey_object.clean$date)

# 3. Select variables, apply the speculation rule, and remove incomplete rows
model.vars <- c("gender", "ethnicity", "age_bins", "college_grad",
                "hh_income", "vote2020")
selected_columns <- c("date", "state_electoral.college", "state_simple",
                      model.vars, dep.var)
if (model_type == "temporal") selected_columns <- c(selected_columns, "poll_lab")
# Start from the unfiltered survey each time, so switching modes is reversible.
survey_object.clean.dep <- copy(survey_object.clean[, ..selected_columns])

if (!speculation %in% c("High", "Moderate")) {
  stop('Set speculation to "High" or "Moderate".')
}

# High applies no score filter. Moderate follows an 80% threshold for these
# six variables; college_grad is deliberately excluded from this score filter.
if (speculation == "Moderate") {
  speculation_vars <- c("gender", "ethnicity", "age_bins", "hh_income",
                        "vote2020", "vote2024")
  for (variable in speculation_vars) {
    score <- as.numeric(survey_object.clean[[paste0("speculation...", variable)]])
    # Keep scores of 80 or less. Missing scores also become missing values.
    survey_object.clean.dep[is.na(score) | score > 80, (variable) := NA_character_]
  }
}

# Apply complete-case rule after masking, before joining area context.
survey_object.clean.dep <- survey_object.clean.dep[complete.cases(survey_object.clean.dep)]

# 4. Attach the already-scaled area predictors -------------------------------
# Match by area: the predictors are constant over time, and the survey keeps its date.
# all.x = TRUE retains records with an unknown area (USA); their context is missing.
survey_object.clean.dep <- merge(
  survey_object.clean.dep, OMNI_pred_scale,
  by = c("state_electoral.college", "state_simple"), all.x = TRUE
)
setorder(survey_object.clean.dep, state_electoral.college, state_simple, date)

# 5. Save a separate prepared survey -----------------------------------------
save(survey_object.clean.dep, survey_period, dep.var, speculation, model_type,
     model_file, input_periods,
     file = "data/processed/survey.RData")
message("Saved ", nrow(survey_object.clean.dep), " survey records (", speculation,
        ") to data/processed/survey.RData")
