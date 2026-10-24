# Run from the repository root. Output: data/processed/survey.RData
# First analysis: the final survey wave, vote2024; choose High or Moderate below.

library(data.table)
load("data/processed/context.RData")  # Includes OMNI_pred_scale from script 01.

# 1. Identify this analysis ---------------------------------------------------
survey_period <- "2024-10-17_2024-10-26"
dep.var <- "vote2024"
speculation <- "High"  # Choose "High" or "Moderate".

# 2. Read the published synthetic survey ------------------------------------
survey_file <- file.path(
  "data/downloaded", paste0("survey_object.clean_", survey_period, ".csv")
)
survey_object.clean <- as.data.table(read.csv(
  survey_file, na.strings = c("", "NA"), stringsAsFactors = FALSE,
  check.names = FALSE
))
survey_object.clean$date <- as.Date(survey_object.clean$date)

# 3. Select variables, apply the speculation rule, and remove incomplete rows
model.vars <- c("gender", "ethnicity", "age_bins", "college_grad",
                "hh_income", "vote2020")
selected_columns <- c("date", "state_electoral.college", "state_simple",
                      model.vars, dep.var)
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
save(survey_object.clean.dep, survey_period, dep.var, speculation,
     file = "data/processed/survey.RData")
message("Saved ", nrow(survey_object.clean.dep), " survey records (", speculation,
        ") to data/processed/survey.RData")
