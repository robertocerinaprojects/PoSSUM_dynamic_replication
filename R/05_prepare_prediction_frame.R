# Prepare population cells for single-wave or pooled vote2024 predictions.
# Run from the repository root, after script 04, in the same R session.
# If restarting R, first set run_dir to the saved fit's output folder.

library(data.table)
library(mltools)

# 1. Load the selected fit's metadata and the context used for fitting --------
load(file.path(run_dir, "training_data.RData"))
load("data/processed/context.RData")

SF_extended <- readRDS("data/downloaded/SF_extended_simple.rds")
setDT(SF_extended)
weight_columns <- grep("^N_star\\.[0-9]+$", names(SF_extended), value = TRUE)
# Keep every population row and every N_star column unchanged.

# Find the context row for each population cell, preserving the frame's order.
context_id <- OMNI_pred_scale[
  SF_extended,
  on = .(state_electoral.college, state_simple, state_abbreviation),
  which = TRUE
]
if (length(context_id) != nrow(SF_extended) || anyNA(context_id)) {
  stop("Each population cell must match exactly one area-predictor row.")
}

predictor_columns <- colnames(train_data_list$Z)
for (column in predictor_columns) {
  set(SF_extended, j = column,
      value = OMNI_pred_scale[[column]][context_id])
}
# set() adds columns in place, avoiding a copy of this large table each time.

# 2. Use the same area and category IDs as the fitted model -------------------
SF_extended[, area_id := match(state_electoral.college,
                               shape_US$state_electoral.college)]

variables <- c("gender", "ethnicity", "college_grad", "age_bins",
               "hh_income", "vote2020")
prefixes <- c("gen", "eth", "edu", "age", "inc", "v20")
category_id_columns <- paste0(prefixes, "_id")

for (i in seq_along(variables)) {
  variable <- variables[i]
  set(SF_extended, j = category_id_columns[i],
      value = match(SF_extended[[variable]], frame_levels[[variable]]))
}
# Education remains available for crosstabs; the model has no education effect.

# 4. Rebuild the past-vote-by-area interactions used by the model -------------
# Example: an indicator for voting D in 2020 multiplied by the area's
# scaled Green vote share.
past_vote <- mltools::one_hot(data.table(
  vote2020 = factor(SF_extended$vote2020, levels = frame_levels$vote2020)
))

for (j in train_data_list$INT + 1L) {
  predictor <- paste0("LP_pct.2020.votes.", make.names(train_data_list$V_labs[j]))
  for (indicator in names(past_vote)) {
    column <- paste0(predictor, "_AND_", indicator)
    set(SF_extended, j = column,
        value = SF_extended[[predictor]] * past_vote[[indicator]])
  }
}
rm(past_vote)

# 5. Inspect the prepared frame ---------------------------------------------
# Both unmatched counts should be zero before calculating predictions.
print(c(
  population_cells = nrow(SF_extended),
  population_draws = length(weight_columns),
  unmatched_area_rows = sum(is.na(SF_extended$area_id)),
  unmatched_category_rows = sum(!complete.cases(
    SF_extended[, ..category_id_columns]
  ))
))

# Keep the prepared frame in memory for the next script. It can be recreated
# from the downloaded file, so we do not save another large copy to disk.
