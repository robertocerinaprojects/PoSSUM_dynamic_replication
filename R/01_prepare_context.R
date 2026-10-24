# Run from the repository root. Output: data/processed/context.RData

library(data.table)
library(sf)
library(geostan)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# 1. Electoral areas and their neighbourhood graph ----------------------------
load("data/raw/electoral.college_shape.RData")  # shape_US

C <- geostan::shape2mat(shape_US, style = "B")  # Binary neighbour connections: 0 or 1.
icar.data <- geostan::prep_icar_data(C)
# Neighbour warnings for Alaska and Hawaii are expected: they are isolated areas.

# 2. Area predictors and ballot access ---------------------------------------
load("data/raw/area_level.RData")       # predictor_area_level
load("data/raw/ballot_access.RData")    # ballot_access

names(ballot_access) <- make.names(names(ballot_access), unique = TRUE)
ballot_access$Electoral.votes <- as.numeric(as.character(ballot_access$Electoral.votes))

# Turn the candidate availability columns from Yes/No into 1/0.
candidate_cols <- grep("candidate|party", names(ballot_access), value = TRUE)
ballot_access[, (candidate_cols) := lapply(.SD, function(x) {
  as.numeric(factor(x, levels = c("No", "Yes"))) - 1
}), .SDcols = candidate_cols]
setnames(ballot_access, candidate_cols, paste0("field_", candidate_cols))

# Remove constant ballot indicators.
numeric_cols <- names(ballot_access)[vapply(ballot_access, is.numeric, logical(1))]
constant_cols <- numeric_cols[vapply(ballot_access[, ..numeric_cols], function(x) {
  var(x, na.rm = TRUE) == 0
}, logical(1))]
ballot_access[, (constant_cols) := NULL]

OMNI_pred <- merge(
  predictor_area_level, ballot_access,
  by.x = "state_abbreviation", by.y = "State...electors", all.x = TRUE
)

# 3. Standardize the predictors ----------------------------------------------
# Centre and scale each predictor using one row per electoral area.
area_columns <- c("state_electoral.college", "state_simple", "state_abbreviation")
cols_to_scale <- setdiff(names(OMNI_pred), area_columns)

scaled_temp <- OMNI_pred[, lapply(.SD, function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}), .SDcols = cols_to_scale]
setnames(scaled_temp, paste0("LP_", names(scaled_temp)))

OMNI_pred_scale <- cbind(OMNI_pred[, ..area_columns], scaled_temp)

# 4. Save the context for the next scripts -----------------------------------
save(shape_US, icar.data, OMNI_pred_scale,
     file = "data/processed/context.RData")
message("Saved spatial inputs and area predictors to data/processed/context.RData")
