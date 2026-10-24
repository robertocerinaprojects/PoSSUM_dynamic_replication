# Run from the repository root, after scripts 01 and 02.
# Output: data/processed/training_data.RData

library(data.table)
library(mltools)

load("data/processed/context.RData")
load("data/processed/survey.RData")  # Includes the chosen High/Moderate setting.

# 1. Read the population frame's demographic categories -----------------------
# Use the same category IDs for survey respondents and later predictions.
# The large frame is needed here only for its category levels, not its weights.
var.name.list <- c("gender", "ethnicity", "college_grad", "age_bins",
                   "hh_income", "vote2020")
model.var.name.list <- c("gen", "eth", "edu", "age", "inc", "v20")

SF_extended <- as.data.table(readRDS("data/downloaded/SF_extended_simple.rds"))
frame_levels <- lapply(SF_extended[, ..var.name.list], function(x) levels(as.factor(x)))
rm(SF_extended)
invisible(gc())

# 2. Encode the outcome and individual characteristics ------------------------
# Keep the original outcome order, with D as the reference category.
N <- nrow(survey_object.clean.dep)
V <- factor(survey_object.clean.dep[[dep.var]],
            levels = c("D", "G", "K", "L", "R", "stay home", "W"))

id_list <- list()
for (i in seq_along(var.name.list)) {
  variable <- var.name.list[i]
  prefix <- model.var.name.list[i]
  id_list[[paste0(prefix, "_id")]] <- match(
    survey_object.clean.dep[[variable]], frame_levels[[variable]]
  )
  id_list[[paste0(prefix, "_N")]] <- length(frame_levels[[variable]])
}
# Education IDs are retained as metadata; the Stan model has no education effect.

# 3. Assemble the area predictors (Z) ----------------------------------------
# Each outcome has its original set of past-vote and/or ballot-access predictors.
# These columns were already scaled in script 01: do not scale them again here.
LP_labs <- list(
  D = "LP_pct.2020.votes.D",
  G = c("LP_pct.2020.votes.G", "LP_field_Jill.Stein..the.Green.Party.candidate"),
  K = "LP_field_Robert.F..Kennedy.Jr...who.is.not.affiliated.with.any.political.party",
  L = c("LP_pct.2020.votes.L", "LP_field_Chase.Oliver..the.Libertarian.Party.candidate"),
  R = "LP_pct.2020.votes.R",
  "stay home" = "LP_pct.2020.votes.stay.home",
  W = "LP_field_Dr..Cornel.West..who.is.not.affiliated.with.any.political.party"
)
LP_labs <- LP_labs[levels(V)]
Z_columns <- unique(unlist(LP_labs, use.names = FALSE))
Z <- as.matrix(survey_object.clean.dep[, ..Z_columns])
Z[is.na(Z)] <- 0  # Unknown-area respondents use a separate branch in Stan.
P <- lengths(LP_labs)

# Stan expects a rectangular index table. The unused entries are padded with 99;
# only the first P[j] entries of each row are used by the model.
pad_vector <- function(x, width) c(x, rep(99L, width - length(x)))
Z_id <- t(vapply(LP_labs, function(x) {
  pad_vector(match(x, colnames(Z)), max(P))
}, integer(max(P))))

# 4. Assemble past-vote-by-area interactions (X) ------------------------------
# Make one indicator per 2020 vote category, using the population frame's levels.
past_vote <- data.table(vote2020 = factor(
  survey_object.clean.dep$vote2020, levels = frame_levels$vote2020
))
past_vote <- as.matrix(mltools::one_hot(past_vote))

# Multiply each indicator by the corresponding area's 2020 vote share.
# K and W have no such predictor: retain their original single zero column.
INT_labs <- lapply(levels(V), function(choice) {
  predictor <- paste0("LP_pct.2020.votes.", make.names(choice))
  if (predictor %in% colnames(Z)) {
    x <- past_vote * Z[, predictor]
    colnames(x) <- paste0(predictor, "_AND_", colnames(past_vote))
  } else {
    x <- matrix(0, nrow = N, ncol = 1, dimnames = list(NULL, predictor))
  }
  x
})
names(INT_labs) <- levels(V)
X <- do.call(cbind, INT_labs)
P_INT <- vapply(INT_labs, ncol, integer(1))
X_id <- t(vapply(INT_labs, function(x) {
  pad_vector(match(colnames(x), colnames(X)), max(P_INT))
}, integer(max(P_INT))))

# INT refers to the non-reference outcomes: Stan adds 1 to get the outcome ID.
INT <- which(P_INT[-1] != 1L)

# 5. Match respondents to the spatial graph ---------------------------------
# The map's row order is also the order used by the neighbourhood graph.
area_N <- nrow(shape_US)
area_id <- match(survey_object.clean.dep$state_electoral.college,
                 shape_US$state_electoral.college)
area_id[is.na(area_id)] <- area_N + 1L
yes_area <- which(area_id <= area_N)
no_area <- which(area_id > area_N)

# 6. Collect and save the training inputs ------------------------------------
# Reuse the spatial inputs from script 01, including its original scale factors.
train_data_list <- c(list(
  N = N, V = as.integer(V), J = nlevels(V), V_labs = levels(V),
  yes_area = yes_area, yes_area_N = length(yes_area),
  no_area = no_area, no_area_N = length(no_area),
  Z = Z, P = P, P_sum = ncol(Z), P_max = max(P), Z_id = Z_id,
  X = X, P_INT = P_INT, P_INT_sum = ncol(X), P_INT_max = max(P_INT),
  X_id = X_id, INT = INT, INT_N = length(INT),
  area_id = area_id, area_N = area_N,
  kappa = icar.data$k,
  group_size = icar.data$group_size, group_idx = icar.data$group_idx,
  N_edges = icar.data$n_edges, node1 = icar.data$node1, node2 = icar.data$node2,
  comp_id = icar.data$comp_id,
  inv_sqrt_scaling_factor = icar.data$inv_sqrt_scale_factor
), id_list)

model_file <- "models/model_ai.survey_structured_optimised.stan"
save(train_data_list, frame_levels, LP_labs, survey_period, dep.var,
     speculation, model_file, file = "data/processed/training_data.RData")
message("Saved Stan inputs for ", N, " respondents (", speculation,
        ") to data/processed/training_data.RData")
