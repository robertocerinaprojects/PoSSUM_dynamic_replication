# Predict vote2024 and poststratify using the plausible population-count draws.
# Run after script 05 in the same R session, with the same run_dir.
# Output: poststratification.RData in the selected fit's output folder.

library(data.table)
library(cmdstanr)
library(posterior)
config <- if (exists("batch_config", inherits = FALSE)) batch_config else new.env(parent = baseenv())
if (!exists("batch_config", inherits = FALSE)) sys.source("R/analysis_config.R", envir = config)

# 1. Read the fitted effects -------------------------------------------------
fit <- readRDS(file.path(run_dir, "fit_object.rds"))
prediction_parameters <- c(
  "beta_s", "eta_s", "lambda_s", "xi_gen_s", "xi_eth_s",
  "xi_inc_s", "xi_age_s", "xi_v20_s", "xi_eta_s"
)
# The saved Stan data identifies pooled fits, including the original temporal
# model if you choose it in the configuration. Single-wave fits have no delta_s.
is_pooled <- "poll_N" %in% names(train_data_list)
if (is_pooled) prediction_parameters <- c(prediction_parameters, "delta_s")

# Extract the fitted effects, then arrange each parameter as an array
# with posterior draw as its first dimension.
pars_sims <- lapply(
  posterior::as_draws_rvars(
    fit$draws(
      variables = prediction_parameters,
      format = "draws_array"
    )
  ),
  posterior::draws_of
)
n_draws <- dim(pars_sims$beta_s)[1]
outcomes <- train_data_list$V_labs
voter_outcomes <- setdiff(outcomes, "stay home")
interaction_outcomes <- train_data_list$INT + 1L

# Pooled predictions describe the latest wave, estimated using all waves.
# Preserve delta_s[s, 1, j - 1] from the original prediction script.
wave_effect <- matrix(0, n_draws, length(outcomes) - 1L)
if (is_pooled) {
  stopifnot(prediction_poll_id == 1L, length(poll_levels) == train_data_list$poll_N)
  wave_effect[,] <- pars_sims$delta_s[, prediction_poll_id, , drop = FALSE]
}
prediction_wave <- if (is_pooled) poll_levels[prediction_poll_id] else survey_period

# Population cells must have known areas, categories and context predictors.
id_columns <- c("area_id", "gen_id", "eth_id", "inc_id", "age_id", "v20_id")
required_columns <- c(id_columns, colnames(train_data_list$Z))
stopifnot(dep.var == "vote2024", length(weight_columns) > 0L,
          all(complete.cases(SF_extended[, ..required_columns])))

# 2. Prepare inputs that are identical for every posterior draw --------------
# Select the exact predictor columns and order used when fitting each outcome.
# Cache these matrices once instead of rebuilding them for every draw.
Z_prediction <- vector("list", length(outcomes))
X_prediction <- vector("list", length(outcomes))
for (j in 2:length(outcomes)) {
  z_columns <- colnames(train_data_list$Z)[
    train_data_list$Z_id[j, seq_len(train_data_list$P[j])]
  ]
  Z_prediction[[j]] <- as.matrix(SF_extended[, ..z_columns])
  if (j %in% interaction_outcomes) {
    x_columns <- colnames(train_data_list$X)[
      train_data_list$X_id[j, seq_len(train_data_list$P_INT[j])]
    ]
    X_prediction[[j]] <- as.matrix(SF_extended[, ..x_columns])
  }
}

# The original ballot rule keeps K, W, G and L only where their scaled ballot
# indicator equals its maximum across the whole frame. Compute this once.
ballot_columns <- vapply(c("K", "W", "G", "L"), function(choice) {
  grep("^LP_field_", LP_labs[[choice]], value = TRUE)
}, character(1))
ballot_allowed <- lapply(ballot_columns, function(column) {
  x <- SF_extended[[column]]
  x == max(x)
})

# A single reporting date preserves the original national crosstab name.
# It labels the survey endpoint. Pooled temporal effects enter the logits below;
# the population frame is not repeated over survey dates.
SF_extended[, date := as.Date(sub(".*_", "", survey_period))]
crosstabs <- c("date", "region", "state_electoral.college", "state_simple",
               "gender", "ethnicity", "college_grad", "hh_income",
               "age_bins", "vote2020", "vote2022")

# Use every posterior draw. For each, sample one N_star column with replacement.
# The same selected column is used for every crosstab in that simulated election.
# The original had no fixed seed; setting one makes this pairing repeatable.
# prediction_seed is defined in R/analysis_config.R.
prediction_seed <- config$prediction_seed
set.seed(prediction_seed)
frame_draws <- data.table(
  sim_id = seq_len(n_draws),
  weight_column = base::sample(weight_columns, n_draws, replace = TRUE)
)

# 3. Calculate population-cell probabilities for one posterior draw ----------
predict_cells <- function(s) {
  # D is the reference outcome, with logit zero.
  logits <- matrix(0, nrow(SF_extended), length(outcomes),
                   dimnames = list(NULL, outcomes))

  for (j in 2:length(outcomes)) {
    logits[, j] <-
      pars_sims$beta_s[s, j - 1L] +
      as.vector(Z_prediction[[j]] %*%
                  pars_sims$eta_s[s, seq_len(train_data_list$P[j]), j - 1L]) +
      pars_sims$lambda_s[s, SF_extended$area_id, j - 1L] +
      pars_sims$xi_gen_s[s, SF_extended$gen_id, j - 1L] +
      pars_sims$xi_eth_s[s, SF_extended$eth_id, j - 1L] +
      pars_sims$xi_inc_s[s, SF_extended$inc_id, j - 1L] +
      pars_sims$xi_age_s[s, SF_extended$age_id, j - 1L] +
      pars_sims$xi_v20_s[s, SF_extended$v20_id, j - 1L]

    if (is_pooled) logits[, j] <- logits[, j] + wave_effect[s, j - 1L]

    if (j %in% interaction_outcomes) {
      logits[, j] <- logits[, j] + as.vector(
        X_prediction[[j]] %*%
          pars_sims$xi_eta_s[s, seq_len(train_data_list$P_INT[j]), j - 1L]
      )
    }
  }

  # exp(-Inf) = 0: exclude unavailable candidates before normalising.
  # This equals the original zero-and-renormalise ballot adjustment.
  for (choice in names(ballot_allowed)) {
    logits[!ballot_allowed[[choice]], choice] <- -Inf
  }

  # Subtracting each row's maximum preserves probabilities and avoids overflow.
  row_max <- do.call(pmax, as.data.frame(logits))
  probabilities <- exp(logits - row_max)
  probabilities / rowSums(probabilities)
}

# 4. Poststratify one simulated election at a time ----------------------------
# Retain small group-level results, rather than cell probabilities for all draws.
crosstab_draws <- setNames(lapply(crosstabs, function(group) {
  vector("list", n_draws)
}), crosstabs)

for (s in seq_len(n_draws)) {
  pred_vote <- as.data.table(predict_cells(s))
  pred_vote[, N_star := SF_extended[[frame_draws$weight_column[s]]]]

  for (group in crosstabs) {
    pred_vote[, group_value := SF_extended[[group]]]
    result <- pred_vote[, lapply(.SD, function(p) {
      sum(p * N_star) / sum(N_star)
    }), by = group_value, .SDcols = outcomes]
    setnames(result, "group_value", group)

    # Turnout = 1 - stay-home. Summing candidate probabilities avoids subtraction
    # rounding to zero when turnout is extremely small. Both describe adults.
    result[, turnout := rowSums(.SD), .SDcols = voter_outcomes]

    # Candidate shares are among voters; stay-home and turnout remain among adults.
    # Apply this adjustment AFTER aggregation, as in the original analysis.
    result[, (voter_outcomes) := lapply(.SD, function(p) p / turnout),
           .SDcols = voter_outcomes]
    result[, `:=`(RD_margin = R - D, sim_id = s)]
    crosstab_draws[[group]][[s]] <- result
  }

  if (s == 1L || s %% 10L == 0L || s == n_draws) {
    message("Poststratified draw ", s, " of ", n_draws)
  }
}

# 5. Save all simulated crosstabs and their population-draw pairings ----------
crosstab.pred_list <- lapply(crosstab_draws, data.table::rbindlist)
save(crosstab.pred_list, frame_draws, prediction_seed,
     survey_period, prediction_wave, dep.var, speculation,
     file = file.path(run_dir, "poststratification.RData"))
message("Saved poststratification draws to ", run_dir)

# Inspect national results. Shares are on the 0-1 scale; R-D can be negative.
print(head(crosstab.pred_list$date))
