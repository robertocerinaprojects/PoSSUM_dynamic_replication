# Shared settings for interactive work and the batch runner.
# This file only defines settings: sourcing it does not fit a model.

# 1. Analyses needed for the selected paper figures --------------------------
# Each pooled run uses all five published waves, retaining their wave labels.
survey_waves <- c(
  "2024-08-15_2024-08-23", "2024-09-07_2024-09-12",
  "2024-09-22_2024-09-29", "2024-10-05_2024-10-15",
  "2024-10-17_2024-10-26"
)
analysis_runs <- data.frame(
  run_id = c(
    paste0("wave", 1:5, "_high"),
    paste0("wave", 1:5, "_moderate"),
    "pooled_high",
    "pooled_moderate"
  ),
  survey_period = c(
    survey_waves,
    survey_waves,
    rep("2024-08-15_2024-10-26", 2)
  ),
  speculation = c(
    rep("High", 5),
    rep("Moderate", 5),
    "High", "Moderate"
  ),
  model = c(rep("single_wave", 10), rep("temporal", 2)),
  ready = TRUE,
  stringsAsFactors = FALSE
)

# All the selected figures concern 2024 vote intentions and N_star weights.
dep.var <- "vote2024"
model_files <- c(
  single_wave = "models/model_ai.survey_structured_optimised.stan",
  temporal = "models/model_ai.survey_structured_temporal_optimised.stan"
)

# Select complete published waves within a configuration's requested period.
# This also allows a future pooled configuration using a subset of the waves.
input_waves <- function(analysis) {
  if (analysis$model == "single_wave") return(analysis$survey_period)
  start <- substr(analysis$survey_period, 1, 10)
  end <- substr(analysis$survey_period, 12, 21)
  waves <- survey_waves[substr(survey_waves, 1, 10) >= start &
                         substr(survey_waves, 12, 21) <= end]
  if (length(waves) < 2L || paste(min(substr(waves, 1, 10)),
      max(substr(waves, 12, 21)), sep = "_") != analysis$survey_period) {
    stop("A pooled period must span at least two complete published waves.")
  }
  waves
}

# 2. Sampling settings ------------------------------------------------------
# These are the settings currently used in this repository.
# Each chain has 1,000 warmup iterations, then 500 sampling iterations.
# thin = 4 retains 125 draws per chain: 500 posterior draws in total.
# parallel_chains controls how many chains run at once, not how many are fitted.
# The archived paper script used 8 chains, 4,750 warmup and 250 sampling
# iterations, with thin = 4. Those are different computational settings.
sampling_settings <- list(
  seed = 20241026,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 500,
  thin = 4,
  max_treedepth = 15,
  adapt_delta = 0.8,
  refresh = 100,
  sig_figs = 18
)

# 3. Pair posterior draws with plausible population-count draws -------------
# Keep the same seed convention as the current poststratification script.
prediction_seed <- 20241027L
