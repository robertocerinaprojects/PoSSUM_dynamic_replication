# Run from the repository root, after scripts 01-03.
# Sections 1-2 prepare the model; section 4 starts the full sampling run.

# 1. Load the prepared data and select CmdStan --------------------------------
library(cmdstanr)
load("data/processed/training_data.RData")
set_cmdstan_path(Sys.getenv("CMDSTAN"))  # Set by the Codespaces configuration.

# 2. Compile the Stan program and select its inputs ---------------------------
# Compilation turns the Stan program into an executable. No model is fitted yet.
model <- cmdstan_model(model_file)

# Pass only variables declared in Stan's data block; keep labels and education
# metadata in train_data_list for later use.
stan_data <- train_data_list[names(model$variables()$data)]

# 3. Create a folder for this run --------------------------------------------
# Keep High and Moderate fits separate, with a new folder for each run.
run_dir <- file.path(
  "output", dep.var, paste0("speculation.", speculation), survey_period,
  format(Sys.time(), "%Y%m%d-%H%M%S")
)
dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

# Save a snapshot of the Stan code alongside each fit:
save(train_data_list, frame_levels, LP_labs, survey_period, dep.var,
     speculation, model_file, file = file.path(run_dir, "training_data.RData"))
file.copy(c(model_file, "models/functions.stan"), run_dir)
writeLines(capture.output(sessionInfo()), file.path(run_dir, "sessionInfo.txt"))

# 4. Fit the model -----------------------------------------------------------
# Original RStan settings: iter = 5000, warmup = 4750, thin = 4, chains = 8.
# CmdStanR separates warmup from the 250 subsequent iterations per chain.
# With thinning, this saves 63 posterior draws per chain, not 250.
# Run two chains at a time; all eight chains still run.
# The original script had no fixed seed. We set one for repeatable reruns.
fit <- model$sample(
  data = stan_data,
  seed = 20241026,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 500,
  thin = 4,
  max_treedepth = 15,
  adapt_delta = 0.8,
  refresh = 1,
  sig_figs = 18,
  output_dir = run_dir
)

# 5. Save the fitted object and inspect sampler diagnostics ------------------
# save_object() includes the draws and diagnostics in the saved R object.
# The per-chain CSV files also remain in run_dir.
fit$save_object(file.path(run_dir, "fit_object.rds"))
saveRDS(fit$time(), file.path(run_dir, "timing.rds"))
fit$diagnostic_summary()

# Save R-hat and effective sample sizes for all model parameters.
parameter_summary <- fit$summary(variables = names(model$variables()$parameters))
write.csv(parameter_summary, file.path(run_dir, "parameter_summary.csv"),
          row.names = FALSE)
message("Saved model fit to ", run_dir)
