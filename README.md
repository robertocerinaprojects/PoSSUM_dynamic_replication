
<p align="center">
  <kbd>
  <img src="https://github.com/robertocerinaprojects/PoSSUM/assets/51832016/c6aab730-ec8c-4bda-a9a3-2024594b0ec8" alt="PossumLogo" width="200" />
  </kbd>
</p>


# PoSSUM: Live Replication

Refit the PoSSUM Stan models from the published synthetic surveys, calculate population estimates, and explore the results in R. Everything runs inside a GitHub Codespace, so you do not need to install R or Stan on your own computer.

**Start here:** follow steps 1–7 below. Run one configuration first, check its diagnostics, then run the full set of 12 configurations.

The current workflow covers data preparation, model fitting, poststratification, summary tables, and the existing vote-share and Electoral College plots. The paper's remaining evaluation and cross-wave comparison scripts are still being incorporated. Running this README does not yet regenerate every figure in the paper.

## Saved results

Saved model fits and poststratified predictions for all 12 configurations
are available in the [initial results release](https://github.com/robertocerinaprojects/PoSSUM_dynamic_replication/releases/tag/results-v0.1.0).

The release includes sampling settings, model snapshots, diagnostics,
and a checksum for the results archive.

To restore the results, download `PoSSUM-results-v0.1.0.tar.gz` into
the repository root and extract it:

```bash
tar -xzf PoSSUM-results-v0.1.0.tar.gz
```

This restores output/batch/. Follow section 6 to inspect a saved run
and generate its tables and plots.

These are initial results pending further convergence assessment.

## 1. Open a Codespace

Sign in to GitHub and open [this repository](https://github.com/robertocerinaprojects/PoSSUM_dynamic_replication). If you want to keep your own code changes on GitHub, click **Fork** first and follow these steps from your fork.

1. Click **Code → Codespaces**.
2. Open the options menu and select **New with options**.
3. Select the `main` branch and the **PoSSUM replication** dev container.
4. Choose **4 cores / 16 GB RAM**, matching the repository's configuration.
5. Click **Create codespace**.

The browser opens a VS Code workspace. The file list is on the left; commands run in the terminal at the bottom. Use **Terminal → New Terminal** if it is hidden.

### Keep track of the free allowance

Codespaces uses your GitHub account's compute and storage allowance. GitHub Free personal accounts currently include **120 core-hours and 15 GB-month of storage per month**. A 4-core Codespace consumes four core-hours for each hour it runs: 30 hours would use the full compute allowance if you had no other usage. Setup time counts too.

Check [your billing and usage page](https://github.com/settings/billing) before starting a long batch. Without a valid payment method, GitHub blocks further use when the allowance is exhausted. If you already have a payment method, check your Codespaces budget and its **Stop usage when budget limit is reached** setting. We have not established that all 12 analyses will finish within one free monthly allowance. See [GitHub's billing documentation](https://docs.github.com/en/billing/concepts/product-billing/github-codespaces) for current limits.

## 2. Wait for setup, then start R

The Codespace automatically runs [`.devcontainer/setup.R`](.devcontainer/setup.R). This installs the required Linux libraries, R packages, and **CmdStan 2.40.0**. The first setup can take a while. Wait for:

```text
Setup complete: analysis packages and CmdStan 2.40.0
```

If setup fails, inspect the first error. Once the problem is resolved, rerun setup in the **terminal**, after the original setup process has ended:

```bash
Rscript .devcontainer/setup.R
```

Start a clean R session in that terminal:

```bash
R --vanilla
```

You should now see the R prompt, `>`. **All following code blocks labelled `r` go into this R session**, without copying the prompt character.

Check that R is running from the repository's top-level folder and can find CmdStan:

```r
getwd()
stopifnot(file.exists("R/run_analyses.R"))
cmdstanr::cmdstan_version()
```

The version should be `2.40.0`. Keep the working directory at the repository root throughout the workflow; do not change into `R/`.

## 3. Preview the 12 configurations

Run the following in your R terminal -- 

```r
source("R/run_analyses.R")
run_analyses()
```

this simply prints the plan. **It does not download data or fit anything.**

| Synthetic survey period | High configuration | Moderate configuration |
| --- | --- | --- |
| 15–23 August 2024 | `wave1_high` | `wave1_moderate` |
| 7–12 September 2024 | `wave2_high` | `wave2_moderate` |
| 22–29 September 2024 | `wave3_high` | `wave3_moderate` |
| 5–15 October 2024 | `wave4_high` | `wave4_moderate` |
| 17–26 October 2024 | `wave5_high` | `wave5_moderate` |
| All five waves, 15 August–26 October 2024 | `pooled_high` | `pooled_moderate` |

All configurations model `vote2024` and use the plausible population-count draws, `N_star`, for poststratification.

- **High** applies no speculation-score filter, then retains complete records for the selected variables.
- **Moderate** requires a recorded speculation score of at most 80 for gender, ethnicity, age, household income, past vote, and 2024 vote intention. It then applies the same complete-case rule. Education is included in the completeness check but has no speculation-score filter.
- **Single-wave models** fit one survey wave at a time.
- **Pooled models** stack the five published surveys, retain each record's wave label, and fit the temporal model. Their saved predictions are for the **latest wave**, informed by all five waves.

The configuration list, model paths, sampling settings, and random seeds are in [`R/analysis_config.R`](R/analysis_config.R).

## 4. Run one configuration and check it

Start with the final synthetic survey wave under High speculation:

```r
first_run <- run_analyses("wave5_high", dry_run = FALSE)
```

The runner downloads the five survey CSVs and the population frame, prepares the inputs, compiles the selected Stan model, samples its posterior, and calculates the poststratified estimates. Downloads are reused on subsequent runs. The population-frame download is approximately **200 MiB**; its size in memory is larger.

Watch the terminal for chain progress and then messages such as `Poststratified draw 10 of 500`. Wait for the R prompt to return before entering the next commands.

The current defaults are **4 chains, 1,000 warmup iterations and 500 sampling iterations per chain, with thinning of 4**. This retains 125 posterior draws per chain, or 500 in total. These are the repository's current settings. The archived paper script used 8 chains, 4,750 warmup iterations and 250 sampling iterations per chain, also with thinning of 4.

Inspect the saved diagnostics:

```r
run_dir <- first_run$run_dir[[1]]
readRDS(file.path(run_dir, "diagnostics.rds"))

parameter_summary <- read.csv(file.path(run_dir, "parameter_summary.csv"))
head(
  parameter_summary[order(parameter_summary$rhat, decreasing = TRUE),
                    c("variable", "rhat", "ess_bulk", "ess_tail")],
  10
)
```

## 5. Run all configurations

Once the first run works and you have reviewed its diagnostics:

```r
runs <- run_analyses(dry_run = FALSE)
```

This processes all 12 configurations **one at a time**. Within each fit, four chains run in parallel. Keep the terminal available so you can check progress. Run only one analysis workflow at a time in a Codespace: batch and interactive scripts share `data/processed/`.

If you want a smaller selection instead, pass its configuration IDs, for example:

```r
runs <- run_analyses(
  c("pooled_high", "pooled_moderate"),
  dry_run = FALSE
)
```

### Stop and resume

Press **Ctrl+C** in the running R terminal to interrupt the current work. When you return to the same Codespace, start R again if necessary and run:

```r
source("R/run_analyses.R")
runs <- run_analyses(dry_run = FALSE)
```

Completed fits and completed predictions are reused separately. An interrupted fit restarts its sampling; it cannot continue from the last MCMC iteration. If the fit finished but poststratification was interrupted, the fit is reused and poststratification starts again.

Codespaces can stop after inactivity and has a [maximum continuous session of 12 hours](https://docs.github.com/en/codespaces/about-codespaces/understanding-the-codespace-lifecycle). All fits should finish long before that. 

## 6. Find results and make the tables and plots

Each batch run is saved under `output/batch/<configuration>/<run-specific-folder>/`. The last folder identifies the fit's inputs and settings; you do not need to type it manually.

After a successful batch, inspect the returned paths:

```r
runs[, c("run_id", "run_dir")]
```

To select a completed result after restarting R, use its saved index:

```r
index <- read.csv("output/batch/wave5_high/latest_run.csv")
run_dir <- index$run_dir[[1]]
```

### Inspect the saved files

| File in `run_dir` | Contents |
| --- | --- |
| `fit_object.rds` | Saved CmdStanR fit, including posterior draws and sampler diagnostics |
| `training_data.RData` | Stan inputs and the category, area, and wave metadata used by this fit |
| `sampling_settings.rds` | Sampling settings used for this fit |
| `diagnostics.rds` | Divergences, treedepth counts, and E-BFMI by chain |
| `parameter_summary.csv` | Posterior summaries, R-hat, and effective sample sizes for declared parameters |
| `timing.rds` | Sampling timings |
| `poststratification.RData` | National, geographic, demographic, and past-vote draws, with their population-draw pairings |
| `sessionInfo.txt` | R session and package information recorded when fitting |
| Stan source files and chain CSVs | Model snapshot and sampler output |

Repeat the diagnostic checks in step 4 for each fit you use. To inspect named effects, including effects reconstructed from the sampled parameters:

```r
library(cmdstanr)
fit <- readRDS(file.path(run_dir, "fit_object.rds"))
fit$summary(variables = c("phi", "omega"))
# For a pooled fit, also try: fit$summary(variables = "delta_s")
```

### Make plots for the selected configuration

With `run_dir` set as above, run these scripts in order:

```r
source("R/07_summarise_results.R")
source("R/08_plot_results.R")
source("R/09_electoral_college.R")
source("R/10_plot_electoral_college.R")
```

These use the saved predictions. The batch runner stops at script 06, so this plotting step is separate.

- `summaries/` contains CSV tables with medians and central 90% credible intervals.
- `figures/` contains **PDFs only**: national vote shares, R–D margins, Electoral College maps, popular-vote and electoral-vote distributions, and comparison plots.
- `electoral_college/` contains electoral-vote draws, win probabilities, and summary tables.

Script 10 downloads and caches its cartogram on first use. Its state comparison uses a fixed **30 October 2024 FiveThirtyEight forecast** for every configuration. For earlier waves, read this as a comparison with that later benchmark. Pooled plots show the full input fieldwork period in their subtitles, while the predictions refer to the latest wave.

To generate these outputs for every configuration in a successfully completed `runs` object:

```r
for (run_dir in runs$run_dir) {
  source("R/07_summarise_results.R")
  source("R/08_plot_results.R")
  source("R/09_electoral_college.R")
  source("R/10_plot_electoral_college.R")
}
```

If you restarted R, recover `runs` using the resume command in step 5 first. Repeating the plotting scripts replaces their tables and PDFs in the selected folders.

To view a PDF, find it in the Explorer, right-click it, choose **Download**, and open it on your computer. CSV tables can also be downloaded this way.

## 7. Save what you need and stop the Codespace

Downloaded inputs, processed data, and `output/` are excluded from Git. **Committing or pushing the repository does not back up your fitted models or results.** Save important results before deleting the Codespace or allowing it to expire.

For a complete copy of the generated outputs, leave R:

```r
q(save = "no")
```

Then run this in the **terminal**:

```bash
tar -czf PoSSUM-results.tar.gz output
```

Find `PoSSUM-results.tar.gz` in the Explorer, right-click it, and choose **Download**. Keep this archive on your computer; do not commit it to Git. Alternatively, download individual result files from `output/`. Keep whole run folders if you want to retain their diagnostics and model snapshots.

When finished, go to [your Codespaces](https://github.com/codespaces), open the Codespace's **…** menu, and choose **Stop codespace**. Closing the browser tab alone does not stop compute usage. A stopped Codespace still consumes storage allowance; delete it only after saving the files you need. See [GitHub's stop/start guide](https://docs.github.com/en/codespaces/developing-in-a-codespace/stopping-and-starting-a-codespace).

## Work through the analysis section by section

For a closer look at how a configuration is built, start a fresh R session from the repository root and run:

```r
analysis_id <- "wave5_high"  # Or another ID from the configuration table.
source("R/00_download_data.R")
source("R/01_prepare_context.R")
source("R/02_prepare_survey.R")
source("R/03_prepare_stan_data.R")
```

Inspect the objects, then continue when ready:

```r
source("R/04_fit_model.R")  # Starts a new fit.
source("R/05_prepare_prediction_frame.R")
source("R/06_poststratify.R")
```

You can open each script and run its annotated sections individually instead of sourcing the whole file. Keep scripts 05 and 06 in the same R session because the prepared population frame is held in memory.

This manual route creates a timestamped run folder and leaves `run_dir` available for scripts 07–10. It does not use the batch runner's reuse mechanism. Use `run_analyses()` for the full configuration sweep.

## Where to look in the repository

| Location | Purpose |
| --- | --- |
| `.devcontainer/` | Codespaces environment and installation script |
| `R/analysis_config.R` | Configuration list, model selection, and shared settings |
| `R/run_analyses.R` | Batch fitting and poststratification, with reuse of completed stages |
| `R/00`–`R/03` scripts | Download inputs and build the synthetic survey and Stan data |
| `R/04`–`R/06` scripts | Fit the model, prepare population cells, and poststratify |
| `R/07`–`R/10` scripts | Summarise and plot completed results |
| `models/` | Original and optimised Stan programs, shared `functions.stan`, and optimisation notes |
| `data/raw/` | Small input files distributed with the repository |
| `data/downloaded/` | Automatically downloaded synthetic surveys, population frame, and plotting reference data |
| `data/processed/` | Rebuildable intermediate data |
| `output/` | Your fitted models, diagnostics, predictions, tables, and plots |

The synthetic survey CSVs come from a pinned commit of the [original PoSSUM repository](https://github.com/robertocerinaprojects/PoSSUM). The population frame comes from this repository's [`data-v0.1.0` release](https://github.com/robertocerinaprojects/PoSSUM_dynamic_replication/releases/tag/data-v0.1.0). The workflow builds the training data directly from those synthetic survey records.
