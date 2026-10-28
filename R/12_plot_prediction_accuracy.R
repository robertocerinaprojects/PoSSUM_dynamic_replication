# Compare saved predictions with the observed 2024 Republican-Democratic margin.
# Adapted from the two-panel figures in 8_c.objective_predictive_power.R.
# Run from the repository root after script 11 and the selected batch analyses.
# Output: four PDFs and two CSV tables in output/evaluation/prediction_accuracy/.
# All plotted margins, intervals and errors are in percentage points.

library(data.table)
library(mgcv)

# 1. Select the completed runs ----------------------------------------------
# These are the four configurations used in the paper's comparison figures.
# Add another completed run ID here to make the same figure for that run.
run_ids <- c("wave5_high", "wave5_moderate", "pooled_high", "pooled_moderate")
output_dir <- "output/evaluation/prediction_accuracy"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

index_files <- file.path("output/batch", run_ids, "latest_run.csv")
stopifnot(all(file.exists(index_files)))
runs <- rbindlist(lapply(index_files, fread))
stopifnot(identical(runs$run_id, run_ids),
          all(file.exists(file.path(runs$run_dir, "poststratification.RData"))))

# 2. Load the observations and define area labels ---------------------------
load("data/processed/evaluation_data.RData")
area_labels <- setNames(c(state.abb, "DC", "NE-1", "NE-2", "NE-3", "ME-1", "ME-2"),
                       c(state.name, "District of Columbia", "Nebraska 1st",
                         "Nebraska 2nd", "Nebraska 3rd", "Maine 1st", "Maine 2nd"))
swing_states <- c("Arizona", "Nevada", "Michigan", "Wisconsin", "Pennsylvania",
                  "Georgia", "North Carolina")

# 3. Define the original accuracy measures ----------------------------------
# Each area has equal weight. Coverage uses strict interval bounds, as before.
# Positive bias means the predicted R-D margin is too Republican.
accuracy_metrics <- function(data) {
  data[, .(
    areas = .N,
    bias = mean(predicted - observed),
    rmse = sqrt(mean((observed - predicted)^2)),
    spearman = cor(predicted, observed, method = "spearman"),
    coverage = mean(observed > lower & observed < upper),
    accurate_calls = mean((observed > 0) == (predicted > 0))
  )]
}

# 4. Draw the two panels in the original style ------------------------------
plot_accuracy_panel <- function(data, swing_only = FALSE) {
  x <- data$predicted
  y <- data$observed
  limits <- if (swing_only) c(-20, 20) else c(-85, 55)
  plot(x, y, pch = NA, xlim = limits, ylim = limits,
       xlab = "PoSSUM", ylab = "Observation",
       main = if (swing_only) "swing states" else "electoral college districts")
  abline(0, 1, col = "gray60")
  abline(h = 0, v = 0, lty = 2, col = "gray70")

  # Coral marks opposite signs of the observed and predicted R-D margins.
  # A zero margin is not coloured as an opposite-sign prediction.
  wrong_sign <- (y > 0 & x < 0) | (x > 0 & y < 0)
  point_colour <- ifelse(wrong_sign, adjustcolor("lightcoral", 0.75),
                         adjustcolor("black", 0.75))
  interval_colour <- ifelse(wrong_sign, adjustcolor("lightcoral", 0.5),
                            adjustcolor("black", 0.25))
  segments(data$lower, y, data$upper, y, col = interval_colour)
  points(x, y, col = point_colour, pch = 19, cex = 0.5, lwd = 1.5)

  # This smooth curve describes the plotted relationship; it is not a new
  # election-model fit. Preserve the original GAM and its default settings.
  if (!swing_only) {
    curve_fit <- mgcv::gam(y ~ s(x))
    curve_y <- predict(curve_fit, newdata = data.frame(x = x))
    lines(x[order(x)], curve_y[order(x)], col = point_colour, lwd = 1.5)
  }

  metrics <- accuracy_metrics(data)
  legend("topleft", bty = "n", legend = c(
    paste0("Bias = ", round(metrics$bias, 2)),
    paste0("RMSE = ", round(metrics$rmse, 2)),
    paste0("corr = ", round(metrics$spearman, 2)),
    paste0("cover = ", round(metrics$coverage, 2)),
    paste0("Accurate calls = ", round(100 * metrics$accurate_calls, 1), "%")
  ))

  # Alternate labels to either side, in observed-margin order, as originally.
  label_size <- if (swing_only) 0.75 else 0.5
  offset <- if (swing_only) 1 else 10
  ordered <- order(y)
  for (j in seq_along(ordered)) {
    i <- ordered[j]
    x_label <- x[i] + if (j %% 2 == 0) offset else -offset
    text(x_label, y[i], data$state_abb[i], col = "white",
         cex = 1.1 * label_size + .Machine$double.eps)
    text(x_label, y[i], data$state_abb[i], col = "black",
         cex = label_size + .Machine$double.eps)
  }
}

write_accuracy_plot <- function(data, filename) {
  pdf(filename, width = 10.4, height = 5.2)
  on.exit(dev.off())
  par(mfrow = c(1, 2))
  plot_accuracy_panel(data)
  plot_accuracy_panel(data[state_electoral.college %in% swing_states],
                      swing_only = TRUE)
}

# 5. Summarise and plot one completed run at a time --------------------------
area_tables <- metric_tables <- vector("list", nrow(runs))
for (i in seq_len(nrow(runs))) {
  run <- runs[i]
  # Load into a separate environment so each run brings its own saved objects.
  saved <- new.env()
  load(file.path(run$run_dir, "poststratification.RData"), envir = saved)

  # The electoral-area crosstab has Maine/Nebraska districts. Append their
  # statewide estimates from state_simple; never average district percentages.
  draws <- copy(saved$crosstab.pred_list$state_electoral.college[
    , .(state_electoral.college, sim_id, RD_margin)])
  statewide <- saved$crosstab.pred_list$state_simple[
    state_simple %in% c("Maine", "Nebraska"),
    .(state_electoral.college = state_simple, sim_id, RD_margin)]
  stopifnot(!any(draws$state_electoral.college %in% c("Maine", "Nebraska")))
  draws <- rbindlist(list(draws, statewide))
  stopifnot(!anyDuplicated(draws[, .(state_electoral.college, sim_id)]),
            setequal(draws$state_electoral.college, names(area_labels)),
            all(is.finite(draws$RD_margin)))

  # Summarise the R-D difference within each draw, NOT the difference between
  # separately summarised R and D shares. quantile() defaults match the original.
  area <- draws[, {
    q <- 100 * quantile(RD_margin, c(0.05, 0.50, 0.95))
    .(lower = q[1], predicted = q[2], upper = q[3])
  }, by = state_electoral.college]
  observed_rows <- match(area$state_electoral.college,
                         results.wide$state_electoral.college)
  stopifnot(!anyNA(observed_rows))
  area[, observed := 100 * results.wide$RD_margin[observed_rows]]
  area <- area[!is.na(observed)]
  area[, state_abb := unname(area_labels[as.character(state_electoral.college)])]
  area[, `:=`(
    # The original per-area error has the opposite sign to aggregate bias.
    error = observed - predicted,
    abs_error = abs(observed - predicted),
    covered = observed > lower & observed < upper,
    correct_call = (observed > 0) == (predicted > 0)
  )]

  # Keep the original 56-area comparison, including DC and statewide ME/NE.
  # The swing-state comparison uses only the seven states listed above.
  metrics <- rbindlist(list(
    cbind(data.table(districts = "all"), accuracy_metrics(area)),
    cbind(data.table(districts = "swing"),
          accuracy_metrics(area[state_electoral.college %in% swing_states]))
  ))
  for (column in c("run_id", "survey_period", "speculation", "run_dir")) {
    set(area, j = column, value = run[[column]])
    set(metrics, j = column, value = run[[column]])
  }
  area_tables[[i]] <- area
  metric_tables[[i]] <- metrics
  write_accuracy_plot(area, file.path(output_dir, paste0(run$run_id, ".pdf")))
  message("Saved prediction-accuracy plot: ", run$run_id)
  rm(saved, draws, statewide)
}

# 6. Save the plotted numbers and accuracy summaries ------------------------
# coverage and accurate_calls are proportions (0-1); errors are percentage points.
area_predictions <- rbindlist(area_tables)
accuracy_summary <- rbindlist(metric_tables)
fwrite(area_predictions, file.path(output_dir, "area_predictions.csv"))
fwrite(accuracy_summary, file.path(output_dir, "accuracy_summary.csv"))
print(accuracy_summary[, .(run_id, districts, areas, bias, rmse, spearman,
                           coverage, accurate_calls)], digits = 4)
message("Saved PDFs and evaluation tables to ", output_dir)
