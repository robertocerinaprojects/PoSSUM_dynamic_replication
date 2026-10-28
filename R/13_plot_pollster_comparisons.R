# Reproduce the six pollster-comparison figures and delta.pdf from the paper.
# Run from the repository root after script 11 and the wave5_high analysis.
# Output: seven PDFs with their original filenames in output/evaluation/.

library(data.table)
library(mgcv)
library(gtools)
library(bayestestR)
library(circlize)

# 1. Read the saved final-wave High predictions and election results ---------
run <- fread("output/batch/wave5_high/latest_run.csv")
stopifnot(nrow(run) == 1L, run$run_id == "wave5_high")
saved <- new.env()
load(file.path(run$run_dir, "poststratification.RData"), envir = saved)
load("data/processed/evaluation_data.RData")

# Include statewide Maine/Nebraska alongside their electoral districts.
PoSSUM <- copy(saved$crosstab.pred_list$state_electoral.college[
  , .(state_electoral.college, RD_margin, sim_id)])
statewide <- saved$crosstab.pred_list$state_simple[
  state_simple %in% c("Maine", "Nebraska"),
  .(state_electoral.college = state_simple, RD_margin, sim_id)]
stopifnot(!any(PoSSUM$state_electoral.college %in% c("Maine", "Nebraska")))
PoSSUM <- rbindlist(list(PoSSUM, statewide))
PoSSUM[, pollster := "PoSSUM"]
rm(saved, statewide)

# 2. Download the published poll crosstabs and the 538 ratings snapshot -------
# Both files are pinned to commits and checked before use.
# The ratings revision dates from 13 September 2024.
input_dir <- "data/downloaded/evaluation"
dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)
download_reference <- function(url, filename, checksum) {
  path <- file.path(input_dir, filename)
  if (!file.exists(path)) download.file(url, path, mode = "wb")
  if (unname(tools::md5sum(path)) != checksum) {
    stop("Reference file does not match the expected snapshot: ", path)
  }
  path
}
polls_file <- download_reference(
  paste0("https://raw.githubusercontent.com/robertocerinaprojects/PoSSUM/",
         "b9005a80c7f6dbbf6e1f48c9fa625dfae24c8b22/",
         "evaluation_data/clean_crosstabs_database.csv"),
  "clean_crosstabs_database.csv", "349cb6e53a33065eaa63e3b29f0fa817")
ratings_file <- download_reference(
  paste0("https://raw.githubusercontent.com/fivethirtyeight/data/",
         "46b626e9292587ff394d7aa6d02f45dc06697cc8/",
         "pollster-ratings/pollster-ratings-combined.csv"),
  "pollster-ratings-combined.csv", "950f9f58de886ad64cd7a249538f0692")
cross <- fread(polls_file)
pollster_rank <- fread(ratings_file)

# 3. Select each pollster's latest poll in each area -------------------------
# Preserve the original selection: no extra cutoff at PoSSUM's fieldwork end.
state_polls <- cross[grepl("state_", condition)]
state_polls[, pollster := factor(as.character(pollster))]
state_polls_latest <- state_polls[, .SD[end.date == max(end.date)],
                                  by = .(pollster, condition)]
setnames(state_polls_latest, "condition", "state_electoral.college")
state_polls_latest[, state_electoral.college :=
                     tools::toTitleCase(gsub("state_", "", state_electoral.college))]
state_polls_latest <- state_polls_latest[
  , .(state_electoral.college, pollster, response, N)]
state_polls_latest <- reshape(state_polls_latest, direction = "wide",
                             timevar = "response",
                             idvar = c("state_electoral.college", "pollster"))
poll_columns <- grep("^N[.]", names(state_polls_latest), value = TRUE)
for (column in poll_columns) {
  set(state_polls_latest, i = which(is.na(state_polls_latest[[column]])),
      j = column, value = 0)
}

# 4. Simulate pollster vote shares using the original Dirichlet calculation --
# Counts are the Dirichlet parameters, including zeros; no pseudocounts added.
# The original had no fixed seed. This seed makes these simulations repeatable.
n_sims <- max(PoSSUM$sim_id)
set.seed(20241026)
poll_draws <- vector("list", nrow(state_polls_latest))
for (i in seq_len(nrow(state_polls_latest))) {
  alpha <- as.numeric(unlist(state_polls_latest[i, ..poll_columns]))
  draws <- gtools::rdirichlet(n_sims, alpha)
  colnames(draws) <- sub("^N[.]", "", poll_columns)
  poll_draws[[i]] <- data.table(
    state_electoral.college = state_polls_latest$state_electoral.college[i],
    RD_margin = draws[, "R"] - draws[, "D"],
    sim_id = seq_len(n_sims), pollster = state_polls_latest$pollster[i])
}
comparison <- rbindlist(c(poll_draws, list(PoSSUM)), use.names = TRUE)
comparison <- merge(comparison,
  results.wide[, .(state_electoral.college, obs_RDmargin = RD_margin)],
  by = "state_electoral.college", all.x = TRUE)
comparison[, `:=`(RD_margin = 100 * RD_margin, obs_RDmargin = 100 * obs_RDmargin)]

# Match each pollster to PoSSUM over exactly the same areas and draw IDs.
possum_wide <- reshape(comparison[pollster == "PoSSUM"], timevar = "pollster",
  direction = "wide", idvar = c("state_electoral.college", "sim_id", "obs_RDmargin"))
comparison <- merge(comparison[pollster != "PoSSUM"], possum_wide,
  by = c("state_electoral.college", "sim_id", "obs_RDmargin"), all.x = TRUE)
stopifnot(!anyNA(comparison[, .(RD_margin, RD_margin.PoSSUM, obs_RDmargin)]))

# 5. Calculate overlap and accuracy on each pollster's covered areas ---------
# First calculate density overlap within each area, then average across areas.
# N below counts areas, not survey respondents. Areas have equal weight.
summary_OVL <- comparison[, .(
  OVL = as.numeric(bayestestR::overlap(RD_margin.PoSSUM, RD_margin))),
  by = .(pollster, state_electoral.college)]
summary_OVL <- summary_OVL[, .(OVL_mean = mean(OVL), N = .N), by = pollster]

comparison_summary <- comparison[, lapply(.SD, quantile, c(0.05, 0.50, 0.95)),
  by = .(state_electoral.college, pollster, obs_RDmargin),
  .SDcols = c("RD_margin", "RD_margin.PoSSUM")]
comparison_summary[, quantile := rep(c("5%", "50%", "95%"), .N / 3)]
comparison_summary <- reshape(comparison_summary, direction = "wide",
  timevar = "quantile", idvar = c("state_electoral.college", "pollster", "obs_RDmargin"))

score_predictions <- function(observed, predicted, lower, upper) {
  data.table(
    coverage90 = mean(observed > lower & observed < upper),
    bias = mean(predicted - observed),
    rmse = sqrt(mean((observed - predicted)^2)),
    mae = mean(abs(observed - predicted)),
    rho = cor(predicted, observed, method = "spearman"),
    # Original rule: strictly matching signs. A zero margin is not a correct call.
    accuracy = mean((observed > 0 & predicted > 0) | (observed < 0 & predicted < 0)))
}
summary_evals <- comparison_summary[, {
  poll <- score_predictions(obs_RDmargin, `RD_margin.50%`, `RD_margin.5%`, `RD_margin.95%`)
  possum <- score_predictions(obs_RDmargin, `RD_margin.PoSSUM.50%`,
                              `RD_margin.PoSSUM.5%`, `RD_margin.PoSSUM.95%`)
  setnames(possum, paste0(names(possum), ".PoSSUM"))
  cbind(poll, possum, N = .N)
}, by = pollster]
summary_evals <- merge(summary_OVL, summary_evals, by = c("pollster", "N"), all = TRUE)
setorder(summary_evals, N)

# 6. Attach 538 ratings -----------
# Keep every matching ratings row when averaging; unmatched names remain missing.
rating_names <-
c(`The New York Times/Siena College` = "The New York Times/Siena College", 
`Marquette University Law School` = "Marquette University Law School", 
YouGov = "YouGov", `Monmouth University Polling Institute` = "Monmouth University Polling", 
`Marist College` = "Marist College", `Suffolk University` = "Suffolk University", 
`Data Orbital` = "Data Orbital", `Emerson College` = "Emerson College", 
`Selzer & Co.` = "Selzer", SurveyUSA = "SurveyUSA", `Beacon Research/Shaw & Co. Research` = "Beacon Research/Shaw & Company Research", 
`Quinnipiac University` = "Quinnipiac University", `MassINC Polling Group` = "MassINC Polling Group", 
`Christopher Newport University Wason Center for Civic Leadership` = "Christopher Newport University", 
`Siena College` = "Siena College", AtlasIntel = "AtlasIntel", 
`Echelon Insights` = "Echelon Insights", `Data for Progress` = "Data for Progress", 
`Remington Research Group` = "Remington", `Roanoke College Institute for Policy and Opinion Research` = "Roanoke College", 
`University of California Berkeley Institute of Governmental Studies` = "UC Berkeley", 
`Research Co.` = "Research Co.", `Saint Anselm College Survey Center` = "St. Anselm College", 
`St. Pete Polls` = "St. Pete Polls", `University of Massachusetts Department of Political Science/YouGov` = "YouGov", 
`Noble Predictive Insights` = "Noble Predictive Insights", `RMG Research` = "RMG Research", 
`Susquehanna Polling & Research Inc.` = "Susquehanna Polling & Research", 
`Pan Atlantic Research` = "Pan Atlantic Research", Cygnal = "Cygnal", 
`Research & Polling Inc.` = "Research & Polling", `Praecones Analytica` = "Praecones Analytica", 
GQR = "GQR", `National Research` = "National Research", InsiderAdvantage = "InsiderAdvantage", 
`Alaska Survey Research` = "Alaska Survey Research", `Morning Consult` = "Morning Consult", 
`Targoz Market Research` = "Targoz Market Research", `Redfield & Wilton Strategies` = "Redfield & Wilton Strategies", 
SoonerPoll.com = "SoonerPoll.com", `TIPP Insights` = "TIPP", 
`Global Strategy Group` = "Global Strategy Group", `WPA Intelligence` = "WPA Intelligence", 
`J.L. Partners` = "J.L. Partners", `Tarrance Group` = "Tarrance Group", 
`CWS Research` = "CWS Research", `Keating Research` = "Keating Research", 
`Benenson Strategy Group` = "Benenson Strategy Group", `Stetson University Center for Public Opinion Research` = "Stetson University Center for Public Opinion Research", 
`Impact Research` = "Impact Research", `Spry Strategies` = "Spry Strategies", 
`Change Research` = "Change Research", `Slingshot Strategies` = "Slingshot Strategies", 
`Public Policy Polling` = "Public Policy Polling", Wick = "Wick", 
`Digital Research` = "Digital Research", `Victory Insights` = "Victory Insights", 
`RABA Research` = "RABA Research", `BK Strategies` = "BK Strategies", 
`OnMessage Inc.` = "OnMessage Inc.", `North Star Opinion Research` = "North Star Opinion Research", 
`co/efficient` = "co/efficient", `Patriot Polling` = "Patriot Polling", 
`Cherry Communications` = "Cherry Communications", `Hendrix College` = "Hendrix College", 
`Trafalgar Group` = "Trafalgar Group", `McLaughlin & Associates` = "McLaughlin & Associates", 
`Dartmouth College Nelson A. Rockefeller Center for Public Policy and the Social Sciences` = "Dartmouth Poll", 
`Reconnect Research/Roanoke College Institute for Policy and Opinion Research` = "Roanoke College", 
`Univision/Latino Decisions/North Star Opinion Research` = "North Star Opinion Research"
)
pollster_rank[, pollster := factor(unname(rating_names[pollster]))]
pollster_rank <- pollster_rank[!is.na(numeric_grade), .(
  numeric_grade = weighted.mean(numeric_grade,
                               number_polls_pollster_time_weighted, na.rm = TRUE)
), by = pollster]
summary_evals <- merge(summary_evals, pollster_rank, by = "pollster", all.x = TRUE)

# 7. Plot the comparisons ------
# Colours follow the original LAB-space interpolation from circlize.
# The same scale is used for both estimates within a comparison.
pollster_colours <- function(values, metric, reference_values = values,
                             difference = FALSE) {
  bounds <- range(reference_values, na.rm = TRUE)
  midpoint <- mean(reference_values, na.rm = TRUE)
  if (metric == "bias") {
    breaks <- c(bounds[1], 0, bounds[2])
    colours <- c("dodgerblue", "gray", "orangered")
  } else {
    breaks <- if (difference) c(bounds[1], 0, bounds[2]) else
      if (metric == "rmse") c(0, midpoint, bounds[2]) else
        c(bounds[1], midpoint, bounds[2])
    colours <- if (metric == "rmse") c("olivedrab", "gray", "red4") else
      c("red4", "gray", "olivedrab")
  }
  circlize::colorRamp2(breaks, colours)(values)
}

# Make one of the six pollster comparison figures.
# N counts the areas covered by each pollster. It controls point size only.
write_pollster_comparison <- function(summary_evals, metric, output_dir) {
  minimum_N <- if (metric %in% c("rho", "accuracy")) 3 else 1
  dt <- summary_evals[N > minimum_N]
  overlap_only <- metric == "OVL"
  y <- dt[[if (overlap_only) "OVL_mean" else metric]]
  yH <- if (overlap_only) NULL else dt[[paste0(metric, ".PoSSUM")]]
  x <- factor(dt$pollster)
  ord <- order(y)
  cex_vals <- (if (overlap_only) 3 else 4) * (dt$N / max(dt$N))[ord]
  col_y <- pollster_colours(y, metric, reference_values = c(y, yH))
  if (!overlap_only) {
    col_yhat <- pollster_colours(yH, metric, reference_values = c(y, yH))
    difference_colours <- pollster_colours(yH[ord] - y[ord], metric,
                                           difference = TRUE)
  }

  suffixes <- c(bias = "bias", rmse = "rmse", rho = "corr",
                accuracy = "accuracy", coverage90 = "coverage", OVL = "OVL")
  titles <- c(bias = "Area-level Pro-Republican Bias",
              rmse = "Area-level RMSE on R-D Margin",
              rho = "Area-level Spearman Corr. on R-D Margin",
              accuracy = "Area-level Winner Classification Accuracy",
              coverage90 = "Area-level 90% Coverage",
              OVL = "Area-level Density Overlap with PoSSUM")
  pdf(file.path(output_dir, paste0("pollsters_", suffixes[metric], "_comparison.pdf")),
      width = 12.5, height = 13.5)
  on.exit(dev.off())
  wide_scale <- metric %in% c("bias", "rmse")
  par(oma = c(0, 22.5, 3, if (wide_scale) 0 else 3),
      mar = c(1, 3, 3, if (wide_scale) 1 else 3))

  limits <- switch(metric,
    bias = c(min(y) - 5, max(y) + 5), rmse = c(0, max(y) + 2.5),
    rho = c(-1, 1), c(0, 1))
  grid <- switch(metric,
    bias = seq(round(min(y[ord]) - 10), round(max(y[ord]) + 10), 1),
    rmse = seq(0, round(max(y[ord]) + 10), 1),
    rho = seq(-1.1, 1.1, by = 0.1),
    coverage90 = seq(0, 1, by = 0.1), seq(-0.1, 1.1, by = 0.1))
  ticks <- switch(metric,
    bias = seq(limits[1], limits[2], length.out = 10),
    rmse = seq(limits[1], limits[2], length.out = 10),
    rho = seq(-1.1, 1.1, by = 0.1),
    coverage90 = seq(0, 1, by = 0.2), seq(0, 1, by = 0.1))

  plot(y[ord], seq_along(y), xlim = limits, yaxt = "n", pch = NA,
       xlab = "", ylab = "", main = "", xaxt = "n")
  abline(h = seq(1 - 10, length(y) + 10, 1), v = grid,
         col = adjustcolor("gray60", 0.25))
  if (metric != "bias") abline(v = mean(y), lty = 1)
  axis(3, at = ticks, labels = if (wide_scale) round(ticks) else ticks)
  mtext(titles[metric], side = 3, outer = TRUE, cex = 2)
  points(y[ord], seq_along(y), pch = 17, cex = cex_vals, col = col_y[ord])
  if (!overlap_only) {
    points(yH[ord], seq_along(y), pch = 16, cex = cex_vals, col = col_yhat[ord])
    # Equal estimates need no arrow (and otherwise produce a zero-length warning).
    rows <- seq_along(y)
    moved <- y[ord] != yH[ord]
    arrows(y[ord][moved], rows[moved], yH[ord][moved], rows[moved],
           length = 0.05, angle = 50, code = 2, lwd = 1.1)
  }
  if (metric == "bias") abline(v = 0)
  axis(2, at = seq_along(y), labels = FALSE, las = 2)
  label_offset <- if (wide_scale) 1 else if (overlap_only) 0.05 else 0.1
  text(par("usr")[1] - label_offset, seq_along(y), labels = x[ord],
       col = col_y[ord], adj = 1, cex = pmax(pmin(cex_vals, 1.5), 0.7),
       xpd = NA, font = 2)

  if (!overlap_only) {
    # Labels show PoSSUM minus the comparison pollster, on the original scale.
    offset <- if (wide_scale) 1 else if (metric == "rho") 0.075 else 0.05
    for (k in seq_along(y)) {
      diff_val <- round(yH[ord][k] - y[ord][k], if (wide_scale) 1 else 2)
      diff_val <- ifelse(diff_val > 0, paste0("+", diff_val), as.character(diff_val))
      text(max(y[ord][k], yH[ord][k]) + offset, k,
           labels = bquote(bold(.(diff_val))), cex = 0.9, adj = 0,
           xpd = if (wide_scale) FALSE else NA, col = difference_colours[k])
    }
  }
  size_labels <- paste0("N = ", c(min(dt$N), median(dt$N), max(dt$N)))
  size_values <- c(min(cex_vals), median(cex_vals), max(cex_vals))
  if (overlap_only) {
    legend("topleft", legend = c("Avg.", size_labels),
           pch = c(NA, 17, 17, 17), lty = c(1, NA, NA, NA),
           pt.cex = c(1, size_values), bg = "white")
  } else if (metric == "bias") {
    legend("topleft", legend = c("Pollster", "PoSSUM", size_labels),
           pch = c(17, 16, 16, 16, 16), pt.cex = c(1, 1, size_values), bg = "white")
  } else {
    legend("topleft", legend = c("Pollster", "PoSSUM", "Avg.", size_labels),
           pch = c(17, 16, NA, 16, 16, 16), lty = c(NA, NA, 1, NA, NA, NA),
           pt.cex = c(1, 1, 1, size_values), bg = "white")
  }
}

# Compare the same evaluation measures with the 538 pollster ratings.
# These panels include one-area pollsters where the metric is defined.
# OVL is the overlap itself; the other five panels show PoSSUM minus pollster.
write_rating_comparison <- function(summary_evals, output_dir) {
  pdf(file.path(output_dir, "delta.pdf"), width = 7.5, height = 5)
  on.exit(dev.off())
  par(mfrow = c(2, 3), mar = c(3, 4, 2, 1), oma = c(0, 0, 3, 0),
      mgp = c(2, 0.6, 0))
  metrics <- c("bias", "rmse", "rho", "accuracy", "coverage90", "OVL")
  titles <- expression("Pro-R Bias", "RMSE", "Spearman Rank Correlation",
                       "Call Accuracy", "90% Coverage", "Density Overlap")
  for (j in seq_along(metrics)) {
    metric <- metrics[j]
    y <- if (metric == "OVL") summary_evals$OVL_mean else
      summary_evals[[paste0(metric, ".PoSSUM")]] - summary_evals[[metric]]
    x <- summary_evals$numeric_grade
    keep <- !(is.na(y) | is.na(x))
    N <- summary_evals$N[keep]
    # The point-size denominator is calculated before the missing-value filter.
    point_size <- (3 * summary_evals$N / max(summary_evals$N))[keep]
    y <- y[keep]
    x <- x[keep]
    plot(x, y, ylim = range(y), xlim = c(min(x), max(x)),
         ylab = if (metric == "OVL") "OVL" else expression(Delta),
         cex = point_size, main = titles[j], xlab = "Pollster Rating")

    # Preserve the default, unweighted GAM and its fitted values +/- 2 SE.
    fit <- mgcv::gam(y ~ s(x))
    pred <- predict(fit, newdata = data.table(x = x), se.fit = TRUE)
    upper <- pred$fit + 2 * pred$se.fit
    lower <- pred$fit - 2 * pred$se.fit
    ix <- order(x)
    polygon(c(x[ix], rev(x[ix])), c(upper[ix], rev(lower[ix])),
            col = adjustcolor("lightcoral", 0.25), border = NA)
    lines(x[ix], pred$fit[ix], col = "red4", lwd = 2)
    abline(h = 0, lty = 1)
    legend(if (metric %in% c("rmse", "OVL")) "bottomleft" else "topleft",
           legend = paste0("N = ", c(min(N), round(quantile(N, 0.9)), max(N))),
           pch = 16,
           pt.cex = 3 * c(min(N / max(N)), round(quantile(N / max(N), 0.9), 1),
                          max(N / max(N))))
  }
  mtext(expression("PoSSUM " * Delta * " v. Pollsters, by 538 Rating"), outer = TRUE)
}

# 8. Write exactly the seven paper figures ----------------------------------
output_dir <- "output/evaluation"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
for (metric in c("bias", "rmse", "rho", "accuracy", "coverage90", "OVL")) {
  write_pollster_comparison(summary_evals, metric, output_dir)
}
write_rating_comparison(summary_evals, output_dir)
message("Saved the six pollster-comparison PDFs and delta.pdf to ", output_dir)
