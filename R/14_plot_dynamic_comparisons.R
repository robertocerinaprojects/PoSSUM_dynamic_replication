# Reproduce the paper's five comparisons of PoSSUM and polls across waves.
# Run from the repository root after the five single-wave High analyses.
# Output: dynamic_exchangeability_{national,gender,ethnicity,age,edu}.pdf
# in output/evaluation/. All margins and intervals are in percentage points.

library(data.table)
library(MCMCpack)

# 1. Read the archived poll inputs ------------------------------------------
# Publish president_polls.csv in the data-v0.1.0 release before sharing this
# workflow. A local copy in data/downloaded/evaluation/ also works.
input_dir <- "data/downloaded/evaluation"
output_dir <- "output/evaluation"
dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
download_reference <- function(url, filename, checksum) {
  path <- file.path(input_dir, filename)
  if (!file.exists(path)) download.file(url, path, mode = "wb")
  if (unname(tools::md5sum(path)) != checksum) {
    stop("Reference file does not match the expected snapshot: ", path)
  }
  path
}
national_file <- download_reference(
  paste0("https://github.com/robertocerinaprojects/PoSSUM_dynamic_replication/",
         "releases/download/data-v0.1.0/president_polls.csv"),
  "president_polls.csv", "fad50140462dad14f57cbc07f368d96e")
crosstabs_file <- download_reference(
  paste0("https://raw.githubusercontent.com/robertocerinaprojects/PoSSUM/",
         "b9005a80c7f6dbbf6e1f48c9fa625dfae24c8b22/",
         "evaluation_data/clean_crosstabs_database.csv"),
  "clean_crosstabs_database.csv", "349cb6e53a33065eaa63e3b29f0fa817")
cross <- fread(crosstabs_file)

# 2. Collect the five saved High-wave predictions ---------------------------
# Use the original reverse-wave loading order.
run_ids <- paste0("wave", 5:1, "_high")
runs <- rbindlist(lapply(file.path("output/batch", run_ids, "latest_run.csv"), fread))
stopifnot(identical(runs$run_id, run_ids))
group_variables <- c(national = "date", gender = "gender", ethnicity = "ethnicity",
                     age = "age_bins", edu = "college_grad")
predictions <- lapply(group_variables, function(x) vector("list", nrow(runs)))
for (i in seq_len(nrow(runs))) {
  saved <- new.env()
  load(file.path(runs$run_dir[i], "poststratification.RData"), envir = saved)
  dates <- as.Date(strsplit(runs$survey_period[i], "_", fixed = TRUE)[[1]])
  for (kind in names(group_variables)) {
    variable <- group_variables[[kind]]
    x <- saved$crosstab.pred_list[[variable]]
    predictions[[kind]][[i]] <- data.table(
      group = if (kind == "national") "National" else as.character(x[[variable]]),
      start.date = dates[1], end.date = dates[2], sim_id = x$sim_id,
      RD_margin = 100 * x$R - 100 * x$D)
  }
  rm(saved, x)
}
predictions <- lapply(predictions, rbindlist)
intervals <- unique(predictions$national[, .(start.date, end.date)])
n_sims <- max(predictions$national$sim_id)

# The dashed 2020 baselines use N_star.1, as in the paper, among 2020 voters.
# Read only the columns needed here into the working object.
frame_columns <- c("vote2020", "N_star.1", unname(group_variables[-1]))
frame <- as.data.table(readRDS("data/downloaded/SF_extended_simple.rds"))[
  vote2020 != "stay home", ..frame_columns]
baselines <- lapply(names(group_variables), function(kind) {
  group <- if (kind == "national") rep("National", nrow(frame)) else
    as.character(frame[[group_variables[[kind]]]])
  x <- data.table(group, vote = frame$vote2020, weight = frame$N_star.1)
  x[, .(RD_margin = 100 * (sum(weight[vote == "R"]) / sum(weight) -
                           sum(weight[vote == "D"]) / sum(weight))), by = group]
})
names(baselines) <- names(group_variables)
rm(frame)

# 3. Match polls and simulate their uncertainty -----------------------------
# A poll belongs to every wave whose fieldwork dates overlap its own dates.
# Shared endpoints count as overlap. No latest-poll filter is applied here.
match_intervals <- function(polls) {
  polls <- copy(polls)
  waves <- copy(intervals)
  polls[, `:=`(start_date = as.Date(start_date), end_date = as.Date(end_date))]
  setkey(polls, start_date, end_date)
  setkey(waves, start.date, end.date)
  out <- foverlaps(polls, waves, type = "any", nomatch = 0L)
  setnames(out, c("start.date", "end.date", "start_date", "end_date"),
           c("PoSSUM_start.date", "PoSSUM_end.date", "poll_start_date", "poll_end_date"))
  out
}
poll_counts <- function(polls, ids, response, categories) {
  wide <- dcast(polls, as.formula(paste(paste(ids, collapse = " + "), "~", response)),
                value.var = "N", fun.aggregate = sum, fill = 0)
  for (category in setdiff(categories, names(wide))) wide[, (category) := 0]
  wide
}
simulate_polls <- function(wide, categories) {
  # Counts are Dirichlet parameters, without added pseudocounts. Preserve the
  # original omission of zero-count categories and their reinsertion as NA.
  # Draw all reported categories; retain D/R for these R-D figures.
  draws <- vector("list", nrow(wide))
  for (i in seq_len(nrow(wide))) {
    alpha <- as.numeric(wide[i, ..categories])
    keep <- alpha != 0
    x <- MCMCpack::rdirichlet(n_sims, alpha[keep])
    colnames(x) <- categories[keep]
    for (party in setdiff(c("D", "R"), colnames(x))) {
      x <- cbind(x, NA_real_)
      colnames(x)[ncol(x)] <- party
    }
    draws[[i]] <- cbind(wide[rep(i, n_sims), setdiff(names(wide), categories), with = FALSE],
                        data.table(sim_id = seq_len(n_sims), D = 100*x[, "D"], R = 100*x[, "R"]))
  }
  rbindlist(draws)
}
summarise_margin <- function(x, by) {
  x[, .(quantile = c("5%", "50%", "95%"),
        RD_margin = as.numeric(quantile(RD_margin, c(.05, .5, .95), na.rm = TRUE))),
    by = by]
}
compare_dynamics <- function(wide, prediction, baseline, categories, complete_only = FALSE) {
  draws <- simulate_polls(wide, categories)
  draws[, RD_margin := R - D]
  ids <- setdiff(names(wide), categories)
  polls <- summarise_margin(draws, ids)
  # Equal weight per poll entry, averaging each party before subtracting D
  # from R. Keep the original denominator; do not renormalise to R + D.
  average <- draws[, lapply(.SD, mean, na.rm = TRUE),
                    by = .(group, PoSSUM_end.date, sim_id), .SDcols = c("D", "R")]
  average[, RD_margin := R - D]
  paired <- merge(average[, .(group, PoSSUM_end.date, sim_id, polls = RD_margin)],
                  prediction[, .(group, end.date, sim_id, possum = RD_margin)],
                  by.x = c("PoSSUM_end.date", "sim_id", "group"),
                  by.y = c("end.date", "sim_id", "group"), all = TRUE)
  paired <- merge(paired, baseline, by = "group", all = TRUE)
  # Ethnicity, age and education use complete matched rows, as originally.
  if (complete_only) paired <- paired[complete.cases(paired)]
  metrics <- paired[, .(bias = mean(possum - polls),
                       corr = cor(possum, polls, method = "spearman")), by = .(sim_id, group)]
  metrics <- metrics[, lapply(.SD, quantile, probs = c(.05, .5, .95)),
                      by = group, .SDcols = c("bias", "corr")]
  metrics[, quantile := rep(c("5%", "50%", "95%"), .N / 3)]
  list(polls = polls, possum = summarise_margin(prediction, c("group", "start.date", "end.date")),
       average = summarise_margin(average, c("group", "PoSSUM_end.date")), metrics = metrics)
}

# Use the same demographic pollster ordering and symbols as the original.
symbols_input <- copy(cross[grepl("age|race|gender|edu|vote2020", condition) &
                              !grepl("PoSSUM", pollster)])
setnames(symbols_input, c("start.date", "end.date"), c("start_date", "end_date"))
symbols_input <- match_intervals(symbols_input)
pollster_symbols <- data.table(pollster = unique(as.character(symbols_input$pollster)))
pollster_symbols[, pch := head(c(0:17, seq.int(21L, length.out = .N)), .N)]
rm(symbols_input)

# 4. Select and align national and demographic polls ------------------------
parties <- c("D", "R", "G", "W", "K", "L")
responses <- c(parties, "stay home", "OTHER")
national <- fread(national_file)
national <- national[election_date == "11/5/24" & stage == "general" & is.na(tracking) &
  (candidate_name %in% c("Kamala Harris", "Donald Trump", "Robert F. Kennedy",
                         "Jill Stein", "Cornel West", "Chase Oliver") | party == "LIB") &
  state == "" & !is.na(sample_size)]
national[, n_cand := .N, by = .(poll_id, pollster, sponsors, question_id, start_date,
                                end_date, population, sample_size)]
national <- national[n_cand > 1]
national[, `:=`(start_date = as.Date(start_date, "%m/%d/%y"),
                end_date = as.Date(end_date, "%m/%d/%y"), N = round(sample_size * (pct / 100)))]
candidate_map <- c("Kamala Harris" = "D", "Donald Trump" = "R", "Jill Stein" = "G",
                   "Cornel West" = "W", "Chase Oliver" = "L", "Robert F. Kennedy" = "K")
national[, response := unname(candidate_map[candidate_name])]
national <- national[!is.na(response)]

gender <- copy(cross[variable == "vote2024" & condition %in% c("gender_male", "gender_female") &
                      !grepl("PoSSUM", pollster)])
gender[, group := fifelse(condition == "gender_male", "M", "F")]
ethnicity <- copy(cross[variable == "vote2024" & grepl("race_", condition) & !grepl("PoSSUM", pollster)])
ethnicity[, group := fcase(condition == "race_white", "White", condition == "race_black", "Black",
  condition == "race_hispanic", "Hispanic", condition == "race_asian", "Asian",
  condition == "race_native", "Native", default = "Mixed-race")]
edu <- copy(cross[variable == "vote2024" & grepl("edu", condition) & !grepl("PoSSUM", pollster)])
edu[, group := fifelse(condition == "education_college", "Has a College Degree",
                       "Does not have a College Degree")]

# Preserve the paper's age alignment: midpoints within five years, with
# open-ended age groups ending at 150. This is not an interval-overlap rule.
age <- copy(cross[variable == "vote2024" & grepl("^age_", condition) & !grepl("PoSSUM", pollster)])
parse_age <- function(condition) {
  if (grepl("^age_over\\.\\d+", condition)) {
    c(as.integer(sub("age_over\\.", "", condition)) + 1L, 150L)
  } else as.integer(strsplit(sub("^age_", "", condition), "\\.")[[1]])[1:2]
}
age[, midpoint := vapply(condition, function(x) mean(parse_age(x)), numeric(1))]
age_groups <- c("1.18-24", "2.25-34", "3.35-44", "4.45-54", "5.55-64", "6.65+")
age_midpoints <- (c(18, 25, 35, 45, 55, 65) + c(24, 34, 44, 54, 64, 150)) / 2
age <- rbindlist(lapply(seq_along(age_groups), function(i) {
  x <- copy(age[abs(midpoint - age_midpoints[i]) <= 5])
  x[, group := age_groups[i]]
  x
}))
poll_inputs <- list(national = national, gender = gender, ethnicity = ethnicity, age = age, edu = edu)
for (kind in names(poll_inputs)[-1]) {
  setnames(poll_inputs[[kind]], c("start.date", "end.date"), c("start_date", "end_date"))
}
rm(national, gender, ethnicity, age, edu, cross)

# 5. Draw the figures in the original style ---------------------------------
# Plot one group. Summary tables keep the original row order within quantiles.
# Poll intervals have the original quarter-day normal jitter; points do not.
plot_dynamic_panel <- function(polls, possum, pollster_symbols, main_title,
                               poll_alpha = 0.35) {
  p_mid <- polls[quantile == "50%"]
  p_low <- polls[quantile == "5%"]
  p_high <- polls[quantile == "95%"]
  pos_mid <- possum[quantile == "50%"]
  pos_low <- possum[quantile == "5%"]
  pos_high <- possum[quantile == "95%"]
  yrange <- range(p_low$RD_margin, p_high$RD_margin,
                  pos_low$RD_margin, pos_high$RD_margin, na.rm = TRUE)
  poll_colour <- adjustcolor("navyblue", alpha.f = poll_alpha)
  poll_fill <- adjustcolor("skyblue", alpha.f = poll_alpha)
  plot(p_mid$poll_end_date, p_mid$RD_margin,
       pch = pollster_symbols$pch[match(p_mid$pollster, pollster_symbols$pollster)],
       col = poll_colour, bg = poll_fill, xlab = "Poll End Date",
       ylab = "R-D Margin", main = main_title, font.main = 1, ylim = yrange)
  interval_date <- p_mid$poll_end_date + rnorm(nrow(p_mid), mean = 0, sd = 0.25)
  segments(interval_date, p_low$RD_margin, interval_date, p_high$RD_margin,
           col = poll_colour, lwd = 0.5)
  abline(h = 0, lty = 2, xpd = FALSE)
  points(pos_mid$end.date, pos_mid$RD_margin, pch = 19, col = "red4", cex = 1.25)
  segments(pos_mid$end.date, pos_low$RD_margin, pos_mid$end.date, pos_high$RD_margin,
           col = "red4", lwd = 2)
  lines(pos_mid$end.date, pos_mid$RD_margin, lwd = 1.5, col = "red4")
}

# Polling averages are offset by one day to separate them from PoSSUM.
add_dynamic_average <- function(average, national = FALSE) {
  mid <- average[quantile == "50%"]
  low <- average[quantile == "5%"]
  high <- average[quantile == "95%"]
  date <- mid$PoSSUM_end.date - 1
  points(date, mid$RD_margin, pch = 16, col = "black", cex = 1)
  if (national) lines(date, mid$RD_margin, col = "black", lwd = 1.5)
  segments(date, low$RD_margin, date, high$RD_margin, col = "black", lwd = 2)
  if (!national) lines(date, mid$RD_margin, lwd = 1.5)
}

# Metrics contain three rows per group: quantile = 5%, 50%, 95%, with bias/corr.
add_dynamic_metrics <- function(metrics, cex = 0.9) {
  bias <- metrics$bias[match(c("5%", "50%", "95%"), metrics$quantile)]
  correlation <- metrics$corr[match(c("5%", "50%", "95%"), metrics$quantile)]
  legend("topleft", legend = c(
    paste0("Bias v. Avg: ", round(bias[2], 1), " (", round(bias[1], 1), ",",
           round(bias[3], 1), ")"),
    paste0("Corr v. Avg: ", round(correlation[2], 2), " (", round(correlation[1], 2),
           ",", round(correlation[3], 2), ")")
  ), bty = "n", cex = cex)
}

# Make exactly one paper figure. Every input table has a common 'group' column.
# polls: poll_end_date, pollster, quantile, RD_margin;
# possum: end.date, quantile, RD_margin;
# average: PoSSUM_end.date, quantile, RD_margin;
# metrics: quantile, bias, corr; baselines: RD_margin (one row per group).
# Dates must already be Date objects; all margins and bias are percentage points.
write_dynamic_comparison <- function(kind, polls, possum, average, metrics,
                                     baselines, pollster_symbols, output_dir) {
  layout <- switch(kind,
    national = list(width = 10, height = 5),
    gender = list(width = 10, height = 5, panels = c(1, 2),
                  groups = c("M", "F"), titles = c("Male", "Female"),
                  outer_title = "GENDER", alpha = 0.35, metric_cex = 0.9),
    ethnicity = list(width = 11.25, height = 3.75, panels = c(1, 3),
                     groups = c("White", "Black", "Hispanic"),
                     titles = c("White", "Black", "Hispanic"),
                     outer_title = "ETHNICITY", alpha = 0.35, metric_cex = 0.9),
    age = list(width = 11.25, height = 7.5, panels = c(2, 3),
               groups = c("1.18-24", "2.25-34", "3.35-44", "4.45-54", "5.55-64", "6.65+"),
               titles = c("1.18-24", "2.25-34", "3.35-44", "4.45-54", "5.55-64", "6.65+"),
               outer_title = "AGE GROUPS", alpha = 0.5, metric_cex = 1.1),
    edu = list(width = 10, height = 5, panels = c(1, 2),
               groups = c("Does not have a College Degree", "Has a College Degree"),
               titles = c("No College Degree", "College Degree"),
               outer_title = "EDUCATION", alpha = 0.5, metric_cex = 1.1),
    stop("Unknown dynamic comparison: ", kind)
  )
  pdf(file.path(output_dir, paste0("dynamic_exchangeability_", kind, ".pdf")),
      width = layout$width, height = layout$height)
  on.exit(dev.off())

  if (kind == "national") {
    par(oma = c(0, 0, 0, 15), xpd = NA)
    plot_dynamic_panel(polls, possum, pollster_symbols, "National Topline")
    # Keep the original additional PoSSUM line and order of plotting layers.
    pos_mid <- possum[quantile == "50%"]
    lines(pos_mid$end.date, pos_mid$RD_margin, col = "red4", lwd = 1.5)
    abline(h = baselines$RD_margin, col = "lightcoral", lwd = 2, lty = 2, xpd = FALSE)
    # The paper used this fixed 2024 reference, separate from the DDHQ snapshot.
    abline(h = 49.8 - 48.3, col = "dodgerblue", lwd = 2, lty = 2, xpd = FALSE)
    add_dynamic_average(average, national = TRUE)
    add_dynamic_metrics(metrics)
    legend(x = max(polls$poll_end_date) + 10, y = max(polls$RD_margin) + 5,
           legend = c("Result", "Past Vote", "PoSSUM", "Polling Avg",
                      pollster_symbols$pollster, "Other"),
           col = c("dodgerblue", "lightcoral", "red4", "black",
                   rep("navyblue", length(pollster_symbols$pch)), "navyblue"),
           lty = c(2, 2, 1, 1, rep(NA, length(pollster_symbols$pch)), 1),
           lwd = c(2, 2, 1.5, 1.5, rep(NA, length(pollster_symbols$pch)), 0.5),
           pch = c(NA, NA, 19, 16, pollster_symbols$pch, NA),
           pt.bg = c(NA, NA, "skyblue", "skyblue",
                     rep("skyblue", length(pollster_symbols$pch)), NA),
           bty = "n", cex = 0.8)
  } else {
    par(oma = c(0, 0, 2, 0), mfrow = layout$panels, xpd = NA)
    for (i in seq_along(layout$groups)) {
      selected_group <- layout$groups[i]
      plot_dynamic_panel(polls[group == selected_group], possum[group == selected_group],
                          pollster_symbols, layout$titles[i], poll_alpha = layout$alpha)
      add_dynamic_average(average[group == selected_group])
      add_dynamic_metrics(metrics[group == selected_group], cex = layout$metric_cex)
      past_margin <- baselines[group == selected_group, RD_margin]
      abline(h = past_margin, col = "lightcoral", lwd = 2, lty = 2, xpd = FALSE)
    }
    title(layout$outer_title, outer = TRUE, font = 2)
  }
}

# 6. Calculate and plot the five comparisons --------------------------------
# Keep the original seeds and order. Plotting jitters poll intervals, so each
# figure is drawn before the next comparison's poll simulations are generated.
set.seed(12345)
for (kind in names(poll_inputs)) {
  polls <- match_intervals(poll_inputs[[kind]])
  categories <- if (kind == "national") parties else responses
  ids <- c("poll_start_date", "poll_end_date", "pollster")
  wave_ids <- c("PoSSUM_start.date", "PoSSUM_end.date")
  ids <- switch(kind,
    national = c("poll_id", ids, wave_ids),
    gender = c(ids, wave_ids, "group"),
    ethnicity = c(ids, "group", wave_ids),
    age = c(ids, "group", wave_ids),
    edu = c(ids, wave_ids, "group"))
  wide <- poll_counts(polls, ids, "response", categories)
  if (kind == "national") wide[, group := "National"]
  if (kind %in% c("gender", "ethnicity")) set.seed(123)
  result <- compare_dynamics(wide, predictions[[kind]], baselines[[kind]], categories,
                             complete_only = kind %in% c("ethnicity", "age", "edu"))
  write_dynamic_comparison(kind, result$polls, result$possum, result$average,
                           result$metrics, baselines[[kind]], pollster_symbols, output_dir)
  message("Saved dynamic_exchangeability_", kind, ".pdf")
}
message("Saved the five dynamic-comparison PDFs to ", output_dir)
