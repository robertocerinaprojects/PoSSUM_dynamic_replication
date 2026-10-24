# Run from the repository root with the same run_dir. All output is PDF.
# Plotting choices are shared across survey waves; calculations use each run's draws.
# Figure 8 keeps the published 30 October 2024 538 forecast as its fixed benchmark.

# 1. Packages and display settings -----------------------------------------
library(data.table)
library(ggplot2)
library(ggrepel)
library(mgcv)

party_colours <- c(R = "orangered", D = "dodgerblue", G = "darkgreen",
                   L = "orange", K = "purple", W = "darkred")
ec_binwidth <- 5       # Electoral votes per histogram bin.
pv_binwidth <- 0.1     # Percentage points per popular-vote bin, for all candidates.
label_seed <- 20241029
plot_theme <- theme_classic(base_size = 12) + theme(
  plot.title = element_text(face = "bold"),
  plot.caption = element_text(hjust = 0), legend.title = element_blank()
)

# 2. Load and pair the saved draws -----------------------------------------
load(file.path(run_dir, "poststratification.RData"))
ec_results <- readRDS(file.path(run_dir, "electoral_college", "electoral_college.RDS"))
stopifnot(dep.var == "vote2024", ec_results$dep.var == dep.var,
          ec_results$survey_period == survey_period, ec_results$speculation == speculation)
figure_dir <- file.path(run_dir, "figures")
dir.create(figure_dir, showWarnings = FALSE)
fieldwork_dates <- as.Date(strsplit(survey_period, "_", fixed = TRUE)[[1]])
fieldwork_label <- paste(format(fieldwork_dates, "%d %b %Y"), collapse = " to ")

# Pair popular and electoral outcomes by sim_id, never by row position.
candidates <- names(party_colours)
national <- crosstab.pred_list$date[, c("sim_id", candidates, "RD_margin"), with = FALSE]
ec_totals <- dcast(ec_results$electoral_vote_draws,
                   sim_id ~ candidate, value.var = "electoral_votes")
setnames(ec_totals, candidates, paste0("EC_", candidates))
ec_winners <- ec_results$election_draws[, .(sim_id, EC_winner = winner)]
stopifnot(!anyDuplicated(national$sim_id), !anyDuplicated(ec_totals$sim_id),
          !anyDuplicated(ec_winners$sim_id),
          setequal(national$sim_id, frame_draws$sim_id),
          setequal(national$sim_id, ec_totals$sim_id),
          setequal(national$sim_id, ec_winners$sim_id))
joint_draws <- merge(national, ec_totals, by = "sim_id")
joint_draws <- merge(joint_draws, ec_winners, by = "sim_id")
vote_shares <- as.matrix(joint_draws[, ..candidates])
joint_draws[, PV_share := apply(vote_shares, 1, max)]
joint_draws[, PV_winner := candidates[max.col(vote_shares, ties.method = "first")]]
joint_draws[rowSums(vote_shares == PV_share) > 1L, PV_winner := "Tie"]
joint_draws[, `:=`(popular_margin = 100 * RD_margin, electoral_margin = EC_R - EC_D)]

# Maine/Nebraska statewide estimates and their districts are separate units.
area_draws <- rbindlist(list(
  crosstab.pred_list$state_electoral.college[
    !state_electoral.college %in% c("Maine", "Nebraska"),
    .(sim_id, area = state_electoral.college, RD_margin)],
  crosstab.pred_list$state_simple[state_simple %in% c("Maine", "Nebraska"),
    .(sim_id, area = state_simple, RD_margin)]
))
stopifnot(!anyDuplicated(area_draws, by = c("sim_id", "area")),
          all(area_draws[, .N, by = sim_id]$N == 56L),
          setequal(area_draws$sim_id, joint_draws$sim_id),
          setequal(area_draws$area, ec_results$allocation$area),
          all(is.finite(area_draws$RD_margin)))
# Summarise the R-D difference within each draw, not the difference of medians.
area_summary <- area_draws[, .(mid = 100 * median(RD_margin),
  lo = 100 * quantile(RD_margin, 0.05), hi = 100 * quantile(RD_margin, 0.95)), by = area]
win_probabilities <- dcast(ec_results$unit_win_probabilities,
                           area ~ candidate, value.var = "win_probability")

# 3. Prepare the original cartogram ----------------------------------------
# Daniel Donner / The Downballot, version 3.0, CC BY 4.0: https://the-db.co/maps
# Download once; this cache folder is already ignored by Git.
reference_dir <- "data/downloaded/electoral_college_plots"
dir.create(reference_dir, recursive = TRUE, showWarnings = FALSE)
map_files <- file.path(reference_dir, "ElecCollv30",
                       paste0("ElecCollv30.", c("shp", "shx", "dbf", "prj")))
if (!all(file.exists(map_files))) {
  map_zip <- file.path(reference_dir, "ElecCollv30.zip")
  download.file(
    "https://drive.google.com/uc?export=download&id=1LaAPXDaFhoIvemxsNyYjsAN6ldyIV8yT",
    map_zip, mode = "wb")
  if (unname(tools::md5sum(map_zip)) != "4ddcbb76a455ea84302836f61b88a61b") {
    stop("The downloaded cartogram differs from the expected version 3.0 archive.")
  }
  unzip(map_zip, exdir = reference_dir)
}
map_data <- sf::st_make_valid(sf::st_read(map_files[1], quiet = TRUE))
map_data$area <- as.character(map_data$STATENAME)
area_names <- c("Washington DC" = "District of Columbia",
  "Maine District 01" = "Maine 1st", "Maine District 02" = "Maine 2nd",
  "Nebraska District 01" = "Nebraska 1st", "Nebraska District 02" = "Nebraska 2nd",
  "Nebraska District 03" = "Nebraska 3rd")
rename <- map_data$area %in% names(area_names)
map_data$area[rename] <- unname(area_names[map_data$area[rename]])
district_labels <- c("Maine 1st" = "ME-01", "Maine 2nd" = "ME-02",
  "Nebraska 1st" = "NE-01", "Nebraska 2nd" = "NE-02", "Nebraska 3rd" = "NE-03")
map_data$label <- as.character(map_data$STATEAB)
district <- map_data$area %in% names(district_labels)
map_data$label[district] <- district_labels[map_data$area[district]]
map_data$margin <- area_summary$mid[match(map_data$area, area_summary$area)]
map_data$probability <- (win_probabilities$R - win_probabilities$D)[
  match(map_data$area, win_probabilities$area)]
stopifnot(all(is.finite(map_data$margin)), all(is.finite(map_data$probability)))

# Label each area once, even when it has several polygons (Alaska).
# Labels start at the polygon centroids and separate automatically.
largest_first <- order(as.numeric(sf::st_area(map_data)), decreasing = TRUE)
label_rows <- largest_first[!duplicated(map_data$area[largest_first])]
map_labels <- map_data[label_rows, ]
sf::st_geometry(map_labels) <- sf::st_centroid(sf::st_geometry(map_labels))
map_base <- ggplot(map_data) + geom_sf(colour = NA, alpha = 0.85) +
  coord_sf(datum = NA) + theme_void(base_size = 13) + theme(
    legend.position = "bottom", legend.title = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 14), plot.caption = element_text(size = 8),
    plot.margin = margin(12, 16, 12, 16)) +
  labs(subtitle = paste("Fieldwork", fieldwork_label),
       caption = "Cartogram: Daniel Donner / The Downballot | CC BY 4.0 | the-db.co/maps")

# 4. Figure 2: median Republican minus Democratic vote share ----------------
vote_share_map <- map_base + aes(fill = margin) +
  geom_text_repel(data = map_labels,
    aes(geometry = geometry, label = paste0(label, "\n", round(margin, 1))),
    stat = "sf_coordinates", fun.geometry = identity, seed = label_seed,
    max.overlaps = Inf, max.time = Inf, max.iter = 10000, point.padding = NA,
    size = 3.8, fontface = "bold", box.padding = 0.3) +
  scale_fill_gradient2(low = "dodgerblue", mid = "grey80", high = "orangered",
    midpoint = 0, limits = c(-10, 10), oob = scales::squish,
    name = "R - D (percentage points)",
    guide = guide_colourbar(barwidth = grid::unit(14, "cm"), title.position = "top")) +
  labs(title = "Republican - Democrat vote share margin")
ggsave(file.path(figure_dir, "vote_share_map.pdf"), vote_share_map,
       width = 12.5, height = 14, bg = "white")

# 5. Figure 3: difference in state and district win probabilities -----------
# Probabilities come from script 09, with ALL draws in each unit's denominator.
win_probability_map <- map_base + aes(fill = probability) +
  geom_text_repel(data = map_labels,
    aes(geometry = geometry, label = paste0(label, "\n", round(probability, 2))),
    stat = "sf_coordinates", fun.geometry = identity, seed = label_seed,
    max.overlaps = Inf, max.time = Inf, max.iter = 10000, point.padding = NA,
    size = 3.8, fontface = "bold", box.padding = 0.3) +
  scale_fill_gradient2(low = "dodgerblue", mid = "grey80", high = "orangered",
    midpoint = 0, limits = c(-1, 1), name = "Pr(R wins) - Pr(D wins)",
    guide = guide_colourbar(barwidth = grid::unit(14, "cm"), title.position = "top")) +
  labs(title = "Republican - Democrat probability of winning")
ggsave(file.path(figure_dir, "win_probability_map.pdf"), win_probability_map,
       width = 12.5, height = 14, bg = "white")

# 6. Figure 4: electoral-vote distributions ---------------------------------
ec_histogram_data <- ec_results$electoral_vote_draws[candidate %in% c("R", "D")]
ec_medians <- ec_histogram_data[, .(median = median(electoral_votes)), by = candidate]
ec_legend <- setNames(paste0(ec_medians$candidate, ": median ", ec_medians$median), ec_medians$candidate)
ec_scenarios <- joint_draws[, c(
  "R wins EC" = mean(EC_winner == "R"), "D wins EC" = mean(EC_winner == "D"),
  "D popular / R EC" = mean(PV_winner == "D" & EC_winner == "R"),
  "R popular / D EC" = mean(PV_winner == "R" & EC_winner == "D"),
  "269-269 tie" = mean(EC_R == 269 & EC_D == 269),
  "Other no majority" = mean(EC_winner == "No majority" & !(EC_R == 269 & EC_D == 269)))]
ec_labels <- paste(names(ec_scenarios), sprintf("%.2f", ec_scenarios), sep = ": ")
ec_caption <- paste(paste(ec_labels[1:3], collapse = " | "),
                    paste(ec_labels[4:6], collapse = " | "), sep = "\n")
ec_histogram <- ggplot(ec_histogram_data, aes(electoral_votes, fill = candidate)) +
  geom_histogram(binwidth = ec_binwidth, boundary = -0.5, position = "identity", alpha = 0.65) +
  geom_vline(xintercept = 270, linewidth = 0.5) +
  geom_vline(data = ec_medians, aes(xintercept = median, colour = candidate), linetype = "dotted") +
  scale_fill_manual(values = party_colours, breaks = c("R", "D"), labels = ec_legend) +
  scale_colour_manual(values = party_colours, guide = "none") +
  scale_x_continuous(breaks = sort(unique(c(pretty(range(ec_histogram_data$electoral_votes)), 270)))) +
  labs(title = "Electoral College votes", subtitle = fieldwork_label,
       x = "Electoral votes", y = "Posterior draws", caption = ec_caption) + plot_theme
ggsave(file.path(figure_dir, "EC_votes_histogram.pdf"), ec_histogram,
       width = 12.5, height = 4.8, bg = "white")

# 7. Figure 5: popular-vote distributions -----------------------------------
# Shares are among voters. Stay-home is not a candidate.
pv_histogram_data <- melt(joint_draws, id.vars = "sim_id", measure.vars = candidates,
                          variable.name = "candidate", value.name = "share")
pv_histogram_data[, share := 100 * share]
pv_medians <- pv_histogram_data[, .(median = median(share)), by = candidate]
pv_legend <- setNames(sprintf("%s: median %.1f%%", pv_medians$candidate, pv_medians$median), pv_medians$candidate)
pv_scenarios <- joint_draws[, c(
  "R wins popular vote" = mean(PV_winner == "R"), "D wins popular vote" = mean(PV_winner == "D"),
  "R majority" = mean(PV_winner == "R" & PV_share >= 0.5),
  "D majority" = mean(PV_winner == "D" & PV_share >= 0.5))]
pv_caption <- paste(paste(names(pv_scenarios), sprintf("%.2f", pv_scenarios), sep = ": "), collapse = " | ")
pv_histogram <- ggplot(pv_histogram_data, aes(share, fill = candidate)) +
  geom_histogram(binwidth = pv_binwidth, boundary = 0, position = "identity", alpha = 0.65) +
  geom_vline(data = pv_medians, aes(xintercept = median, colour = candidate), linetype = "dotted") +
  scale_fill_manual(values = party_colours, breaks = candidates, labels = pv_legend) +
  scale_colour_manual(values = party_colours, guide = "none") +
  labs(title = "Popular vote", subtitle = fieldwork_label, x = "Vote share (%)",
       y = "Posterior draws", caption = pv_caption) + plot_theme
ggsave(file.path(figure_dir, "PV_histogram.pdf"), pv_histogram,
       width = 12.5, height = 4.8, bg = "white")

# 8. Figure 6: joint popular and electoral margins --------------------------
# Quadrants describe R/D leads. Pale colours mark split popular/EC outcomes.
joint_draws[, point_fill := fcase(
  popular_margin > 0 & electoral_margin > 0, "orangered",
  popular_margin < 0 & electoral_margin < 0, "dodgerblue",
  popular_margin < 0 & electoral_margin > 0, alpha("orangered", 0.2),
  popular_margin > 0 & electoral_margin < 0, alpha("dodgerblue", 0.2), default = "black")]
margin_limit <- max(15, ceiling(max(abs(joint_draws$popular_margin))))
quadrants <- data.table(
  x = c(-1, 1, -1, 1) * margin_limit * 0.94, y = c(500, 500, -450, -450),
  hjust = c(0, 1, 0, 1), ec = c("R", "R", "D", "D"), pv = c("D", "R", "D", "R"),
  probability = joint_draws[, c(mean(electoral_margin > 0 & popular_margin < 0),
    mean(electoral_margin > 0 & popular_margin > 0),
    mean(electoral_margin < 0 & popular_margin < 0),
    mean(electoral_margin < 0 & popular_margin > 0))])
margin_plot <- ggplot(joint_draws, aes(popular_margin, electoral_margin)) +
  geom_hline(yintercept = 0, colour = "grey50") + geom_vline(xintercept = 0, colour = "grey50") +
  geom_point(aes(fill = point_fill), shape = 21, size = 1.8) + scale_fill_identity() +
  geom_text(data = quadrants, aes(x, y, hjust = hjust, label = paste(ec, "leads Electoral College"), colour = ec),
            inherit.aes = FALSE, fontface = "bold", size = 3.6) +
  geom_text(data = quadrants, aes(x, y - 35, hjust = hjust, label = paste(pv, "leads popular vote"), colour = pv),
            inherit.aes = FALSE, size = 3.3) +
  geom_text(data = quadrants, aes(x, y - 70, hjust = hjust, label = sprintf("Probability = %.2f", probability)),
            inherit.aes = FALSE, size = 3.3) +
  scale_colour_manual(values = party_colours, guide = "none") +
  coord_cartesian(xlim = c(-margin_limit, margin_limit), ylim = c(-538, 538)) +
  labs(title = "Popular vote and Electoral College", subtitle = fieldwork_label,
       x = "R - D popular margin (percentage points)", y = "R - D electoral votes") + plot_theme
ggsave(file.path(figure_dir, "PV_v_EC.pdf"), margin_plot,
       width = 8, height = 8, bg = "white")

# 9. Figure 7: Republican Electoral College advantage -----------------------
# Preserve the original logistic GAM: response is EC_R > EC_D, with ties coded 0.
# The threshold is the first OBSERVED popular margin with fitted probability > 0.5.
joint_draws[, R_leads_EC := as.integer(electoral_margin > 0)]
gam_curve <- data.table(popular_margin = numeric(), probability = numeric())
advantage_fit <- NULL
threshold <- NA_real_
if (uniqueN(joint_draws$R_leads_EC) > 1L && uniqueN(joint_draws$popular_margin) >= 10L) {
  advantage_fit <- mgcv::gam(R_leads_EC ~ s(popular_margin),
                             family = binomial(link = "logit"), data = joint_draws)
  gam_curve <- joint_draws[, .(popular_margin)]
  gam_curve[, probability := predict(advantage_fit, newdata = joint_draws, type = "response")]
  setorder(gam_curve, popular_margin)
  threshold <- gam_curve[probability > 0.5, popular_margin][1]
}
advantage_caption <- if (any(joint_draws$electoral_margin == 0)) {
  "Black points at 0.5 mark tied R-D electoral totals."
} else NULL
advantage_plot <- ggplot(joint_draws, aes(popular_margin, R_leads_EC)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(data = joint_draws[electoral_margin != 0], aes(fill = point_fill), shape = 21,
    position = position_jitter(width = 0, height = 0.05, seed = label_seed), size = 1.8) +
  geom_point(data = joint_draws[electoral_margin == 0], aes(y = 0.5), size = 2) +
  geom_line(data = gam_curve, aes(y = probability), linewidth = 0.8) + scale_fill_identity() +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("D leads EC", "Even chance", "R leads EC")) +
  coord_cartesian(ylim = c(-0.1, 1.1)) +
  labs(title = "Republican Electoral College advantage", subtitle = fieldwork_label,
       x = "R - D popular margin (percentage points)", y = "Pr(R leads D in electoral votes)",
       caption = advantage_caption) + plot_theme
if (is.finite(threshold)) {
  advantage_plot <- advantage_plot +
    annotate("segment", x = threshold, xend = threshold, y = 0, yend = 0.5, linetype = "dotted") +
    annotate("label", x = threshold, y = 0.57, label = sprintf("Threshold: %.1f pp", threshold), size = 3.5)
} else {
  advantage_plot <- advantage_plot + labs(caption = paste(c(advantage_caption,
    "The draws do not identify a 50% crossing."), collapse = "\n"))
}
ggsave(file.path(figure_dir, "R_EC_advantage.pdf"), advantage_plot,
       width = 8, height = 8, bg = "white")

# 10. Figure 8: fixed 538 benchmarks ----------------------------------------

# The published figure uses the 30 October forecast, although fieldwork ended
# on 26 October.
# Below are the source median vote shares, rearranged into one row per unit.
# Values are fractions; "National" denotes the source's national popular vote.
# Source timestamp: 2024-10-30T22:00:07Z; parties REP/DEM; metric median.
# Source ZIP (daily_summary.csv):
# https://web.archive.org/web/20241120024903id_/https://projects.fivethirtyeight.com/2024-general-data/538_2024_election_forecast_data.zip
# Keeping this small fixed table here avoids a separate data upload or download.
forecast <- fread(text = "state_abb,REP,DEM
National,0.4896,0.5104
AK,0.5498,0.4502
AL,0.6391,0.3609
AR,0.6429,0.3571
AZ,0.5102,0.4898
CA,0.374,0.626
CO,0.44,0.56
CT,0.4127,0.5873
DC,0.0752,0.9248
DE,0.4186,0.5814
FL,0.5307,0.4693
GA,0.5088,0.4912
HI,0.3636,0.6364
IA,0.5476,0.4524
ID,0.6664,0.3336
IL,0.4264,0.5736
IN,0.5865,0.4135
KS,0.582,0.418
KY,0.6399,0.3601
LA,0.604,0.396
MA,0.3598,0.6402
MD,0.3561,0.6439
ME,0.4586,0.5414
ME-01,0.3883,0.6117
ME-02,0.5349,0.4651
MI,0.4946,0.5054
MN,0.47,0.53
MO,0.5706,0.4294
MS,0.5937,0.4063
MT,0.5897,0.4103
NC,0.5066,0.4934
ND,0.6676,0.3324
NE,0.5876,0.4124
NE-01,0.5842,0.4158
NE-02,0.4658,0.5342
NE-03,0.7735,0.2265
NH,0.4724,0.5276
NJ,0.4337,0.5663
NM,0.4616,0.5384
NV,0.4993,0.5007
NY,0.4137,0.5863
OH,0.5415,0.4585
OK,0.6685,0.3315
OR,0.4349,0.5651
PA,0.502,0.498
RI,0.4164,0.5836
SC,0.5651,0.4349
SD,0.639,0.361
TN,0.6191,0.3809
TX,0.536,0.464
UT,0.6118,0.3882
VA,0.4662,0.5338
VT,0.3324,0.6676
WA,0.4092,0.5908
WI,0.496,0.504
WV,0.7022,0.2978
WY,0.7279,0.2721")

national_margin_2024 <- forecast[state_abb == "National", 100 * (REP - DEM)]
national_margin_2020 <- 46.8 - 51.3  # Original national baseline, in points.
uniform_swing <- national_margin_2024 - national_margin_2020  # +2.42 points.

# Read the ORIGINAL, unscaled 2020 vote percentages in a separate environment.
# This avoids overwriting any scaled predictors in your interactive session.
baseline <- new.env()
load("data/raw/area_level.RData", envir = baseline)
benchmark <- as.data.table(baseline$predictor_area_level)[, .(
  area = state_electoral.college,
  label = state_abbreviation,
  uniform_swing = pct.2020.votes.R - pct.2020.votes.D + uniform_swing
)]
district_labels <- c("Maine 1st" = "ME-01", "Maine 2nd" = "ME-02",
                     "Nebraska 1st" = "NE-01", "Nebraska 2nd" = "NE-02",
                     "Nebraska 3rd" = "NE-03")
benchmark[area %in% names(district_labels), label := district_labels[area]]
state_forecast <- forecast[state_abb != "National"]
benchmark[, forecast := 100 * (state_forecast$REP - state_forecast$DEM)[
  match(label, state_forecast$state_abb)]]


# Figure 8: compare the 54 electoral areas with the published 538 benchmarks.
# benchmark contains the fixed 30 October 2024 forecast and uniform swing.
# All margins and the 90% posterior intervals are in percentage points.
benchmark_plot <- merge(benchmark, as.data.table(area_summary)[, .(area, mid, lo, hi)],
                        by = "area", all.x = TRUE, sort = FALSE)
stopifnot(nrow(benchmark_plot) == 54L, !anyDuplicated(benchmark_plot$area),
          all(is.finite(as.matrix(benchmark_plot[, .(mid, lo, hi, uniform_swing, forecast)]))))
benchmark_plot <- melt(benchmark_plot,
  id.vars = c("area", "label", "mid", "lo", "hi"),
  measure.vars = c("uniform_swing", "forecast"),
  variable.name = "method", value.name = "benchmark_margin")
benchmark_plot[, method := factor(method, levels = c("uniform_swing", "forecast"),
  labels = c("Uniform swing (538 popular vote)", "538 state forecast"))]
benchmark_plot[, disagree := (mid > 0) != (benchmark_margin > 0)]

# Keep the original GAM defaults; ggplot's automatic smoother uses other defaults.
benchmark_curves <- benchmark_plot[, {
  curve <- mgcv::gam(benchmark_margin ~ s(mid), data = .SD)
  .(mid = sort(mid), benchmark_margin = as.numeric(predict(curve,
      newdata = data.frame(mid = sort(mid)))))
}, by = method]

# Bias is PoSSUM minus benchmark; coverage uses the original strict inequalities.
benchmark_metrics <- benchmark_plot[, .(annotation = paste(
  sprintf("Bias = %.2f", mean(mid - benchmark_margin)),
  sprintf("RMSE = %.2f", sqrt(mean((mid - benchmark_margin)^2))),
  sprintf("Pearson r = %.2f", cor(mid, benchmark_margin, method = "pearson")),
  sprintf("90%% interval coverage = %.0f%%", 100 * mean(benchmark_margin > lo & benchmark_margin < hi)),
  sprintf("Call agreement = %.0f%%", 100 * mean(!disagree)), sep = "\n")), by = method]

# Shared axis limits include every posterior interval.
benchmark_limits <- range(c(benchmark_plot$lo, benchmark_plot$hi, benchmark_plot$benchmark_margin))
state_benchmark_plot <- ggplot(benchmark_plot, aes(mid, benchmark_margin)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey75", linewidth = 0.3) +
  geom_segment(aes(x = lo, xend = hi, yend = benchmark_margin),
               colour = "black", alpha = 0.25, linewidth = 0.3) +
  geom_point(colour = "black", alpha = 0.25, size = 1.2) +
  geom_line(data = benchmark_curves, colour = "darkorange", linewidth = 0.55) +
  ggrepel::geom_text_repel(aes(label = label, colour = disagree,
      fontface = ifelse(disagree, "bold", "plain")),
    size = 2.7, seed = label_seed, max.overlaps = Inf, max.iter = 50000, max.time = Inf,
    force_pull = 100, box.padding = 0.35, point.padding = 0.15,
    min.segment.length = 0, segment.linetype = "dotted",
    segment.colour = "grey70", segment.size = 0.2, show.legend = FALSE) +
  geom_text(data = benchmark_metrics,
    aes(x = -Inf, y = Inf, label = annotation), inherit.aes = FALSE,
    hjust = -0.08, vjust = 1.1, size = 3.1, lineheight = 1.15) +
  scale_colour_manual(values = c("FALSE" = "darkgrey", "TRUE" = "black")) +
  facet_wrap(~ method, nrow = 1) +
  coord_cartesian(xlim = benchmark_limits, ylim = benchmark_limits) +
  labs(title = "Benchmarking state R-D margins",
    subtitle = paste0("Fieldwork ", fieldwork_label, " | 538 forecast: 30 Oct 2024"),
    x = "PoSSUM R-D margin (percentage points)",
    y = "Benchmark R-D margin (percentage points)",
    caption = "Solid lines: 90% intervals. Dotted lines connect labels. Bold: different R/D calls. Orange: original GAM fit.") +
  plot_theme +
  theme(panel.border = element_rect(fill = NA, colour = "grey60"),
        strip.background = element_blank(), strip.text = element_text(face = "bold"),
        legend.position = "none")
ggsave(file.path(figure_dir, "state_benchmark.pdf"), state_benchmark_plot,
       width = 15, height = 7.5, units = "in", bg = "white")
