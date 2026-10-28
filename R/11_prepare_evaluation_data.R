# Prepare the observed election results used by the evaluation scripts.
# Run from the repository root; no fitted model is needed for this step.
# Output: data/processed/evaluation_data.RData.

library(data.table)

# 1. Download the election-results snapshot used in the original analysis ----
# This is the DDHQ snapshot dated 7 November 2024, 1:50 PM, not a later tally.
# Pin both the source commit and file checksum so the benchmark stays fixed.
source_commit <- "b9005a80c7f6dbbf6e1f48c9fa625dfae24c8b22" # Git commit ID identifying a particular saved version of the original PoSSUM repository.
results_file <- "data/downloaded/evaluation/election_results_2024_DDHQ.csv"
dir.create(dirname(results_file), recursive = TRUE, showWarnings = FALSE)

if (!file.exists(results_file)) {
  download.file(
    paste0("https://raw.githubusercontent.com/robertocerinaprojects/PoSSUM/",
           source_commit,
           "/evaluation_data/2024%20US%20election%20results%20-%20DDHQ.csv"),
    results_file, mode = "wb"
  )
}
if (unname(tools::md5sum(results_file)) != "72fa3f575fbf0d4d3a92f2040d0d773e") {
  stop("The downloaded DDHQ file does not match the original snapshot.")
}
results <- fread(results_file)
setnames(results, c("Abbrv", "Votes"), c("state_abb", "true_votes"))
results[, true_votes := as.numeric(gsub(",", "", true_votes))]

# 2. Prepare 2024 area shares and margins ------------------------------------
# Each share uses ALL recorded candidate votes in its area as the denominator.
results[, pct_votes := true_votes / sum(true_votes), by = state_abb]

# Base R supplies the state names; add DC and the five electoral districts.
state_names <- setNames(c(state.name, "District of Columbia"), c(state.abb, "DC"))
district_names <- c("NE-01" = "Nebraska 1st", "NE-02" = "Nebraska 2nd",
                    "NE-03" = "Nebraska 3rd", "ME-01" = "Maine 1st",
                    "ME-02" = "Maine 2nd")
area_names <- c(state_names, district_names)
results[, state_electoral.college := unname(area_names[state_abb])]
stopifnot(!anyNA(results$state_electoral.college))

candidate_labels <- c(
  "Donald J. Trump" = "R", "Kamala Harris" = "D",
  "Robert F. Kennedy Jr." = "K", "Jill Stein" = "G",
  "Chase Oliver" = "L", "Cornel West" = "W"
)
results.wide <- dcast(results, state_electoral.college ~ Candidate,
                      value.var = "pct_votes")
setnames(results.wide, names(candidate_labels), unname(candidate_labels))

# Group OTHER and handle missing candidate shares.
# An absent named candidate remains NA; absent OTHER candidates contribute zero.
other_candidates <- c("Vermin Supreme", "Shiva Ayyadurai", "Robert Wells",
                      "Claudia De la Cruz", "Richard Duncan", "Peter Sonski",
                      "Chris Garrity", "Joel Skousen", "None Of These Candidates")
results.wide[, OTHER := rowSums(.SD, na.rm = TRUE), .SDcols = other_candidates]
results.wide <- results.wide[, .(state_electoral.college, R, D, K, G, L, W, OTHER)]
results.wide[, `:=`(
  D_2pv = D / (D + R), R_2pv = R / (D + R),
  RD_margin_2pv = R / (D + R) - D / (D + R), RD_margin = R - D
)]

# 3. Prepare the original 2020 reference data --------------------------------
# Use the unscaled area data, including its eligible-population estimates.
load("data/raw/area_level.RData")
setDT(predictor_area_level)
vote_columns <- grep("^pct[.]2020[.]votes[.]", names(predictor_area_level),
                     value = TRUE)
obs2020 <- predictor_area_level[
  , c("state_electoral.college", vote_columns,
      "N.2020.voting.eligible.pop_predict"), with = FALSE
]
setnames(obs2020, sub("^pct[.]2020[.]votes[.]", "", names(obs2020)))
# T is turnout as a percentage of the voting-eligible population.
parties2020 <- c("D", "R", "L", "OTHER", "PSL", "G")

# The input contains districts for Maine/Nebraska. Reconstruct statewide totals
# by adding implied vote counts, rather than averaging district percentages.
aggregate_statewide <- function(data, state) {
  districts <- data[grepl(paste0("^", state, " "), state_electoral.college)]
  districts[, (parties2020) := lapply(.SD, function(p) {
    (p / 100) * (T / 100 * N.2020.voting.eligible.pop_predict)
  }), .SDcols = parties2020]
  districts[, `:=`(
    T = (T / 100) * N.2020.voting.eligible.pop_predict,
    stay.home = N.2020.voting.eligible.pop_predict * (1 - T / 100),
    state_electoral.college = state
  )]
  totals <- districts[, lapply(.SD, sum), by = state_electoral.college]
  totals[, (parties2020) := lapply(.SD, function(v) 100 * v / T),
         .SDcols = parties2020]
  totals[, `:=`(
    T = 100 * T / N.2020.voting.eligible.pop_predict,
    stay.home = 100 * stay.home / N.2020.voting.eligible.pop_predict
  )]
  totals
}
stopifnot(
  !any(obs2020$state_electoral.college %in% c("Maine", "Nebraska")),
  all(unname(district_names) %in% obs2020$state_electoral.college)
)
obs2020 <- rbindlist(list(
  obs2020, aggregate_statewide(obs2020, "Nebraska"),
  aggregate_statewide(obs2020, "Maine")
))
obs2020[, `:=`(
  D_2pv = 100 * D / (D + R), R_2pv = 100 * R / (D + R),
  RD_margin_2pv = 100 * R / (D + R) - 100 * D / (D + R),
  RD_margin = R - D
)]
obs2020[, OTHER := OTHER + PSL][, PSL := NULL]

# 4. Compute national shares and the swing from 2020 -------------------------
# Use the 50 states and DC only. District rows must not be counted again.
results[, Candidate := unname(candidate_labels[Candidate])]
results[is.na(Candidate), Candidate := "OTHER"]
stopifnot(setequal(results[state_abb %in% names(state_names), state_abb],
                   names(state_names)))
results.nat <- results[state_abb %in% names(state_names),
                       .(true_votes = sum(true_votes, na.rm = TRUE)), by = Candidate]
results.nat[, true_votes := 100 * true_votes / sum(true_votes)]

obs2020.nat <- copy(obs2020[state_electoral.college %in% unname(state_names)])
stopifnot(nrow(obs2020.nat) == 51L,
          setequal(obs2020.nat$state_electoral.college, unname(state_names)))
for (party in c("R", "D", "G", "L", "OTHER")) {
  obs2020.nat[, (party) := (get(party) / 100) *
               ((T / 100) * N.2020.voting.eligible.pop_predict)]
}
obs2020.nat <- obs2020.nat[, lapply(.SD, sum),
                         .SDcols = c("R", "D", "G", "L", "OTHER")]
obs2020.nat <- 100 * obs2020.nat / rowSums(obs2020.nat)
obs2020.nat[, RD_margin := R - D]

# Retain the original column names: these national "true_votes" are percentages.
# Candidates K and W have a zero 2020 reference, as in the original script.
results.nat[, true_votes.2020 := vapply(Candidate, function(candidate) {
  if (candidate %in% names(obs2020.nat)) obs2020.nat[[candidate]] else 0
}, numeric(1))]
results.nat <- rbindlist(list(results.nat, data.table(
  Candidate = "RD_margin",
  true_votes = results.nat[Candidate == "R", true_votes] -
               results.nat[Candidate == "D", true_votes],
  true_votes.2020 = results.nat[Candidate == "R", true_votes.2020] -
                    results.nat[Candidate == "D", true_votes.2020]
)))
results.nat[, swing := true_votes - true_votes.2020]

# 5. Repeat the national comparison using only the two major parties ---------
results.nat_2pv <- results.nat[Candidate %in% c("R", "D")]
results.nat_2pv[, c("true_votes", "true_votes.2020") :=
                 lapply(.SD, function(x) 100 * x / sum(x)),
               .SDcols = c("true_votes", "true_votes.2020")]
results.nat_2pv <- rbindlist(list(results.nat_2pv, data.table(
  Candidate = "RD_margin",
  true_votes = results.nat_2pv[Candidate == "R", true_votes] -
               results.nat_2pv[Candidate == "D", true_votes],
  true_votes.2020 = results.nat_2pv[Candidate == "R", true_votes.2020] -
                    results.nat_2pv[Candidate == "D", true_votes.2020],
  swing = NA_real_
)))
results.nat_2pv[, swing := true_votes - true_votes.2020]

# 6. Save the reference tables for the next evaluation scripts ---------------
evaluation_source <- list(
  ddhq_commit = source_commit,
  ddhq_updated = unique(results[["Last Updated"]]),
  input_md5 = tools::md5sum(c(results_file, "data/raw/area_level.RData"))
)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
save(results, results.wide, obs2020, obs2020.nat, results.nat, results.nat_2pv,
     evaluation_source, file = "data/processed/evaluation_data.RData")

message("Saved evaluation data for ", nrow(results.wide),
        " areas to data/processed/evaluation_data.RData")
print(results.nat, digits = 5)
