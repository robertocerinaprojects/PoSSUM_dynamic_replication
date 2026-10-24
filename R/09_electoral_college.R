# Calculate Electoral College outcomes from the saved vote2024 predictions.
# Keep all state and district predictions from the same sim_id together.
# As in the original analysis, the largest candidate share wins each unit.
# Run from the repository root after script 06, with the same run_dir.
# Output: electoral_college.RDS and CSV tables in run_dir/electoral_college/.

library(data.table)

# 1. Load predictions and the original electoral-vote allocation -------------
load(file.path(run_dir, "poststratification.RData"))
load("data/raw/ballot_access.RData")
setDT(ballot_access)

candidates <- c("D", "R", "K", "L", "G", "W")
split_states <- c("Maine", "Nebraska")
draw_ids <- frame_draws$sim_id

# Base R supplies the 50 state names; add the District of Columbia.
state_names <- setNames(
  c(state.name, "District of Columbia"),
  c(state.abb, "DC")
)
allocation <- ballot_access[, .(
  area = unname(state_names[as.character(`State / electors`)]),
  electors = as.numeric(as.character(`Electoral votes`))
)]

# Maine and Nebraska award two electors statewide and one per district.
allocation[area %in% split_states, electors := 2]
allocation <- rbindlist(list(
  allocation,
  data.table(
    area = c("Maine 1st", "Maine 2nd", "Nebraska 1st",
             "Nebraska 2nd", "Nebraska 3rd"),
    electors = 1
  )
))
stopifnot(nrow(allocation) == 56L, !anyNA(allocation),
          uniqueN(allocation$area) == 56L, sum(allocation$electors) == 538)

# 2. Combine state, district and statewide Maine/Nebraska predictions -------
# The electoral-area crosstab contains Maine/Nebraska districts. Their
# statewide estimates come from the state_simple crosstab in the same draw.
ec_predictions <- crosstab.pred_list$state_electoral.college[
  !state_electoral.college %in% split_states,
  c("sim_id", "state_electoral.college", candidates), with = FALSE
]
setnames(ec_predictions, "state_electoral.college", "area")

statewide <- crosstab.pred_list$state_simple[
  state_simple %in% split_states,
  c("sim_id", "state_simple", candidates), with = FALSE
]
setnames(statewide, "state_simple", "area")
ec_predictions <- rbindlist(list(ec_predictions, statewide), use.names = TRUE)
ec_predictions[, electors := allocation$electors[match(area, allocation$area)]]

# Each saved draw must contain every allocation unit exactly once.
coverage <- ec_predictions[, .(units = .N, electors = sum(electors)), by = sim_id]
stopifnot(
  dep.var == "vote2024", length(draw_ids) > 0L, !anyDuplicated(draw_ids),
  !anyNA(ec_predictions),
  !anyDuplicated(ec_predictions, by = c("sim_id", "area")),
  setequal(coverage$sim_id, draw_ids),
  all(coverage$units == 56L), all(coverage$electors == 538),
  all(is.finite(as.matrix(ec_predictions[, ..candidates])))
)

# 3. Award electors within each draw ----------------------------------------
# The original analysis did not specify how to resolve exact vote-share ties.
# Stop on such a tie rather than selecting a candidate by column order.
pick_winner <- function(shares) {
  leaders <- which(shares == max(shares))
  if (length(leaders) != 1L) {
    stop("An electoral unit has an exact tie; inspect ec_predictions.")
  }
  candidates[leaders]
}
ec_predictions[, winner := apply(.SD, 1, pick_winner), .SDcols = candidates]

# Include every candidate in every draw, even when they win zero electors.
ec_draws <- ec_predictions[, .(
  candidate = candidates,
  electoral_votes = vapply(candidates, function(choice) {
    sum(electors[winner == choice])
  }, numeric(1))
), by = sim_id]

# Any candidate reaching 270 has a majority. Otherwise record "No majority".
election_draws <- ec_draws[, .(
  winner = if (any(electoral_votes >= 270)) {
    candidate[which.max(electoral_votes)]
  } else {
    "No majority"
  }
), by = sim_id]

# 4. Summarise electoral totals and win probabilities -----------------------
# Use the same 5th/50th/95th percentiles as the other summary tables.
ec_summary <- ec_draws[, .(
  median = median(electoral_votes),
  lower = quantile(electoral_votes, 0.05),
  upper = quantile(electoral_votes, 0.95),
  win_probability = mean(electoral_votes >= 270)
), by = candidate]

# Count wins over ALL draws for each unit.
unit_win_probabilities <- ec_predictions[, .(
  candidate = candidates,
  win_probability = vapply(candidates, function(choice) {
    mean(winner == choice)
  }, numeric(1))
), by = .(area, electors)]

election_probabilities <- data.table(outcome = c(candidates, "No majority"))
election_probabilities[, probability := vapply(outcome, function(choice) {
  mean(election_draws$winner == choice)
}, numeric(1))]

# 5. Save results for inspection and subsequent plotting --------------------
ec_dir <- file.path(run_dir, "electoral_college")
dir.create(ec_dir, showWarnings = FALSE)

ec_results <- list(
  allocation = allocation,
  unit_draws = ec_predictions[, .(sim_id, area, electors, winner)],
  electoral_vote_draws = ec_draws,
  election_draws = election_draws,
  electoral_vote_summary = ec_summary,
  unit_win_probabilities = unit_win_probabilities,
  election_probabilities = election_probabilities,
  survey_period = survey_period,
  dep.var = dep.var,
  speculation = speculation
)
saveRDS(ec_results, file.path(ec_dir, "electoral_college.RDS"))

tables <- c("electoral_vote_draws", "electoral_vote_summary",
            "unit_win_probabilities", "election_probabilities")
for (table in tables) {
  fwrite(ec_results[[table]], file.path(ec_dir, paste0(table, ".csv")))
}

print(ec_summary)
print(election_probabilities)
message("Saved Electoral College results to ", ec_dir)
