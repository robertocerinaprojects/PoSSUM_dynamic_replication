# Summarise national, geographic, demographic and past-vote estimates.
# Report medians and central 90% credible intervals.
# Candidate shares are percentages among voters; turnout and stay-home
# are percentages among adults. The R-D margin is in percentage points.
# Run after script 06, with the same run_dir.
# Output: one CSV per crosstab and an RDS containing all summary tables,
# saved in the selected fit's summaries/ folder.

library(data.table)

# 1. Load the poststratified draws -------------------------------------------
load(file.path(run_dir, "poststratification.RData"))

# 2. Summarise each crosstab -------------------------------------------------
summary_tables <- list()

for (group in names(crosstab.pred_list)) {

  # Keep the group label and draw ID; stack the quantities into one column.
  long_draws <- melt(
    crosstab.pred_list[[group]],
    id.vars = c(group, "sim_id"),
    variable.name = "quantity",
    value.name = "value"
  )

  # Summarise each quantity within each group.
  # Multiplying by 100 gives percentages, or percentage points for R-D.
  summary_tables[[group]] <- long_draws[, .(
    median = 100 * median(value),
    lower = 100 * quantile(value, 0.05),
    upper = 100 * quantile(value, 0.95)
  ), by = c(group, "quantity")]
}

# 3. Save the tables --------------------------------------------------------
summary_dir <- file.path(run_dir, "summaries")
dir.create(summary_dir, showWarnings = FALSE)

for (group in names(summary_tables)) {

  # The original "date" crosstab contains the national estimates.
  filename <- if (group == "date") "national" else group

  fwrite(
    summary_tables[[group]],
    file.path(summary_dir, paste0(filename, ".csv"))
  )
}

# Retain all tables together for subsequent plotting in R.
saveRDS(
  summary_tables,
  file.path(summary_dir, "summary_tables.rds")
)

# 4. Inspect a table --------------------------------------------------------
print(summary_tables$date, digits = 4)