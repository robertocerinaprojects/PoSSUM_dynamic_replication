# Plot national candidate shares and R-D margins across all crosstabs.
# Points show posterior medians; lines show central 90% credible intervals.
# Order every margin plot from smallest R-D at the top to largest at the bottom.
# Shares are percentages among voters; R-D margins are in percentage points.
# Run after script 07, with the same run_dir.
# Output: PDF figures in the selected fit's figures/ folder.

library(data.table)
library(ggplot2)

# 1. Load the summaries -----------------------------------------------------
summary_tables <- readRDS(
  file.path(run_dir, "summaries", "summary_tables.rds")
)

figure_dir <- file.path(run_dir, "figures")
dir.create(figure_dir, showWarnings = FALSE)

# 2. Define a common plotting style ----------------------------------------
plot_intervals <- function(plot_data, title, x_label) {
  ggplot(plot_data, aes(x = median, y = label)) +
    geom_linerange(
      aes(xmin = lower, xmax = upper),
      orientation = "y"
    ) +
    geom_point(size = 2) +
    labs(
      title = title,
      subtitle = "Median and central 90% credible interval",
      x = x_label,
      y = NULL
    ) +
    theme_minimal(base_size = 11)
}

# Keep the plots available for viewing in the R session.
plots <- list()

# 3. Plot national candidate shares ----------------------------------------
# Turnout and stay-home use a different denominator, so exclude them here.
national <- summary_tables$date[
  !quantity %in% c("turnout", "stay home", "RD_margin")
]

national[, label := factor(
  quantity,
  levels = rev(as.character(quantity))
)]

plots$national_vote <- plot_intervals(
  national,
  title = "National vote estimates",
  x_label = "Share among voters (%)"
)

ggsave(
  file.path(figure_dir, "national_vote.pdf"),
  plot = plots$national_vote,
  width = 7,
  height = 4
)

# 4. Plot the R-D margin for every other crosstab ----------------------------
for (group in setdiff(names(summary_tables), "date")) {
  margins <- summary_tables[[group]][quantity == "RD_margin"]

  # Use readable category labels.
  label_values <- as.character(margins[[group]])
  label_values[label_values == "OTHER"] <- "Other"

  # Sort categories by their median R-D margin.
  label_order <- label_values[order(margins$median)]

  # Reverse factor levels so the smallest margin appears at the top.
  margins[, label := factor(label_values, levels = rev(label_order))]

  plots[[group]] <- plot_intervals(
    margins,
    title = paste("R-D margin by", group),
    x_label = "Republican minus Democratic share (percentage points)"
  ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      colour = "grey50"
    )

  # Give larger crosstabs enough vertical space for their labels.
  ggsave(
    file.path(figure_dir, paste0("RD_margin_", group, ".pdf")),
    plot = plots[[group]],
    width = 7,
    height = max(4, 2 + 0.22 * nrow(margins))
  )
}

# 5. Display plots ----------------------------------------------------------
print(plots$national_vote)

# Run these individually to view other breakdowns:
# plots$gender
# plots$state_simple
# plots$vote2020
# plots$vote2022