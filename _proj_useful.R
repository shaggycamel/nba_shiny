# Custom Functions --------------------------------------------------------

calc_z_pcts <- function(df) {
  df |>
    dplyr::mutate(
      fg_pct = dplyr::coalesce(fgm / fga, 0),
      ft_pct = dplyr::coalesce(ftm / fta, 0),
      fg_impact = (fg_pct - (sum(fgm, na.rm = TRUE) / sum(fga, na.rm = TRUE))) * fga,
      ft_impact = (ft_pct - (sum(ftm, na.rm = TRUE) / sum(fta, na.rm = TRUE))) * fta,
      fg_z = (fg_impact - mean(fg_impact, na.rm = TRUE)) / sd(fg_impact, na.rm = TRUE),
      ft_z = (ft_impact - mean(ft_impact, na.rm = TRUE)) / sd(ft_impact, na.rm = TRUE)
    ) |>
    dplyr::select(-tidyr::ends_with("impact"))
}

# Reverse plotly legend labels
reverse_legend_labels <- function(plotly_plot) {
  n_labels <- length(plotly_plot$x$data)
  plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
  plotly_plot
}

# General categories
general_cat_cols = c(
  "min",
  "fgm",
  "fga",
  "fg_pct",
  "fg_z",
  "fg3_m",
  "fg3_a",
  "fg3_pct",
  "ft_pct",
  "ft_z",
  "ftm",
  "fta",
  "oreb",
  "dreb",
  "reb",
  "ast",
  "stl",
  "blk",
  "tov",
  "pf",
  "pts",
  "dd2",
  "td3",
  "plus_minus"
)
