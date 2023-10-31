
# Project Constants -------------------------------------------------------

# Dataframe used to select NBA stats
stat_selection <- 
  tibble::tribble(
    ~formatted_name, ~database_name,
     "Minutes", "min",
     "3-pointers", "fg3_m",
     "Points", "pts",
     "Field Goal %", "fg_pct",
     "Free Throw %", "ft_pct",
     "Field Goal Z", "fg_z",
     "Free Throw Z", "ft_z",
     "Rebounds", "reb",
     "Assists", "ast",
     "Steals", "stl",
     "Blocks", "blk",
     "Turnovers", "tov"
  )

# Analysis columns
anl_cols <- list(
  stat_cols = c("min", "fgm", "fga", "fg_pct", "fg_z", "fg3_m", "fg3_a", "fg3_pct", "ft_pct", "ft_z", "ftm", "fta", "oreb", "dreb", "reb", "ast", "stl", "blk", "tov", "pf", "pts"),
  h2h_cols = c("fg_pct", "fg3_m", "ft_pct", "reb", "ast", "stl", "blk", "tov", "pts")
)


# Custom Functions --------------------------------------------------------

calc_z_pcts <- function(df){
  df |> 
    dplyr::mutate(
      fg_pct = coalesce(fgm / fga, 0), 
      ft_pct = coalesce(ftm / fta, 0),
      fg_impact = (fg_pct - (sum(fgm) / sum(fga))) * fga,
      ft_impact = (ft_pct - (sum(ftm) / sum(fta))) * fta,
      fg_z = (fg_impact - mean(fg_impact)) / sd(fg_impact), 
      ft_z = (ft_impact - mean(ft_impact)) / sd(ft_impact)
    ) |> select(-ends_with("impact"))
}

# Reverse plotly legend labels
reverse_legend_labels <- function(plotly_plot) {
  n_labels <- length(plotly_plot$x$data)
  plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
  plotly_plot
}


