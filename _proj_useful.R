
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


