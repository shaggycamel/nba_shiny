
# Custom Functions --------------------------------------------------------

calc_z_pcts <- function(df, name_suffix=NULL){
  df |> 
    dplyr::mutate(
      !!sym(paste0("fg", name_suffix, "_pct")) := dplyr::coalesce(!!sym(paste0("fgm", name_suffix)) / !!sym(paste0("fga", name_suffix)), 0), 
      !!sym(paste0("ft", name_suffix, "_pct")) := dplyr::coalesce(!!sym(paste0("ftm", name_suffix)) / !!sym(paste0("fta", name_suffix)), 0),
      fg_impact = (!!sym(paste0("fg", name_suffix, "_pct")) - (sum(!!sym(paste0("fgm", name_suffix)), na.rm = TRUE) / sum(!!sym(paste0("fga", name_suffix)), na.rm = TRUE))) * !!sym(paste0("fga", name_suffix)),
      ft_impact = (!!sym(paste0("ft", name_suffix, "_pct")) - (sum(!!sym(paste0("ftm", name_suffix)), na.rm = TRUE) / sum(!!sym(paste0("fta", name_suffix)), na.rm = TRUE))) * !!sym(paste0("fta", name_suffix)),
      !!sym(paste0("fg", name_suffix, "_z")) := (fg_impact - mean(fg_impact, na.rm = TRUE)) / sd(fg_impact, na.rm = TRUE), 
      !!sym(paste0("ft", name_suffix, "_z")) := (ft_impact - mean(ft_impact, na.rm = TRUE)) / sd(ft_impact, na.rm = TRUE)
    ) |> dplyr::select(-tidyr::ends_with("impact"))
}

# Reverse plotly legend labels
reverse_legend_labels <- function(plotly_plot) {
  n_labels <- length(plotly_plot$x$data)
  plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
  plotly_plot
}


