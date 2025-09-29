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

# Cat extraction
cat_specs <- function(vec = FALSE, h2h = TRUE, incl_nba_cat = NULL, excl_nba_cat = NULL) {
  # fmt: skip
  obj <- df_fty_cats2
  if (h2h) {
    obj <- filter(obj, h2h_cat)
  }
  if (!is.null(incl_nba_cat)) {
    obj <- bind_rows(obj, filter(df_fty_cats2, nba_category %in% incl_nba_cat))
  }
  if (!is.null(excl_nba_cat)) {
    obj <- filter(obj, !nba_category %in% excl_nba_cat)
  }
  obj <- arrange(distinct(obj), display_order)
  obj <- as.list(tibble::deframe(select(obj, fmt_category, nba_category)))
  if (vec) {
    obj <- unlist(obj, use.names = FALSE)
  }
  obj
}
