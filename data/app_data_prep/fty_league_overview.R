
df_fty_league_overview_prepare <<- function(platform_selected, league_selected){
  
  df_fty_league_overview <- 
    df_fty_box_score |> 
    left_join(
      df_fty_box_score |> 
        select(competitor_id, matchup, all_of(anl_cols$h2h_cols)) |> 
        mutate(across(anl_cols$h2h_cols, \(x) percent_rank(x)), .by = matchup) |> 
        mutate(tov = 1 - tov) |> # Reverse turnover distribution | Eventually fix this in any hover boxes
        pivot_longer(all_of(anl_cols$h2h_cols), names_to = "stat", values_to = "perc_rank") |> 
        mutate(nine_cat = sum(perc_rank), .by = c(competitor_id, matchup)) |>
        slice_max(perc_rank, n = 5, by = c(competitor_id, matchup), with_ties = FALSE) |> 
        summarise(five_cat = sum(perc_rank), .by = c(competitor_id, matchup, nine_cat)),
      by = join_by(competitor_id, matchup)
    ) |> 
    group_by(matchup) |> 
    calc_z_pcts() |> 
    ungroup() |> 
    mutate(
      across(
        c(unlist(fmt_to_db_stat_name[fmt_to_db_stat_name != "min"], use.names = FALSE), "matchup"), 
        \(x) lead(x, order_by = matchup), 
        .names = "{.col}_lead"
      ),
      .by = competitor_id
    ) |> 
    mutate(
      across(
        unlist(fmt_to_db_stat_name[fmt_to_db_stat_name != "min"], use.names = FALSE),
        \(x) rank(x*-1),
        .names = "{.col}_rank"
      ),
      .by = matchup
    ) |> 
    mutate(tov_rank = rank(tov), .by = matchup) |> 
    mutate(
      across(
        paste0(unlist(fmt_to_db_stat_name[fmt_to_db_stat_name != "min"], use.names = FALSE), "_rank"), 
        \(x) lead(x, order_by = matchup), 
        .names = "{.col}_lead"
      ),
      .by = competitor_id
    )
  
  df_latest_matchup <- filter(df_fty_league_overview, matchup == max(matchup))
  df_fty_league_overview <- filter(df_fty_league_overview, matchup < max(matchup))
    
  df_fty_league_overview <- 
    df_fty_league_overview |> 
    group_by(competitor_id, matchup) |> 
    group_map(.keep = TRUE, \(df_t, ...){
      x <- seq(-5, 5, 0.3)
      df_ls <- list()
      for(stat in fmt_to_db_stat_name[fmt_to_db_stat_name != "min"]){
        
        matchup_sigmoid <- x
        if(df_t[[stat]] > df_t[[paste0(stat, "_lead")]]) matchup_sigmoid <- rev(matchup_sigmoid)
        
        stat_sigmoid <- scales::rescale(pracma::sigmoid(matchup_sigmoid), to = c(df_t[[stat]], df_t[[paste0(stat, "_lead")]]))
        stat_rank_sigmoid <- scales::rescale(pracma::sigmoid(matchup_sigmoid), to = c(df_t[[paste0(stat, "_rank")]], df_t[[paste0(stat, "_rank_lead")]]))
        matchup_sigmoid <- scales::rescale(matchup_sigmoid, to = c(df_t$matchup, df_t$matchup_lead))
        
        df_ls <- append(df_ls, list(tibble(competitor_id = df_t$competitor_id, matchup = df_t$matchup, matchup_sigmoid = matchup_sigmoid, stat = stat, sigmoid = stat_sigmoid, rank_sigmoid = stat_rank_sigmoid)))
      }
      df_ls
    }) |> 
    bind_rows() |> 
    filter(!(as.integer(matchup_sigmoid) == matchup_sigmoid & matchup != matchup_sigmoid)) |> 
    pivot_wider(names_from = stat, values_from = c(sigmoid, rank_sigmoid)) |> 
    rename_with(\(x) str_remove(x, "sigmoid_"), .cols = starts_with("sigmoid_")) |> 
    rename_with(\(x) paste0(str_remove(x, "rank_sigmoid_"), "_rank"), .cols = starts_with("rank_sigmoid_"))
  
  df_fty_league_overview |> 
    bind_rows(select(df_latest_matchup, any_of(colnames(df_fty_league_overview)))) |> 
    mutate(matchup_sigmoid = if_else(is.na(matchup_sigmoid), matchup, matchup_sigmoid)) |> 
    left_join(
      df_fty_base |> 
        filter(platform == platform_selected, league_id == league_selected) |> 
        select(competitor_id, competitor_name), 
      by = join_by(competitor_id)
    )

}

