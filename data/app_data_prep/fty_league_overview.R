
df_fty_league_overview <<- df_fty_box_score |> 
      left_join(
        df_fty_box_score |> 
          select(competitor_id, matchup, all_of(anl_cols$h2h_cols)) |> 
          mutate(across(anl_cols$h2h_cols, \(x) percent_rank(x)), .by = matchup) |> 
          pivot_longer(all_of(anl_cols$h2h_cols), names_to = "stat", values_to = "perc_rank") |> 
          mutate(nine_cat = sum(perc_rank), .by = c(competitor_id, matchup)) |>
          slice_max(perc_rank, n = 5, by = c(competitor_id, matchup), with_ties = FALSE) |> 
          summarise(five_cat = sum(perc_rank), .by = c(competitor_id, matchup, nine_cat)),
        by = join_by(competitor_id, matchup)
      ) |> 
      group_by(matchup) |> 
      calc_z_pcts() |> 
      ungroup()
