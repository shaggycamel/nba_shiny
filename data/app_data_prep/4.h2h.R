
df_h2h <<- dh_getQuery(db_con, "h2h.sql") |> 
    mutate(league_week = if_else(origin == "past" & dow == 7, league_week - 1, league_week)) |> 
    left_join(
      dh_getQuery(db_con, "SELECT week, competitor_id, opponent_id, opponent_name FROM fty.league_schedule WHERE season = '{cur_season}'"),
      by = join_by(league_week == week, competitor_id)
    ) |> 
    mutate(playing = case_when(
      playing == 1 & player_injury_status == "OUT" ~ "1*",
      playing == 1 ~ "1",
      .default = NA_character_
    ))

