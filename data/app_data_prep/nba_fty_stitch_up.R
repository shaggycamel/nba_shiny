df_stitch <<- bind_rows(
  df_fty_roster |>
    select(assigned_date, player_fantasy_id, , player_injury_status) |>
    mutate(player_availability = "rostered"),

  df_fty_free_agents |>
    select(assigned_date = timestamp, player_fantasy_id = player_id, player_injury_status) |>
    mutate(player_availability = "free_agent", assigned_date = lubridate::as_date(assigned_date))
) |>
  slice_max(assigned_date, by = player_fantasy_id)


df_nba_player_box_score <<- df_nba_player_box_score |>
  left_join(
    select(df_stitch, -assigned_date),
    by = setNames("player_fantasy_id", paste0(str_to_lower(platform_selected), "_id")),
  )
