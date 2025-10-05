df_stitch <<- bind_rows(
  df_fty_roster |>
    select(timestamp, player_fantasy_id, , player_injury_status) |>
    mutate(player_availability = "rostered"),

  df_fty_free_agents |>
    select(timestamp, player_fantasy_id = player_id, player_injury_status) |>
    mutate(player_availability = "free_agent")
) |>
  slice_max(timestamp, by = player_fantasy_id)


df_nba_player_box_score <<- df_nba_player_box_score |>
  left_join(
    select(df_stitch, -timestamp),
    by = setNames("player_fantasy_id", str_c(str_to_lower(platform_selected()), "_id")),
  )
