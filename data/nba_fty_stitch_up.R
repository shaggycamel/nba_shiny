
df_stitch <- bind_rows(
  df_fty_roster |> 
    filter(timestamp > max(timestamp) - 1000) |> 
    select(player_fantasy_id, player_injury_status) |> 
    mutate(player_availability = "rostered"),

  df_fty_free_agents |> 
    select(player_fantasy_id=player_id, player_injury_status) |> 
    mutate(player_availability = "free_agent")  
)



df_nba_player_box_score <<- df_nba_player_box_score |> 
  left_join(
    df_stitch, 
    by = setNames("player_fantasy_id", paste0(str_to_lower(platform_selected), "_id")),
  )
