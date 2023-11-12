
df_player_log <<- dh_getQuery(db_con, "player_log.sql") |> 
  mutate(slug_season = ordered(slug_season)) |> 
  mutate(season_type = ordered(season_type, c("Pre Season", "Regular Season", "Playoffs"))) |> 
  mutate(year_season_type = forcats::fct_cross(season_type, str_sub(slug_season, start = 6), sep=" "))
  
