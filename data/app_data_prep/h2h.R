
# rolling stats -----------------------------------------------------------

# Doesn't account for situations where a player is traded.
# Assigns stats to player's most recent team (within one season)
df_rolling_stats <<- df_nba_player_box_score |> 
  arrange(game_date) |>
  mutate(across(any_of(anl_cols$stat_cols), \(x) slider::slide_period_dbl(x, game_date, "day", ~ mean(.x, na.rm = TRUE), .before = 15, .after = -1)), .by = player_id) |>
  filter(game_date < cur_date) |> 
  mutate(across(any_of(anl_cols$stat_cols), \(x) coalesce(x, 0))) |> 
  select(-c(season, season_type, year_season_type, game_id)) |> 
  mutate(origin = "past") |> 
  (\(df_t){
    bind_rows(
      df_t,
      df_nba_schedule |> 
        filter(game_date >= cur_date) |> 
        left_join(
          select(df_nba_roster, player_id, espn_id, yahoo_id, player_name=player, team_slug), 
          by = join_by(team == team_slug),
          relationship = "many-to-many"
        ) |> 
        select(player_id, espn_id, yahoo_id, player_name, team_slug=team, game_date) |> 
        left_join(
          slice_max(df_t, order_by = game_date, by = player_id) |> 
            select(player_id, any_of(anl_cols$stat_cols)),
          by = join_by(player_id),
          relationship = "many-to-many"
        ) |> 
        mutate(origin = if_else(game_date == cur_date, "today", "future"))
    )
  })()


# PAST
df_past <<- df_fty_roster |> 
  filter(timestamp < cur_date) |> 
  mutate(dow = lubridate::wday(date, week_start = 1), .after = date) |> 
  left_join(
    select(df_nba_schedule, team, game_date, scheduled_to_play),
    by = join_by(player_team == team, date == game_date)
  ) |> 
  rename(game_date = date)


df_h2h_prepare <<- function(c_id=NULL, exclude=NULL, add=NULL, from_tomorrow=NULL){
  
  print(paste("Sys.Date:", Sys.Date()))
  print(paste("Sys.time:", Sys.time()))
  print(paste("cur_date:", cur_date))
  print(paste("EST date:", force_tz(as.Date(with_tz(Sys.time(), "EST")), tz = "EST")))
  print(paste("EST time:", force_tz((with_tz(Sys.time(), "EST")), tz = "EST")))
  print(paste("Max roster date:", force_tz(as.Date(max(df_fty_roster$timestamp)), tz = "EST")))
  print(paste("Max roster timestamp:", force_tz((max(df_fty_roster$timestamp)), tz = "EST")))
  
    
  # TODAY
  df_today <- df_fty_roster |> 
    filter(timestamp >= force_tz(as.Date(max(timestamp)), tz = "EST")) |> 
    select(-c(league_week, starts_with("opponent"))) |>
    bind_rows(
      filter(df_nba_player_box_score, player_name %in% add) |>
        slice_max(order_by = game_date, by = player_name) |> 
        select(player_id, player_fantasy_id:=paste0(str_to_lower(platform_selected), "_id"), player_name, player_team = team_slug) |> 
        mutate(season = cur_season, competitor_id = c_id)
    )
  
  # FUTURE
  df_future <<- left_join(
      df_today,
      df_nba_schedule |> 
        filter(game_date >= cur_date) |>
        left_join(
          distinct(select(df_fty_schedule, starts_with("week"))),
          by = join_by(game_date >= week_start, game_date <= week_end)
        ) |>
        select(game_date, fty_matchup_week=week, team),
      by = join_by(player_team == team),
      relationship = "many-to-many"
    ) |> 
    left_join(
      select(df_fty_schedule, -c(season, league_id, platform)),
      by = join_by(competitor_id, between(game_date, week_start, week_end)),
      relationship = "many-to-many"
    ) |> 
    rename(league_week = fty_matchup_week) |> 
    mutate(dow = lubridate::wday(date, week_start = 1)) |> 
    left_join(
      select(df_nba_schedule, team, game_date, scheduled_to_play),
      by = join_by(player_team == team, game_date)    
    ) |>
    select(all_of(colnames(df_past)))
  
  # COMBINE
  df_h2h <- bind_rows(df_past, df_future) |> 
    left_join(
      select(df_rolling_stats, -c(espn_id, yahoo_id, player_name, team_slug)),
      by = join_by(player_id, game_date)
    )
   
  # FROM TOMORROW TWEAKING
  if(from_tomorrow){
    df_h2h <- df_h2h |> 
      anti_join(
        filter(df_h2h, competitor_id == c_id, player_name %in% add, origin == "today"),
        by = join_by(competitor_id, player_id, game_date)
      ) |> 
      anti_join(
        filter(df_h2h, competitor_id == c_id, player_name %in% exclude, origin == "future"),
        by = join_by(competitor_id, player_id, game_date)
      )
  } else {
    df_h2h <- df_h2h |> 
      anti_join(
        filter(df_h2h, competitor_id == c_id, player_name %in% exclude, origin != "past"),
        by = join_by(competitor_id, player_id, game_date)
      )
  }

}

df_h2h_og <<- df_h2h_prepare(c_id = 5, from_tomorrow = FALSE)
