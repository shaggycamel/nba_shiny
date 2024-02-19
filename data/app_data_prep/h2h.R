
# rolling stats -----------------------------------------------------------

# Doesn't account for situations where a player is traded.
# Assigns stats to player's most recent team (within one season)
df_rolling_stats <<- df_player_log |> 
  arrange(game_date) |> 
  mutate(across(any_of(anl_cols$stat_cols), \(x) slider::slide_period_dbl(x, game_date, "day", ~ mean(.x, na.rm = TRUE), .before = 15, .after = -1)), .by = player_id) |> 
  mutate(across(any_of(anl_cols$stat_cols), \(x) coalesce(x, 0))) |> 
  select(-c(slug_season, year_season, season_type, year_season_type, free_agent_status, game_id, wl)) |> 
  mutate(origin = "past") |> 
  (\(df_t){
    bind_rows(
      df_t,
      df_nba_schedule |> 
        filter(game_date >= cur_date) |> 
        left_join(
          select(df_nba_roster, player_id, fty_id, player_name=player, team_slug), 
          by = join_by(team == team_slug),
          relationship = "many-to-many"
        ) |> 
        select(player_id, fty_id, player_name, team_slug=team, game_date) |> 
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
  mutate(us_date = with_tz(timestamp, tzone = "EST"), .before = timestamp) |> 
  filter(us_date < cur_date) |> 
  mutate(
    ts = format(us_date, "%H:%M"), 
    us_date = as.Date(us_date),
    dow = lubridate::wday(us_date, week_start = 1),
    .after = timestamp
  ) |> 
  slice_max(ts, by = c(us_date, competitor_id)) |> 
  select(-c(ts, timestamp)) |> 
  left_join(
    select(df_nba_schedule, team, game_date, scheduled_to_play),
    by = join_by(player_team == team, us_date == game_date)    
  )


df_h2h_prepare <<- function(competitor=NULL, exclude=NULL, add=NULL, from_tomorrow=NULL){

  c_id <- unique(filter(df_fty_schedule, competitor_name == competitor)$competitor_id)
  
  # PRE-FUTURE
  df_future_pre <- df_fty_roster |> 
    mutate(
      us_date = as.Date(with_tz(timestamp, tzone = "EST")), 
      ts = format(with_tz(timestamp, tzone = "EST"), "%H:%M"), 
      dow = lubridate::wday(us_date, week_start = 1),
      .after = timestamp
    ) |> 
    slice_max(paste(us_date, ts), by = competitor_id) |> 
    select(-ts) |> 
    select(-c(timestamp, us_date, dow, league_week, starts_with("opponent"))) |> 
    bind_rows(
      filter(df_player_log, player_name %in% add) |>
        slice_max(order_by = game_date) |>
        select(player_id, player_fantasy_id = fty_id, player_name, player_team = team_slug) |> 
        mutate(season = cur_season, competitor_id = c_id, competitor_name = competitor)
    )
  
  
  # FUTURE
  df_future <- left_join(
      df_future_pre,
      filter(df_nba_schedule, game_date >= cur_date) |> 
        select(game_date, season_week, team),
      by = join_by(player_team == team),
      relationship = "many-to-many"
    ) |> 
    left_join(
      select(df_fty_schedule, -c(season, league_id)),
      by = join_by(competitor_id, competitor_name, between(game_date, week_start, week_end)),
      relationship = "many-to-many"
    ) |> 
    rename(us_date = game_date, league_week = season_week) |> 
    mutate(dow = lubridate::wday(us_date, week_start = 1)) |> 
    left_join(
      select(df_nba_schedule, team, game_date, scheduled_to_play),
      by = join_by(player_team == team, us_date == game_date)    
    ) |> 
    select(all_of(colnames(df_past)))
  

  df_h2h <- bind_rows(df_past, df_future) |> 
    left_join(
      select(df_rolling_stats, -c(fty_id, player_name, team_slug)),
      by = join_by(player_id, us_date == game_date)
    )
   
  
  if(from_tomorrow){
    df_h2h <- df_h2h |> 
      anti_join(
        filter(df_h2h, competitor_id == c_id, player_name %in% add, origin == "today"),
        by = join_by(competitor_id, player_id, us_date)
      ) |> 
      anti_join(
        filter(df_h2h, competitor_id == c_id, player_name %in% exclude, origin == "future"),
        by = join_by(competitor_id, player_id, us_date)
      )
  } else {
    df_h2h <- df_h2h |> 
      anti_join(
        filter(df_h2h, competitor_id == c_id, player_name %in% exclude, origin != "past"),
        by = join_by(competitor_id, player_id, us_date)
      )
  }

}

df_h2h_og <<- df_h2h_prepare(competitor = "senor_cactus", from_tomorrow = FALSE)
