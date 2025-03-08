
# rolling stats -----------------------------------------------------------

# Doesn't account for situations where a player is traded.
# Assigns stats to player's most recent team (within one season)
df_rolling_stats <<- df_nba_player_box_score |> 
  arrange(game_date) |>
  mutate(across(any_of(anl_cols$stat_cols), \(x) slider::slide_period_dbl(x, game_date, "day", ~ mean(.x, na.rm = TRUE), .before = 15, .after = -1)), .by = player_id) |>
  filter(game_date < cur_date) |> 
  mutate(across(any_of(anl_cols$stat_cols), \(x) coalesce(x, 0))) |> 
  select(-c(season, season_type, year_season_type, game_id))

df_rolling_stats <<- df_rolling_stats |> 
  bind_rows(
    df_nba_schedule |> 
      filter(game_date > (cur_date - days(1))) |> 
      left_join(
        select(df_nba_roster, player_id, espn_id, yahoo_id, player_name=player, team_slug), 
        by = join_by(team == team_slug),
        relationship = "many-to-many"
      ) |> 
      select(player_id, espn_id, yahoo_id, player_name, team_slug=team, game_date) |> 
      left_join(
        slice_max(df_rolling_stats, order_by = game_date, by = player_id) |> 
          select(player_id, any_of(anl_cols$stat_cols)),
        by = join_by(player_id),
        relationship = "many-to-many"
      )
  ) 
  # distinct() # putting this here just in case...
  # specifically for the section: filter(game_date >= (cur_date - days(1)))


# PAST
df_past <<- df_fty_roster |> 
  filter(timestamp < force_tz(as.Date(max(timestamp)), tz = "EST")) |> 
  mutate(dow = lubridate::wday(date, week_start = 1), .after = date) |> 
  left_join(
    select(df_nba_schedule, team, game_date, scheduled_to_play),
    by = join_by(player_team == team, date == game_date)
  ) |> 
  select(-c(season_type, begin_date, end_date)) |> 
  rename(game_date = date) |> 
  distinct() 
  # distinct to combat duplication that happens on server ONLY


df_h2h_prepare <<- function(c_id=NULL, exclude=NULL, add=NULL, from_tomorrow=NULL){

  # DELTE
df_past |> 
  filter(player_name == "Aaron Nesmith", league_week == 19) |> 
  glimpse()
  
    
  # TODAY
  df_today <- df_fty_roster |>
    filter(timestamp >= force_tz(as.Date(max(timestamp)), tz = "EST")) |> 
    mutate(dow = lubridate::wday(date, week_start = 1), .after = date) |> 
    left_join(
      select(df_nba_schedule, team, game_date, scheduled_to_play),
      by = join_by(player_team == team, date == game_date)
    ) |> 
    rename(game_date = date) |>
    bind_rows(
      filter(df_nba_player_box_score, player_name %in% add) |>
        slice_max(order_by = game_date, by = player_name) |> 
        select(player_id, player_fantasy_id:=paste0(str_to_lower(platform_selected), "_id"), player_name, player_team = team_slug) |> 
        mutate(season = cur_season, competitor_id = c_id)
    ) |> 
    select(-c(season_type, begin_date, end_date))
  
  
  # FUTURE
  df_future <- distinct(df_nba_schedule, game_date) |>
    filter(game_date> max(df_fty_roster$timestamp)) |> 
    cross_join(select(df_fty_competitor, competitor_id)) |> 
    left_join(
      select(df_today, season, platform, league_id, competitor_id, player_fantasy_id, player_id, player_name, player_team, player_injury_status),
      by = join_by(competitor_id),
      relationship = "many-to-many"
    ) |> 
    left_join(
      select(df_nba_schedule, game_date, team, scheduled_to_play),
      by = join_by(game_date, player_team == team)
    ) |> 
    left_join(
      df_fty_schedule |> 
        select(league_week = week, week_start, week_end, competitor_id, opponent_id),
      by = join_by(competitor_id, between(game_date, week_start, week_end))
    ) |> 
    mutate(dow = lubridate::wday(game_date, week_start = 1)) |> 
    select(any_of(colnames(df_past)))
  

  # COMBINE
  df_h2h <- bind_rows(list(past=df_past, today=df_today, future=df_future), .id = "origin") |> 
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