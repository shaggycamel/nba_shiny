SELECT 
  roster.season,
  roster.league_id,
  roster.timestamp,
  roster.league_week,
  roster.competitor_id,
  roster.competitor_name,
  roster.player_fantasy_id,
  id_matchup.nba_id AS player_id,
  roster.player_name,
  roster.player_team,
  roster.player_injury_status,
  roster.player_acquisition_type,
  schedule.opponent_id, 
  schedule.opponent_name
FROM fty.competitor_roster AS roster
LEFT JOIN fty.league_schedule AS schedule ON roster.season = schedule.season
    AND roster.league_id = schedule.league_id
    AND roster.competitor_id = schedule.competitor_id 
    AND roster.league_week = schedule.week
LEFT JOIN util.fty_nba_id_matchup AS id_matchup ON roster.player_fantasy_id = id_matchup.fty_id