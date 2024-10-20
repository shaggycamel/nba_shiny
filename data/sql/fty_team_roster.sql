SELECT 
  roster.season,
  roster.platform,
  roster.league_id,
  roster.timestamp,
  roster.league_week,
  roster.competitor_id,
  competitor.competitor_name,
  roster.player_fantasy_id,
  id_match.nba_id AS player_id,
  roster.player_name,
  roster.player_team,
  roster.player_injury_status,
  roster.player_acquisition_type,
  schedule.opponent_id,
  opponent.competitor_name AS opponent_name

FROM fty.competitor_roster AS roster

LEFT JOIN fty.league_schedule AS schedule ON roster.season = schedule.season
    AND roster.league_id = schedule.league_id
    AND roster.competitor_id = schedule.competitor_id 
    AND roster.league_week = schedule.week
  
LEFT JOIN fty.league_competitor AS competitor ON roster.season = competitor.season
	AND roster.platform = competitor.platform
	AND roster.league_id = competitor.league_id
	AND roster.competitor_id = competitor.competitor_id
	
LEFT JOIN fty.league_competitor AS opponent ON roster.season = opponent.season
	AND roster.platform = opponent.platform
	AND roster.league_id = opponent.league_id
	AND schedule.opponent_id = opponent.competitor_id
    
LEFT JOIN util.nba_fty_name_match AS id_match ON roster.player_fantasy_id = id_match.{platform}_id

WHERE roster.season = '{cur_season}'
  AND roster.platform = '{platform}'
  AND roster.league_id = {league_id}