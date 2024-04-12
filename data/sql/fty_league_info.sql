SELECT DISTINCT
  league_id,
  league_name,
  competitor_id,
  competitor_name
FROM fty.league_info
WHERE season = '{cur_season}'