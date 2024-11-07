SELECT * 
FROM fty.league_schedule
WHERE season = '{cur_season}'
  AND platform = '{platform}'
  AND league_id = {league_id}