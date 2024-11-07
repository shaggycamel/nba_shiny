SELECT * 
FROM fty.free_agents
WHERE season = '{cur_season}'
  AND platform = '{platform}'
  AND league_id = {league_id}