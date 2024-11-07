SELECT lc.*, l.league_name
FROM fty.league_competitor AS lc
LEFT JOIN fty.league AS l on lc.league_id = l.league_id
WHERE lc.season = '{cur_season}'