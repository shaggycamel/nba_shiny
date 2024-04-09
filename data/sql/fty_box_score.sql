SELECT bs.*, li.competitor_abbrev, li.competitor_name 
FROM fty.matchup_box_score AS bs
LEFT JOIN fty.league_info AS li ON bs.competitor_id = li.competitor_id
	AND bs.league_id = li.league_id
WHERE bs.season = '{cur_season}'
