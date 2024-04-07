SELECT bs.*, li.competitor_abbrev, li.competitor_name 
FROM fty.matchup_box_score AS bs
LEFT JOIN fty.league_info AS li ON bs.competitor_id = li.competitor_id
	AND bs.league_id = li.league_id
	--AND bs.season = li.season_year 
--	## THERE IS A MISMATCH BETWEEN YEARS HERE
--	## NEED TO FIX NEXT YEAR 
WHERE bs.season = ('20' || RIGHT('{cur_season}', 2))::INT
