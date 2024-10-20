SELECT 
	bs.*,
	competitor.competitor_abbrev, 
	competitor.competitor_name 
	
FROM fty.matchup_box_score AS bs

LEFT JOIN fty.league_competitor AS competitor ON bs.competitor_id = competitor.competitor_id
	AND bs.league_id = competitor.league_id
	AND bs.platform = competitor.platform
	AND bs.season = competitor.season

WHERE bs.season = '{cur_season}'
	AND bs.platform = '{platform}'
	AND bs.league_id = {league_id}