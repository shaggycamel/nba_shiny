
WITH cte_team_latest_roster AS (
    SELECT * 
    FROM (SELECT *, ROW_NUMBER() OVER(PARTITION BY season, player_id ORDER BY how_acquired DESC) AS rn FROM nba.team_roster) AS inner_q
    WHERE inner_q.rn = 1
        AND season = '{cur_season}'
)

SELECT 
	lgs.season, 
	lgs.season_type, -- rename in source table to 'season_type'
	RIGHT(lgs.season, 2) || '-' || lgs.season_type AS year_season_type,
	pbs.game_id,
	lgs.game_date,
	pbs.player_name,
	pbs.player_id,
    id_match.espn_id,
	id_match.yahoo_id,
	team.team_slug,
	pbs.min,
	pbs.fgm,
	pbs.fga,
	pbs.fg_pct,
	pbs.fg3_m,
	pbs.fg3_a,
	pbs.fg3_pct,
	pbs.ftm,
	pbs.fta,
	pbs.ft_pct,
	pbs.pts,
	pbs.oreb,
	pbs.dreb,
	pbs.reb,
	pbs.ast,
	pbs.stl,
	pbs.blk,
	pbs.to, -- rename to tov in source table
	pbs.pf,
	pbs.plus_minus,
	pbs.e_off_rating,
	pbs.off_rating,
	pbs.e_def_rating,
	pbs.def_rating,
	pbs.e_net_rating,
	pbs.net_rating,
	pbs.ast_pct,
	pbs.ast_tov,
	pbs.ast_ratio,
	pbs.oreb_pct,
	pbs.dreb_pct,
	pbs.reb_pct,
	pbs.tm_tov_pct,
	pbs.efg_pct,
	pbs.ts_pct,
	pbs.usg_pct,
	pbs.e_usg_pct,
	pbs.e_pace,
	pbs.pace,
	pbs.pace_per40,
	pbs.poss,
	pbs.pie
	
FROM nba.player_box_score AS pbs
LEFT JOIN nba.league_game_schedule AS lgs ON pbs.game_id = lgs.game_id
LEFT JOIN util.nba_fty_name_match AS id_match ON pbs.player_id = id_match.nba_id 
LEFT JOIN cte_team_latest_roster AS team ON pbs.player_id = team.player_id

WHERE lgs.season >= '{prev_season}'
	AND lgs.season_type != 'All Star'


