SELECT *,
    REGEXP_REPLACE(REPLACE(SLUG_MATCHUP, team, ''), ' @ | vs. ', '', 'gi') AS against
    --EXTRACT(week FROM GAME_DATE) AS season_week
FROM (
    SELECT 
        SLUG_SEASON,
        TYPE_SEASON,
        GAME_ID,
        GAME_DATE,
        SLUG_MATCHUP,
        LEFT(SLUG_MATCHUP, 3) AS team,
        CASE 
            WHEN SLUG_MATCHUP LIKE '%vs%'
            THEN 'home'
            ELSE 'away' 
        END AS home_away
        
    FROM nba.league_game_schedule
    
    UNION ALL
    
    SELECT 
        SLUG_SEASON,
        TYPE_SEASON,
        GAME_ID,
        GAME_DATE,
        SLUG_MATCHUP,
        RIGHT(SLUG_MATCHUP, 3) AS team,
        CASE 
            WHEN SLUG_MATCHUP LIKE '%@%'
            THEN 'home'
            ELSE 'away' 
        END AS home_away
        
    FROM nba.league_game_schedule
) AS home_away_union

LEFT JOIN (
	SELECT DISTINCT 
		week AS fty_matchup_week, 
		week_start, 
		week_end 
	FROM fty.league_schedule
	WHERE season = '{cur_season}'
) AS fty_mw ON home_away_union.game_date BETWEEN fty_mw.week_start AND fty_mw.week_end

WHERE slug_season = '{cur_season}'
--    AND type_season IN ('Pre Season', 'Regular Season')
  AND type_season = 'Regular Season'
    

