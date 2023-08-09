SELECT *,
    REGEXP_REPLACE(REPLACE(SLUG_MATCHUP, team, ''), ' @ | vs. ', '', 'gi') AS against,
    EXTRACT(week FROM GAME_DATE) AS season_week
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

WHERE SLUG_SEASON = '{cur_season}'
    AND TYPE_SEASON = 'Regular Season'
    

