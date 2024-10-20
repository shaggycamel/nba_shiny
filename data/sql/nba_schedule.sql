SELECT *,
    REGEXP_REPLACE(REPLACE(slug_matchup, team, ''), ' @ | vs. ', '', 'gi') AS against
    --EXTRACT(week FROM GAME_DATE) AS season_week
FROM (
    SELECT 
        season,
        season_type,
        game_id,
        game_date,
        slug_matchup,
        LEFT(slug_matchup, 3) AS team,
        CASE 
            WHEN slug_matchup LIKE '%vs%'
            THEN 'home'
            ELSE 'away' 
        END AS home_away
        
    FROM nba.league_game_schedule
    
    UNION ALL
    
    SELECT 
        season,
        season_type,
        game_id,
        game_date,
        slug_matchup,
        RIGHT(slug_matchup, 3) AS team,
        CASE 
            WHEN slug_matchup LIKE '%@%'
            THEN 'home'
            ELSE 'away' 
        END AS home_away
        
    FROM nba.league_game_schedule
) AS home_away_union


WHERE season = '{cur_season}'
--    AND type_season IN ('Pre Season', 'Regular Season')
  AND season_type = 'Regular Season'
    

