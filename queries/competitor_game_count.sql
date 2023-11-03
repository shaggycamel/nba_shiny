
WITH cte_league_schedule_pre AS (
    SELECT 
        game_id, 
        game_date,
        kd.season_type,
        CASE 
            WHEN EXTRACT(week FROM game_date) < 30 THEN EXTRACT(week FROM game_date) + 52
            ELSE EXTRACT(week FROM game_date)          
        END AS league_week,
        RIGHT(slug_matchup, 3) AS team, 
        1 AS playing 
    FROM nba.league_game_schedule AS lgs
    LEFT JOIN util.key_dates AS kd ON lgs.game_date BETWEEN kd.begin_date AND kd.end_date
    WHERE lgs.slug_season = '{cur_season}'
    UNION ALL
    SELECT 
        game_id, 
        game_date,
        kd.season_type,
        CASE 
            WHEN EXTRACT(week FROM game_date) < 30 THEN EXTRACT(week FROM game_date) + 52
            ELSE EXTRACT(week FROM game_date)          
        END AS league_week,
        LEFT(slug_matchup, 3) AS team, 
        1 AS playing 
    FROM nba.league_game_schedule AS lgs
    LEFT JOIN util.key_dates AS kd ON lgs.game_date BETWEEN kd.begin_date AND kd.end_date
    WHERE lgs.slug_season = '{cur_season}'
),


cte_league_schedule AS (
    SELECT 
        game_id,
        game_date,
        season_type,
        CASE
            WHEN season_type = 'Pre Season' THEN 0
            WHEN season_type = 'Regular Season' THEN league_week - (SELECT MAX(league_week) FROM cte_league_schedule_pre WHERE season_type = 'Pre Season')
        END AS league_week,
        team,
        playing
    FROM cte_league_schedule_pre
),


cte_current_past_schedule_roster AS (
    SELECT
        timestamp::DATE - 1 AS us_date,
        roster.league_week, 
        -- To account for the time difference between USA and NZ
        CASE
            WHEN EXTRACT('dow' FROM timestamp::DATE - 1) = 0 THEN 7
            ELSE EXTRACT('dow' FROM timestamp::DATE - 1) 
        END AS dow, 
        competitor_id,
        competitor_name,
        player_fantasy_id,
        player_name,
        player_team,
        player_injury_status,
        cte_league_schedule.playing
    FROM fty.competitor_roster AS roster
    
    INNER JOIN (SELECT MAX(date_trunc('second', timestamp)) FROM fty.competitor_roster GROUP BY timestamp::DATE) AS max_time
        ON date_trunc('second', roster.timestamp) = max_time.max
        
    LEFT JOIN cte_league_schedule ON roster.player_team = cte_league_schedule.team
        AND roster.timestamp::DATE - 1 = cte_league_schedule.game_date
),


cte_future_schedule_roster AS (
    SELECT 
        cte_league_schedule.game_date AS us_date,
        cte_league_schedule.league_week,
        CASE
            WHEN EXTRACT('dow' FROM cte_league_schedule.game_date) = 0 THEN 7
            ELSE EXTRACT('dow' FROM cte_league_schedule.game_date)
        END AS dow, 
        roster.competitor_id,
        roster.competitor_name,
        roster.player_fantasy_id,
        roster.player_name,
        cte_league_schedule.team,
        roster.player_injury_status,
        cte_league_schedule.playing
    FROM cte_league_schedule
    LEFT JOIN (
        SELECT *
        FROM fty.competitor_roster AS roster
        INNER JOIN (
            SELECT MAX(date_trunc('second', timestamp))
            FROM fty.competitor_roster
        ) AS max ON date_trunc('second', roster.timestamp) = max.max
    ) AS roster ON cte_league_schedule.team = roster.player_team
    
    WHERE game_date > (SELECT MAX(us_date) FROM cte_current_past_schedule_roster)
)


SELECT *, 'past'::TEXT AS origin FROM cte_current_past_schedule_roster
UNION ALL 
SELECT *, 'future'::TEXT AS origin FROM cte_future_schedule_roster



