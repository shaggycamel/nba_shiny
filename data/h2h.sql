
-- PRE CALCULATIONS
-- Obtain the league matchup schedule prior to calculating
-- league week correctly
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
--    WHERE lgs.slug_season = '{cur_season}'
    WHERE lgs.slug_season = '2023-24'
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
--    WHERE lgs.slug_season = '{cur_season}'
    WHERE lgs.slug_season = '2023-24'
),


-- POST CALCULATIONS
-- Obtain the league matchup schedule
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
)

SELECT * FROM cte_league_schedule
----- USE DF_SCHEDULE INSTEAD OF THIS SECTION

-- PRE CALCULATIONS
-- Get the past roster of each fantasy competitor's players.
-- This is prior to joining each players average stats.
WITH cte_current_past_schedule_roster_pre AS (
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
--        cte_league_schedule.playing,
        'past'::TEXT AS origin
    FROM fty.competitor_roster AS roster
    
    INNER JOIN (SELECT MAX(date_trunc('second', timestamp)) FROM fty.competitor_roster GROUP BY timestamp::DATE) AS max_time
        ON date_trunc('second', roster.timestamp) = max_time.max
        
--    LEFT JOIN cte_league_schedule ON roster.player_team = cte_league_schedule.team
--        AND roster.timestamp::DATE - 1 = cte_league_schedule.game_date
       
    -- To ensure today's games don't appear as "past"
    WHERE timestamp::DATE < (SELECT MAX(timestamp::DATE) FROM fty.competitor_roster)
),


-- PRE CALCULATIONS
-- Obtain competitors future rosters based on the their latest team
-- configuration in `cte_current_past_schedule_roster_pre`
-- Again, this is prior to joining player stats
cte_future_schedule_roster_pre AS (
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
        cte_league_schedule.team AS player_team,
        roster.player_injury_status,
--        cte_league_schedule.playing,
        'future'::TEXT AS origin
    FROM cte_league_schedule
    LEFT JOIN (
        SELECT *
        FROM fty.competitor_roster AS roster
        INNER JOIN (
            SELECT MAX(date_trunc('second', timestamp))
            FROM fty.competitor_roster
        ) AS max ON date_trunc('second', roster.timestamp) = max.max
    ) AS roster ON cte_league_schedule.team = roster.player_team
    
    WHERE game_date > (SELECT MAX(us_date) FROM cte_current_past_schedule_roster_pre)
)

SELECT * FROM cte_current_past_schedule_roster_pre
UNION ALL 
SELECT * FROM cte_future_schedule_roster_pre


-- Calculates all players rolling averages irrespective of their rostered status
-- this is then joined onto past and future roster obects
cte_rolling_stats AS(
    SELECT 
        base_log.player_id,
        mtchup.fty_id,
        base_log.game_id,
        base_log.game_date, 
        COUNT(roll_log.game_date) AS game_count_in_avg_calc, -- Just for sanity measure
        AVG(roll_log.min) AS min,
        AVG(roll_log.fgm) AS fgm,
        AVG(roll_log.fga) AS fga,
        AVG(roll_log.fg3_m) AS fg3_m,
        AVG(roll_log.fg3_a) AS fg3_a,
        AVG(roll_log.ftm) AS ftm,
        AVG(roll_log.fta) AS fta,
        AVG(roll_log.oreb) AS oreb,
        AVG(roll_log.dreb) AS dreb,
        AVG(roll_log.reb) AS reb,
        AVG(roll_log.ast) AS ast,
        AVG(roll_log.stl) AS stl,
        AVG(roll_log.blk) AS blk,
        AVG(roll_log.tov) AS tov,
        AVG(roll_log.pf) AS pf,
        AVG(roll_log.pts) AS pts,
        AVG(roll_log.plus_minus) AS plus_minus 
    FROM nba.player_game_log AS base_log
    LEFT JOIN util.fty_nba_id_matchup AS mtchup ON base_log.player_id = mtchup.nba_id
    LEFT JOIN nba.player_game_log AS roll_log ON base_log.player_id = roll_log.player_id
        AND base_log.game_date > roll_log.game_date
        AND base_log.game_date - 15 <= roll_log.game_date
    WHERE roll_log.slug_season >= '{prev_season}'
    GROUP BY base_log.player_id, mtchup.fty_id, base_log.game_id, base_log.game_date
),


-- Combine `cte_current_past_schedule_roster_pre` and `cte_rolling_stats`
cte_current_past_schedule_roster AS (
    SELECT 
        cpsr_pre.us_date,
        cpsr_pre.league_week,
        cpsr_pre.dow,
        cpsr_pre.competitor_id,
        cpsr_pre.competitor_name,
        roll_stat.player_id,
        cpsr_pre.player_fantasy_id,
        cpsr_pre.player_name,
        cpsr_pre.player_team,
        cpsr_pre.player_injury_status,
        cpsr_pre.playing,
        cpsr_pre.origin,
        roll_stat.min,
        roll_stat.fgm,
        roll_stat.fga,
        roll_stat.fg3_m,
        roll_stat.fg3_a,
        roll_stat.ftm,
        roll_stat.fta,
        roll_stat.oreb,
        roll_stat.dreb,
        roll_stat.reb,
        roll_stat.ast,
        roll_stat.stl,
        roll_stat.blk,
        roll_stat.tov,
        roll_stat.pf,
        roll_stat.pts,
        roll_stat.plus_minus     
    FROM cte_current_past_schedule_roster_pre AS cpsr_pre
    LEFT JOIN cte_rolling_stats AS roll_stat ON cpsr_pre.player_fantasy_id = roll_stat.fty_id
        AND cpsr_pre.us_date = roll_stat.game_date
        AND cpsr_pre.playing = 1
        AND cpsr_pre.player_injury_status IN ('ACTIVE', 'DAY_TO_DAY')
),


-- Combine `cte_future_schedule_roster_pre` and `cte_rolling_stats`
-- to forecast future averages
cte_future_schedule_roster AS (
    SELECT 
        fsr_pre.us_date,
        fsr_pre.league_week,
        fsr_pre.dow,
        fsr_pre.competitor_id,
        fsr_pre.competitor_name,
        player_current_stats.player_id,
        fsr_pre.player_fantasy_id,
        fsr_pre.player_name,
        fsr_pre.player_team,
        fsr_pre.player_injury_status,
        fsr_pre.playing,
        fsr_pre.origin,
        player_current_stats.min, 
        player_current_stats.fgm,
        player_current_stats.fga,
        player_current_stats.fg3_m,
        player_current_stats.fg3_a,
        player_current_stats.ftm,
        player_current_stats.fta,
        player_current_stats.oreb,
        player_current_stats.dreb,
        player_current_stats.reb,
        player_current_stats.ast,
        player_current_stats.stl,
        player_current_stats.blk,
        player_current_stats.tov,
        player_current_stats.pf,
        player_current_stats.pts,
        player_current_stats.plus_minus 
    FROM cte_future_schedule_roster_pre AS fsr_pre
    LEFT JOIN (
        SELECT cpsr.* 
        FROM cte_current_past_schedule_roster AS cpsr
        INNER JOIN (
            SELECT MAX(game_date) AS game_date, player_id
            FROM cte_rolling_stats
            WHERE min IS NOT NULL
            GROUP BY player_id
        ) AS player_latest_game ON cpsr.us_date = player_latest_game.game_date 
            AND cpsr.player_id = player_latest_game.player_id
    ) AS player_current_stats ON fsr_pre.player_fantasy_id = player_current_stats.player_fantasy_id   
)


-- Combine past and future `schedule_roster` objects
SELECT * FROM cte_current_past_schedule_roster
UNION ALL 
SELECT * FROM cte_future_schedule_roster

