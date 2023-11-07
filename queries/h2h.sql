
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


cte_current_past_schedule_roster_pre AS (
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
        cte_league_schedule.playing,
        'past'::TEXT AS origin
    FROM fty.competitor_roster AS roster
    
    INNER JOIN (SELECT MAX(date_trunc('second', timestamp)) FROM fty.competitor_roster GROUP BY timestamp::DATE) AS max_time
        ON date_trunc('second', roster.timestamp) = max_time.max
        
    LEFT JOIN cte_league_schedule ON roster.player_team = cte_league_schedule.team
        AND roster.timestamp::DATE - 1 = cte_league_schedule.game_date
),


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
        cte_league_schedule.team,
        roster.player_injury_status,
        cte_league_schedule.playing,
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
),


cte_current_past_schedule_roster_stat_roll AS (
    SELECT 
        cpsr_pre.us_date,
        cpsr_pre.league_week, 
        cpsr_pre.dow, 
        cpsr_pre.competitor_id,
        cpsr_pre.competitor_name,
        cpsr_pre.player_fantasy_id,
        mtchup.nba_id,
        AVG(log.min) AS min,
        AVG(log.fgm) AS fgm,
        AVG(log.fga) AS fga,
        AVG(log.fg3_m) AS fg3_m,
        AVG(log.fg3_a) AS fg3_a,
        AVG(log.ftm) AS ftm,
        AVG(log.fta) AS fta,
        AVG(log.oreb) AS oreb,
        AVG(log.dreb) AS dreb,
        AVG(log.reb) AS reb,
        AVG(log.ast) AS ast,
        AVG(log.stl) AS stl,
        AVG(log.blk) AS blk,
        AVG(log.tov) AS tov,
        AVG(log.pf) AS pf,
        AVG(log.pts) AS pts,
        AVG(log.plus_minus) AS plus_minus       
    FROM cte_current_past_schedule_roster_pre AS cpsr_pre
    LEFT JOIN util.fty_nba_id_matchup AS mtchup ON cpsr_pre.player_fantasy_id = mtchup.fty_id
    LEFT JOIN nba.player_game_log AS log ON mtchup.nba_id = log.player_id 
        AND log.game_date > cpsr_pre.us_date - 15 
        AND log.game_date < cpsr_pre.us_date
        AND cpsr_pre.playing = 1
        AND cpsr_pre.player_injury_status IN ('ACTIVE', 'DAY_TO_DAY')
    GROUP BY cpsr_pre.us_date, cpsr_pre.league_week, cpsr_pre.dow, cpsr_pre.competitor_id, cpsr_pre.competitor_name, cpsr_pre.player_fantasy_id, mtchup.nba_id
),


cte_current_past_schedule_roster AS (
    SELECT 
        cpsr_pre.*, 
        cpsr_sr.nba_id, 
        cpsr_sr.min, 
        cpsr_sr.fgm,
        cpsr_sr.fga,
        cpsr_sr.fg3_m,
        cpsr_sr.fg3_a,
        cpsr_sr.ftm,
        cpsr_sr.fta,
        cpsr_sr.oreb,
        cpsr_sr.dreb,
        cpsr_sr.reb,
        cpsr_sr.ast,
        cpsr_sr.stl,
        cpsr_sr.blk,
        cpsr_sr.tov,
        cpsr_sr.pf,
        cpsr_sr.pts,
        cpsr_sr.plus_minus 
    FROM cte_current_past_schedule_roster_pre AS cpsr_pre
    LEFT JOIN cte_current_past_schedule_roster_stat_roll AS cpsr_sr 
        ON cpsr_pre.us_date = cpsr_sr.us_date
        AND cpsr_pre.competitor_id = cpsr_sr.competitor_id
        AND cpsr_pre.player_fantasy_id = cpsr_sr.player_fantasy_id
),


cte_future_schedule_roster AS (
    SELECT 
        fsr_pre.*,
        player_current_stats.nba_id, 
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
        SELECT cpsr_sr.* 
        FROM cte_current_past_schedule_roster_stat_roll AS cpsr_sr
        INNER JOIN (
            SELECT MAX(us_date) AS us_date, player_fantasy_id
            FROM cte_current_past_schedule_roster_stat_roll
            WHERE min IS NOT NULL
            GROUP BY player_fantasy_id
        ) AS player_latest_game ON cpsr_sr.us_date = player_latest_game.us_date 
            AND cpsr_sr.player_fantasy_id = player_latest_game.player_fantasy_id
    ) AS player_current_stats ON fsr_pre.player_fantasy_id = player_current_stats.player_fantasy_id   
)


SELECT * FROM cte_current_past_schedule_roster
UNION ALL 
SELECT * FROM cte_future_schedule_roster
