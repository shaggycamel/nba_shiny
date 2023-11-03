WITH cte_fantasy_rosters AS (
    SELECT base.*, id_matchup.nba_id
    FROM fty.competitor_roster AS base
    INNER JOIN (SELECT MAX(date_trunc('second', timestamp)) FROM fty.competitor_roster) AS max_time
        ON date_trunc('second', base.timestamp) = max_time.max
    LEFT JOIN util.fty_nba_id_matchup AS id_matchup ON base.player_fantasy_id = id_matchup.fty_id
)

SELECT 
    cte_fantasy_rosters.competitor_id,
    cte_fantasy_rosters.competitor_name,
    cte_fantasy_rosters.player_fantasy_id,
    player_stats_two_weeks.player_id AS player_nba_id,
    cte_fantasy_rosters.player_name,
    cte_fantasy_rosters.player_team,
    SUM(player_stats_two_weeks.fgm) AS fgm,
    SUM(player_stats_two_weeks.fga) AS fga,
    SUM(player_stats_two_weeks.ftm) AS ftm,
    SUM(player_stats_two_weeks.fta) AS fta,
    AVG(player_stats_two_weeks.fg3_m) AS fg3_m,
    AVG(player_stats_two_weeks.reb) AS reb,    
    AVG(player_stats_two_weeks.ast) AS ast,
    AVG(player_stats_two_weeks.stl) AS stl, 
    AVG(player_stats_two_weeks.blk) AS blk,
    AVG(player_stats_two_weeks.tov) AS tov,
    AVG(player_stats_two_weeks.pts) AS pts 
FROM cte_fantasy_rosters
INNER JOIN (SELECT * FROM nba.player_game_log WHERE game_date >= '{cur_date}'::DATE - 15) AS player_stats_two_weeks
    ON cte_fantasy_rosters.nba_id = player_stats_two_weeks.player_id
    
GROUP BY cte_fantasy_rosters.competitor_id, cte_fantasy_rosters.competitor_name, cte_fantasy_rosters.player_fantasy_id, player_stats_two_weeks.player_id, cte_fantasy_rosters.player_name, cte_fantasy_rosters.player_team
    

