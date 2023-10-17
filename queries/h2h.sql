
WITH cte_fantasy_rosters AS (
    SELECT base.*, id_matchup.nba_id, league_info.competitor_name
    FROM fty.competitor_roster AS base
    INNER JOIN (SELECT MAX(timestamp) FROM fty.competitor_roster) AS max_time
        ON base.timestamp = max_time.max
    LEFT JOIN util.fty_nba_id_matchup AS id_matchup ON base.player_fantasy_id = id_matchup.fty_id
    LEFT JOIN fty.league_info AS league_info ON base.competitor_id = league_info.competitor_id
)

SELECT 
    cte_fantasy_rosters.competitor_id,
    cte_fantasy_rosters.competitor_name,
    SUM(player_stats_two_weeks.fgm) / SUM(player_stats_two_weeks.fga) AS fg_pct,
    SUM(player_stats_two_weeks.ftm) / SUM(player_stats_two_weeks.fta) AS ft_pct,
    SUM(player_stats_two_weeks.fg3_m) AS fg3_m,
    SUM(player_stats_two_weeks.reb) AS reb,    
    SUM(player_stats_two_weeks.ast) AS ast,
    SUM(player_stats_two_weeks.stl) AS stl, 
    SUM(player_stats_two_weeks.blk) AS blk,
    SUM(player_stats_two_weeks.tov) AS tov,
    SUM(player_stats_two_weeks.pts) AS pts
FROM cte_fantasy_rosters
LEFT JOIN (
    SELECT
        player_id,
        SUM(fgm) AS fgm,
        SUM(fga) AS fga,
        SUM(ftm) AS ftm,
        SUM(fta) AS fta,
        AVG(fg3_m) AS fg3_m,
        AVG(reb) AS reb,    
        AVG(ast) AS ast,
        AVG(stl) AS stl, 
        AVG(blk) AS blk,
        AVG(tov) AS tov,
        AVG(pts) AS pts 
    FROM nba.player_game_log 
    WHERE game_date >= CURRENT_DATE - 15
    GROUP BY player_id
) AS player_stats_two_weeks ON cte_fantasy_rosters.nba_id = player_stats_two_weeks.player_id
    
GROUP BY cte_fantasy_rosters.competitor_id, cte_fantasy_rosters.competitor_name
