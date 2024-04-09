
WITH cte_team_latest_roster AS (
    SELECT * 
    FROM (SELECT *, ROW_NUMBER() OVER(PARTITION BY slug_season, player_id ORDER BY how_acquired DESC) AS rn FROM nba.team_roster) AS inner_q
    WHERE inner_q.rn = 1
        AND slug_season = '{cur_season}'
)



SELECT 
    log.slug_season, 
    year_season,
    season_type,
    RIGHT(log.slug_season, 2) || '-' || season_type AS year_season_type,
    log.player_id,
    id_matchup.fty_id,
    info.display_first_last AS player_name,
    team.team_slug,
    fa.player_status AS free_agent_status,
    game_date, 
    game_id, 
    wl,  
    min,
    fgm, 
    fga, 
    fg_pct, 
    fg3_m, 
    fg3_a, 
    fg3_pct, 
    ft_pct, 
    ftm, 
    fta, 
    oreb, 
    dreb, 
    reb, 
    ast, 
    stl, 
    blk, 
    tov, 
    pf, 
    pts,
    plus_minus
FROM nba.player_game_log AS log
LEFT JOIN cte_team_latest_roster AS team ON log.player_id = team.player_id
LEFT JOIN util.fty_nba_id_matchup AS id_matchup ON log.player_id = id_matchup.nba_id 
LEFT JOIN fty.free_agents AS fa ON id_matchup.fty_id = fa.player_id::INT
LEFT JOIN nba.player_info AS info ON log.player_id = info.person_id
WHERE log.slug_season >= '{prev_season}'
  AND season_type != 'All Star'