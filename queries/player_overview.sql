SELECT 
    season_id, 
    stats.player_id,
    id_matchup.fty_id,
    fa.player_status AS free_agent_status,
    info.display_first_last AS player_name,  
    stats.team_abbreviation,
    player_age, 
    min,
    fgm, 
    fga, 
    fg3_m, 
    fg3_a, 
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
    fg_pct, 
    fg3_pct, 
    ft_pct

FROM nba.player_season_stats AS stats
LEFT JOIN util.fty_nba_id_matchup AS id_matchup ON stats.player_id = id_matchup.nba_id 
LEFT JOIN fty.free_agents AS fa ON id_matchup.fty_id = fa.player_id::INT
LEFT JOIN nba.player_info AS info ON stats.player_id = info.person_id

WHERE season_id = '2022-23' 

ORDER BY stats.player_id, season_id  
