WITH cte_latest_team_roster AS (
  SELECT * 
  FROM (
    SELECT 
      *, 
      ROW_NUMBER() OVER(PARTITION BY slug_season, player_id ORDER BY how_acquired DESC) AS rn 
    FROM nba.team_roster
  ) AS inner_q
  WHERE inner_q.rn = 1
    AND slug_season = '{cur_season}'
)

SELECT
  slug_season,
  season,
  team_slug,
  team_id,
  player,
  nickname,
  player_slug,
  num,
  position,
  height_cm,
  weight_kg,
  birth_date,
  age,
  exp,
  school,
  player_id,
  id_matchup.fty_id, 
  how_acquired
FROM cte_latest_team_roster
LEFT JOIN util.fty_nba_id_matchup AS id_matchup ON cte_latest_team_roster.player_id = id_matchup.nba_id 
