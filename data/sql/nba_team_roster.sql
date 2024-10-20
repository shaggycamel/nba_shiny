WITH cte_latest_team_roster AS (
  SELECT * 
  FROM (
    SELECT 
      *, 
      ROW_NUMBER() OVER(PARTITION BY season, player_id ORDER BY how_acquired DESC) AS rn 
    FROM nba.team_roster
  ) AS inner_q
  WHERE inner_q.rn = 1
    AND season = '{cur_season}'
)

SELECT
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
  id_match.espn_id, 
  id_match.yahoo_id, 
  how_acquired
FROM cte_latest_team_roster
LEFT JOIN util.nba_fty_name_match AS id_match ON cte_latest_team_roster.player_id = id_match.nba_id 
