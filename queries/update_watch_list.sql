INSERT INTO fty.watch_list (nba_id, fty_id, nba_name)
SELECT nba_id, fty_id, nba_name
FROM util.fty_nba_id_matchup
WHERE nba_name IN ('{watched}')
  AND nba_id IS NOT NULL
