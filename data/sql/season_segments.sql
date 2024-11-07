SELECT 
    *,
    RIGHT(season , 2) || ' - ' || season_type AS year_season_type
FROM util.key_dates
WHERE season >= '{prev_season}'
    AND season_type != 'All Star'