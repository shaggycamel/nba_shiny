SELECT 
    *,
    begin_date + (end_date - begin_date) / 2 AS mid_date,
    RIGHT(slug_season , 2) || ' - ' || season_type AS year_season_type
FROM util.key_dates
WHERE year_season >= 2022
    AND season_type != 'All Star'