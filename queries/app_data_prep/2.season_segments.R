
df_season_segments <<- dh_getQuery(db_con, "season_segments.sql") |> 
  mutate(mid_date = begin_date + (end_date - begin_date) / 2)