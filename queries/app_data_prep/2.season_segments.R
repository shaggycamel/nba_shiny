
df_season_segments <<- dh_getQuery(db_con, "season_segments.sql") |> 
  (\(df){
    bind_rows(
      filter(df, Sys.Date() > begin_date, Sys.Date() < end_date) |> 
        mutate(end_date = Sys.Date()),
      
      setdiff(df, filter(df, Sys.Date() > begin_date, Sys.Date() < end_date))
    )
  })() |> 
  mutate(mid_date = begin_date + (end_date - begin_date) / 2)



