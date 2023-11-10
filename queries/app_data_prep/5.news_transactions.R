
df_news <<- dh_getQuery(db_con, "SELECT * FROM nba.transaction_log WHERE date >= '{cur_date}'::DATE - 15") |>
  mutate(across(c(transaction_type, team, acc_req), as.factor)) |> 
  arrange(desc(date))

