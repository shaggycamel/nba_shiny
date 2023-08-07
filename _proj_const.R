
# Project Constants -------------------------------------------------------

# Dataframe used to select NBA stats
stat_selection <- 
  tibble::tribble(
    ~formatted_name, ~database_name,
     "Minutes", "min",
     "3-pointers", "fg3_m",
     "Points", "pts",
     "Field Goal %", "fg_pct",
     "Free Throw %", "ft_pct",
     "Rebounds", "reb",
     "Assists", "ast",
     "Steals", "stl",
     "Blocks", "blk",
     "Turnovers", "tov"
  )
