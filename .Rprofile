source("renv/activate.R")

local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cran.rstudio.com/"
  r["shaggycamel"] <- "https://github.com/shaggycamel/nba.dataRub.git"
  options(repos = r)
})

if(Sys.info()[["user"]] %in% c("fred", "oli-pi")){
  Sys.setenv(RETICULATE_PYTHON = paste0("/Users/", Sys.info()[["user"]], "/git/nba_shiny/renv/python/virtualenvs/renv-python-3.11/bin/python"))
} else{
  Sys.setenv(RETICULATE_PYTHON = "/home/shiny/.virtualenvs/python3_env/bin/python")
}
  
