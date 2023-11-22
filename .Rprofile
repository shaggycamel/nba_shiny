source("renv/activate.R")

local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cran.rstudio.com/"
  r["shaggycamel"] <- "https://github.com/shaggycamel/nba.dataRub.git"
  options(repos = r)
})

if(Sys.info()[["user"]] == "shiny"){
  Sys.setenv(RETICULATE_PYTHON = "/home/shiny/.virtualenvs/python3_env/bin/python")
} else if(Sys.info()[["user"]] == "fred"){
  Sys.setenv(RETICULATE_PYTHON = "/Users/fred/git/nba_shiny/renv/python/virtualenvs/renv-python-3.11/bin/python")
} else if(Sys.info()[["user"]] == "oli-pi"){
  Sys.setenv(RETICULATE_PYTHON = "/home/oli-pi/git/nba_shiny/renv/python/virtualenvs/renv-python-3.11/bin/python")
}

  
