
# Python initilisation ----------------------------------------------------

packages <- c("nba_api")
  
reticulate::virtualenv_create(envname = "python3_env", python = "/usr/bin/python3")
reticulate::virtualenv_install("python3_env", packages = packages)
reticulate::use_virtualenv("python3_env", required = TRUE)
