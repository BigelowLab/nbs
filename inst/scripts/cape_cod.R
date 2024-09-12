suppressPackageStartupMessages({
  library(nbs)
  library(dplyr)
})


cc_study_uris = query_nbs(period = "daily",
                          dates = as.Date(c("2021-01-01", 
                                            "2021-12-31")),
                          product = "uvcomp") 
bb = c(xmin = 289.13, ymin = 41.41, xmax = 290.77, ymax = 42.23)
path = nbs_path("cape_cod")
ok = dir.create(path, recursive = TRUE, showWarnings = FALSE)
dbs = fetch_nbs(cc_study_uris, 
                params = "all", 
                bb = bb, 
                path = path, 
                verbose = TRUE)




db = read_database(path)
db_jan = 
