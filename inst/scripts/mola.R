period = c("six-hour", "daily", "monthly")[3]
dates = c(as.Date("1987-07-09"),as.Date("1987-09-09"))
param = c("uvcomp", "stress")[1]
nrt = FALSE
base_uri = nbs_thredds_url()

# x = query_nbs(period = c("six-hour", "daily", "monthly")[3],
#                 dates = c(as.Date("1987-07-09"),as.Date("1987-09-09")),
#                 param = c("uvcomp", "stress")[1],
#                 nrt = FALSE,
#                 base_uri = nbs_thredds_url())
muri = structure(list(date = structure(c(6398, 6429, 6460), class = "Date"), 
                   yymmdd = c("198707", "198708", "198709"), 
                   url = c("https://www.star.nesdis.noaa.gov/thredds/dodsC/uvcompNCEIBlendedGlobalSCIMonthlyWW00/1987/NBSv02_wind_monthly_198707.nc", 
                           "https://www.star.nesdis.noaa.gov/thredds/dodsC/uvcompNCEIBlendedGlobalSCIMonthlyWW00/1987/NBSv02_wind_monthly_198708.nc", 
                           "https://www.star.nesdis.noaa.gov/thredds/dodsC/uvcompNCEIBlendedGlobalSCIMonthlyWW00/1987/NBSv02_wind_monthly_198709.nc"
                   )), 
              row.names = c(NA, -3L), 
              class = c("tbl_df", "tbl", "data.frame")) |>
  dplyr::pull()

duri = query_nbs(period = c("six-hour", "daily", "monthly")[2],
              dates = c(as.Date("1987-07-09"),as.Date("1987-08-09")),
              param = c("uvcomp", "stress")[1],
              nrt = FALSE,
              base_uri = nbs_thredds_url()) |>
  dplyr::pull()

suri = query_nbs(period = c("six-hour", "daily", "monthly")[1],
                 dates = c(as.Date("1987-07-09"),as.Date("1987-08-09")),
                 param = c("uvcomp", "stress")[1],
                 nrt = FALSE,
                 base_uri = nbs_thredds_url()) |>
  dplyr::pull()

bb = c(xmin = 288, ymin = 39, xmax = 297, ymax = 46)
path = nbs_path("mola")
verbose = TRUE


db = lapply(uri,
  function(u){
    x = ncdf4::nc_open(u)
    nav = nbs_get_nav(x, bb = bb)
    db = nbs_generate_database(x) |>
      
    
    
    
    
  })
