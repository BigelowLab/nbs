#' Base THREDDS url
#' 
#' @export
#' @param data logical, if TRUE return the thredds database url otherwise
#'   the plain vanilla coastwatch url
nbs_thredds_url = function(data = FALSE){
  if (data){
    url = 'https://www.star.nesdis.noaa.gov/thredds/dodsC'
  } else{
    url = "https://coastwatch.noaa.gov"
  } 
  url
}
#' Query the THREDDS catalog
#' 
#' @export
#' @param period chr, one of "six-hour", "daily" (default) or "monthly"
#' @param dates Date, start and stop dates to query
#' @param product char, one of "uvcomp" (default) or "stress"
#' @param nrt logical, if TRUE query for near real time (default is false)
#' @param base_uri char, the base uri for the thredds catalog
#' @return a tibble with zero or more resources identified
query_nbs = function(period = c("six-hour", "daily", "monthly")[2],
                     dates = c(as.Date("1987-07-09"),as.Date("1987-09-09")),
                     product = c("uvcomp", "stress")[1],
                     nrt = FALSE,
                     base_uri = nbs_thredds_url()){
  
  if (FALSE){
    period = c("six-hour", "daily", "monthly")[1]
    dates = c(as.Date("1987-07-09"), as.Date("1987-09-09"))
    product = c("uvcomp", "stress")[1]
    nrt = FALSE
    base_uri = nbs_thredds_url()
  }
  
  if (period[1] == "sixhour") period[1] = "six-hour"
  
  if (length(dates) == 1) dates = c(dates,dates)
  if (!inherits(dates, "Date")) dates = as.Date(dates)
  
  dates = seq(from = dates[1], to = dates[2], 
              by = if (tolower(period[1]) == "monthly") "month" else "day")
  
  Top = thredds::get_catalog(file.path(base_uri, 
                                       "thredds/socd/coastwatch/catalog_ncei_global_winds.xml"))
  
  SS = Top$list_services()
  SS = SS[grepl("ncdods", names(SS), ignore.case = TRUE)][[1]]
  opendap_path = SS[['base']]
  
  CC = Top$list_catalogs()
  is_nrt = sapply(CC, function(C) grepl("nrt", C[['href']], fixed = TRUE))
  CC = if (nrt) CC[is_nrt] else CC[!is_nrt]
  is_stress = grepl("stress", names(CC), ignore.case = TRUE)
  CC = if(tolower(product[1]) == "stress") CC[is_stress] else CC[!is_stress]
  CC = CC[grepl(period[1], names(CC), ignore.case = TRUE)][[1]]
  
  Cat = thredds::get_catalog(file.path(base_uri,"thredds/socd/coastwatch", CC[['href']]))
  CC = Cat$list_catalogs()
  CC = CC[grepl("per-file", names(CC), ignore.case = TRUE)][[1]]
  
  # note the subtle switch in the path
  # https://coastwatch.noaa.gov/thredds/catalog/uvcompNCEIBlendedGlobalSCIDailyWW00/catalog.xml
  Cat = thredds::get_catalog(paste0(base_uri, CC[['href']]))
  base_url = dirname(Cat$url)
  CC = Cat$list_catalogs()
  
  FMT = ifelse(period == "monthly", "%Y%m", "%Y%m%d")
  x = dplyr::tibble(date = dates, 
                    year  = format(dates, "%Y"),
                    dateid = format(dates, FMT)) |>
    dplyr::group_by(.data$year) |>
    dplyr::group_map(
      function(tbl, key, CC = NULL, base_url = "."){
        # https://coastwatch.noaa.gov/thredds/catalog/uvcompNCEIBlendedGlobalSCIDailyWW00/1987/catalog.html
        C = CC[[key$year]]
        catalog = thredds::get_catalog(file.path(base_url, C[['href']]))
        dd = catalog$list_datasets()
        ss = strsplit(gsub(".nc", "", names(dd), fixed = TRUE), "[_.]")
        len = lengths(ss)
        names(dd) = mapply(function(x, y) x[y], ss, len)
        y = dplyr::tibble(dateid = names(dd),
                          url = file.path(nbs_thredds_url(data = TRUE),
                                          sapply(dd, "[", "urlPath")))
        dplyr::left_join(tbl, y, by = "dateid")
      }, CC = CC, base_url = base_url) |>
    dplyr::bind_rows()
  x                  
}