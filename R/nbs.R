#' Retrieve nbs variables
#' 
#' @export
#' @param x ncdf4 object
#' @param drop character items to not list as variables.  Set to 'none' to drop none.
#' @return character vector
nbs_vars <- function(x, drop = c("time_run", "time_offset", "crs", "mask", "latitude",
                                 "longitude", "zlev")){
  if (inherits(x, "ncdf4")){
    x <- names(x$var)
  } else {
    # this is a union of wind and stress, so a jumble
    x <- c("u_wind", "v_wind", "windspeed", "x_tau", "y_tau")
  }
  x[!(x %in% drop)]
}


#' Retrieve the assumed CRS
#' 
#' @export
#' @return CRS as character or numeric
nbs_crs <- function(){
  return("OGC:CRS84")
}

#' Provides a brief tally/overview of available data 
#' 
#' @export
#' @return character vector of catalog names
nbs_tally <- function(){

  base_uri = nbs_thredds_url()
  Top = thredds::get_catalog(file.path(base_uri, 
                                       "thredds/socd/coastwatch/catalog_ncei_global_winds.xml"))
  names(Top$list_catalogs())
}


#' Retrieve the base URL for NBS
#' @export
#' @return character, the base URL
nbs_base_url <- function(){
  "https://www.star.nesdis.noaa.gov/thredds/dodsC/CoastWatch/NCEI/Blended"
}

#' Retrieve a named character vector of paths relative to the root path.
#' Each element is named with the product name
#' 
#' @export
#' @param x missing or character, if missing return all products. If character,
#'   return only those requested
#' @return named character vector
nbs_product <- function(x = NULL){
  p = c(
    "uvcomp_SCISixHourGlobal" = "uvcomp/SCISixHourGlobal/WW00/LoM",
    "stress_SCISixHourGlobal" = "stress/SCISixHourGlobal/WW00/LoM",
    "uvcomp_SCIDailyGlobal" = "uvcomp/SCIDailyGlobal/WW00/LoM",
    "stress_SCIDailyGlobal" = "stress/SCIDailyGlobal/WW00/LoM",
    "uvcomp_SCIMonthlyGlobal" = "uvcomp/SCIMonthlyGlobal/WW00/LoM",
    "stress_SCIMonthlyGlobal" = "stress/SCIMonthlyGlobal/WW00/LoM",
    "uvcomp_NRTSixHourGlobal" = "uvcomp/NRTSixHourGlobal/WW00",
    "stress_NRTSixHourGlobal" = "stress/NRTSixHourGlobal/WW00")
  if (!is.null(x)) p <- p[x]
  p
}

#' Craft a nbs URL for a given date
#' 
#' @export
#' @param product character, the name of the product
#' @param root character, the root URL
#' @return one or more URLs
nbs_product_url <- function(product = c("uvcomp_SCISixHourGlobal", 
                                "stress_SCISixHourGlobal", 
                                "uvcomp_SCIDailyGlobal", 
                                "stress_SCIDailyGlobal", 
                                "uvcomp_SCIMonthlyGlobal", 
                                "stress_SCIMonthlyGlobal", 
                                "uvcomp_NRTSixHourGlobal", 
                                "stress_NRTSixHourGlobal"),
                      root = nbs_base_url()){
  
  file.path(root, nbs_product(product))             
}

#' Generate a database-let from the the filename of the of a ncdf4 object
#' 
#' @export
#' @param x ncdf4 object
#' @return tibble database composed of the following fields
#' \itemize{
#'   \item{source char the datasetid}
#'   \item{date} Date
#'   \item{hour char in %H%M%S (HHMMSS) format} 
#'   \item{per char period as in one of "sizh", "day", "month"}
#'   \item{param char, u_wind, windspeed, etc}
#'   \item{nrt logical TRUE is near real-time}
#' }
nbs_generate_database = function(x){
  # day     uvcompNCEIBlendedGlobalSCIDailyWW00/1987/NBSv02_wind_daily_19870709.nc
  # sixhour uvcompNCEIBlendedGlobalSCISixHourWW00/1987/NBSv02_wind_6hourly_19870709.nc
  # monthly uvcompNCEIBlendedGlobalSCIMonthlyWW00/1987/NBSv02_wind_monthly_198707.nc
  
  # day     stressNCEIBlendedGlobalSCIDailyWW00/1987/NBSv02_wind_stress_daily_19870709.nc
  # sixhour stressNCEIBlendedGlobalSCISixHourWW00/1987/NBSv02_wind_stress_6hourly_19870709.nc
  # monthly stressNCEIBlendedGlobalSCIMonthlyWW00/1987/NBSv02_wind_stress_monthly_198707.nc
  
  # Blended/uvcomp/NRTSixHourGlobal/WW00
  # uvcompNCEIBlendedGlobalNRTSixHourWW00/NBSv02_wind_6hourly_20230101_nrt.nc
  
  prod = c(
    # productname             =  ncdf filename component
    "uvcomp_SCISixHourGlobal" = "uvcompNCEIBlendedGlobalSCISixHourWW00",  
    "stress_SCISixHourGlobal" = "stressNCEIBlendedGlobalSCISixHourWW00",
    "uvcomp_SCIDailyGlobal"   = "uvcompNCEIBlendedGlobalSCIDailyWW00", 
    "stress_SCIDailyGlobal"   = "stressNCEIBlendedGlobalSCIDailyWW00", 
    "uvcomp_SCIMonthlyGlobal" = "uvcompNCEIBlendedGlobalSCIMonthlyWW00", 
    "stress_SCIMonthlyGlobal" = "stressNCEIBlendedGlobalSCIMonthlyWW00", 
    "uvcomp_NRTSixHourGlobal" = "Blended/uvcomp/NRTSixHourGlobal/WW00", 
    "stress_NRTSixHourGlobal" = "Blended/stress/NRTSixHourGlobal/WW00",
    "uvcomp_NRTDailyGlobal"   = "Blended/uvcomp/NRTDailyGlobal/WW00", 
    "stress_NRTDailyGlobal"   = "Blended/stress/NRTDailyGlobal/WW00"
    )
 
  ix = sapply(names(prod),
              function(p) grepl(prod[[p]], x$filename, fixed = TRUE))
  source = names(prod)[ix]
  
  is6h = grepl("SixHour", x$filename, ignore.case = TRUE)
  isday = grepl("Daily", x$filename, ignore.case = TRUE)
  ismonth = grepl("Monthly", x$filename, ignore.case = TRUE)
  isuv = grepl("uvcomp", x$filename, fixed = TRUE)
  nrt = grepl("NRT", x$filename, fixed = TRUE)
 
  time = nbs_time(x)
  
  drop = c("mask", "latitude", "longitude", "lon", "lat", "time", "crs")
  vars = names(x$var)
  param = vars[!(vars %in% drop)]
  
  per = if(is6h){
      "sixhour"
    } else if (isday){
      "day"
    } else {
      "month"
    }
  
 expand.grid(source = source, date = time, 
              hour = "000000",  param = param, per = per, nrt = nrt,
             stringsAsFactors = FALSE) |>
   dplyr::as_tibble() |>
   dplyr::mutate(hour = format(date, "%H%M%S"),
                 date = as.Date(as.character(date)))
}

#' Working with time contents in NBS ncdf4 objects
#' 
#' @description
#' Time may be in days or hours since a common epoch (1978-01-01). `nms_time_unit()`
#' returns a string such as 'days' or 'hours'. `nbs_epoch()` returns a Date or POSIXct 
#' class starting element. `nbs_get_time()` returns all of the time elements as Date
#' or POSIXct. `nbs_trange()` returns a two element vector of start/stop values as
#' Date or POSIXct class.  `nbs_tcount()` returns the number of time steps.
#' 
#' @export
#' @rdname nbs_time
#' @param x ncdf4 object
#' @return nbs_time_units returns a character time unit (days or hours)
nbs_time_units = function(x){
  strsplit(x$dim$time$units, " ")[[1]][1]
}

#' @export
#' @rdname nbs_time
#' @return nbs_epoch returns a single Date or POSIXct starting point from which time is figured
nbs_epoch = function(x){
  
  if (grepl("hours", x$dim$time$units, fixed = TRUE)){
    t0 = as.POSIXct(x$dim$time$units, format = "hours since %Y-%m-%d", tz = 'UTC')
  } else if (grepl("days", x$dim$time$units, fixed = TRUE)){
    t0 = as.Date(x$dim$time$units, format = "days since %Y-%m-%d")
  } else {
    stop("time unit not known:", x$dim$time$units)
  }
  t0
} # nbs_epoch

#' @export
#' @rdname nbs_time
#' @return nbs_time returns a Date or POSIXct vector
nbs_time = function(x){
  # there are some issues with time - it looks like numeric overflow
  # at the source, but I don't have time to investigate
  # here I assume regular intervals from the start
  n = nbs_tcount(x)
  step = switch(nbs_time_units(x),
                "days" = 1,
                "hours" = 60*60)
  
  nbs_epoch(x) + (x$dim$time$vals * step)
} # nbs_time

#' @export
#' @rdname nbs_time
#' @return nbs_trange returns a 2-element vector of Date or POSIXct start and stop values
nbs_trange = function(x){
  range(nbs_time(x))
} # nbs_trange

#' @export
#' @rdname nbs_time
#' @return nbs_tcount returns the number of time steps
nbs_tcount = function(x){
  x$dim$time$len
} # nbs_tcount



#' Retrieve the spatial resolution stated in the global metadata
#' 
#' @export
#' @param x ncdf4 object
#' @return 2 element (lon, lat) resolution vector
nbs_res <- function(x){
  c(0.25, 0.25)
}

#' Retrieve a vector of longitudes  
#' 
#' @export
#' @param x ncdf4 object
#' @return numeric vector  
nbs_lon = function(x){
  x$dim$lon$vals
}

#' Retrieve a vector of latitudes  
#' 
#' @export
#' @param x ncdf4 object
#' @return numeric vector  
nbs_lat= function(x){
  x$dim$lat$vals
}


#' Convert a real time to an index
#' 
#' @export
#' @param x ncdf4 object
#' @param time numeric index, Date or POSIXct 
#' @return index
nbs_which_time = function(x, time = 1){
  is_real_time = inherits(time, c("Date", "POSIXt"), which = TRUE) |>
    as.logical() |>
    any()
  
  times = nbs_time(x)
  if (is_real_time) {
    time = findInterval(time, times)
    if (time <= 0)  stop("time must be at or later than:", 
                       format(times, 
                              ifelse(inherits(times, "Date"), "%Y-%m-%d", "%Y-%m-%dT%H:%M:%S")))
  } else {
    ntimes = length(times)
    if (time <= 0 || time > (ntimes + 1)) {
      stop(sprintf("time as index must be greater than 0 and less than %i", ntimes + 1))
    }
  }
  time
}


#' Compute a navigation list given a bounding box
#' 
#' @export
#' @param x ncfd4 object
#' @param bb 4 element vector of (xmin, ymin, xmax, ymax) or an sf-like object
#'   from which a bbox can be extracted
#' @param time index, Date or POSIXct single time
#' @param varid char, the name of the variable to extract
#' @return navigation list
nbs_get_nav = function(x, 
                       bb = c(xmin = 288, ymin = 39, xmax = 297, ymax = 46), 
                       varid = "u_wind", 
                       time = 1){
  
  if (inherits(bb, "bbox")) bb = as.vector(bb)
  if (!all(c("xmin", "xmax", "ymin", "ymax") %in% names(bb))){
    stop("bb must have named elements: xmin, xmax, ymin and ymax")
  }
  
  stopifnot(varid %in% names(x$var))
  
  time = nbs_which_time(x, time)
  
  res = nbs_res(x)
  r2 = res/2
  lon = nbs_lon(x)
  lat = nbs_lat(x)
  closest_index = function(x, vec){
    which.min(abs(vec-x))
  } 
  
  xi = c("xmin", "xmax")
  yi = c("ymin", "ymax")
  ix = unname(sapply(bb[xi] + c(-r2[1], r2[1]), closest_index, lon))
  nx = ix[2] - ix[1] + 1
  xmin = lon[ix[1]] - r2[1]
  xmax = lon[ix[2]] + r2[1]
  
  iy = unname(sapply(bb[yi] + c(-r2[2], r2[2]), closest_index, lat))
  if (iy[1] >= iy[2]) {
    ny = iy[1] - iy[2] + 1
    ymin = lat[iy[1]] - r2[2]
    ymax = lat[iy[2]] + r2[1]
    iy = rev(iy)
  } else {
    ny = iy[2] - iy[1] + 1
    ymin = lat[iy[1]] - r2[2]
    ymax = lat[iy[2]] + r2[1]
  }
  
  # for bbox we still want [-180,180]
  bbox = c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
  
  list(
    bb = bb,
    varid = varid,
    bbox = sf::st_bbox(bbox, crs = nbs_crs()),
    start = c(ix[1], iy[1], 1, time),
    count = c(nx, ny, 1, 1) )
} # nbs_get_nav


#' Extract an array or stars oject
#' 
#' @export
#' @param x ncdf4 object
#' @param time numeric, Date or POSIXct single time to extract
#' @param bb 4 element vector of (xmin, ymin, xmax, ymax) or an sf-like object
#'   from which a bbox can be extracted
#' @param varid char, the name of the variable to extract. One or more varid 
#'   may be specified for a single time.
#' @param nav NULL or list, if not a list compute the navigation list as needed
#' @param form char, one of 'array' or 'stars' (default) specifying the output type
#' @param verbose logical, if TRUE output messages
#' @return stars or array object
nbs_get_var = function(x,
                        time = 1, 
                        bb = c(xmin = 288, ymin = 39, xmax = 297, ymax = 46), 
                        varid = 'u_wind',
                        nav = NULL,
                        form = c("stars", "array")[1],
                        verbose = FALSE){
  
  if(FALSE){
    time = 1
    bb = c(xmin = 288, ymin = 39, xmax = 297, ymax = 46)
    varid = 'u_wind'
    nav = NULL
    form = c("stars", "array")[1]
  }
  
  time = nbs_which_time(x, time)
  
  if (length(varid) > 1){
    ss = lapply(varid,
        function(v){
          nbs_get_var(x, time = time, bb = bb, varid = v, nav = nav, form = form)
        })
    if (tolower(form[1]) == 'stars'){
      ss = do.call(c, append(ss, list(along = NA_integer_)))
    } else{
      names(ss) = varid
    }
    return(ss)
  }
  
  
  if (is.null(nav)) {
    nav = nbs_get_nav(x, bb = bb, varid = varid, time = time)
  } else {
    nav$start[4] = time[1]
    nav$varid = varid[1]
  }
  
  m <- try(ncdf4::ncvar_get(x, nav$varid,
                            start = nav$start,
                            count = nav$count))
  if (inherits(m, 'try-error')){
    if (verbose) print(nav)
    return(NULL)
  }
  if (tolower(form[1]) == 'array') return(m)
  
  stars::st_as_stars(nav$bbox,
                     values = m,
                     nx = nav$count[1],
                     ny = nav$count[2]) |>
    rlang::set_names(varid) |>
    stars::st_flip("y")
} # nbs_get_var


