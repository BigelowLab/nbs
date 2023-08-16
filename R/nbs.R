#' Provides a brief tally/overview of available data 
#' 
#' @export
#' @return a tibble of summary information
nbs_tally <- function(){

  pp <- nbs_product(x = NULL)
  xx <- lapply(names(pp), function(p) {
      x = try(ncdf4::nc_open(nbs_product_url(p)), silent = TRUE)
      if (inherits(x,"try-error")){
        warning("unable to open:", p)
        x = NULL
      }
      x
    }  )
  names(xx) <- names(pp)
  ix <- !sapply(xx, is.null)
  xx = xx[ix]
  pp = pp[ix]
  
  vv <- sapply(xx,
               function(x){
                 paste(names(x$var)[-c(1:2)], collapse = ", ")
               })

  
  ss = sapply(xx, function(x) nbs_trange(x) |> as.Date())
  
  tn <- sapply(xx, nbs_tcount)
  ok <- lapply(xx, ncdf4::nc_close)
  
  
  dplyr::tibble(name = names(xx), 
                #longname = names(pp), 
                vars = vv,
                time_count = tn,
                start = ss[1,,drop = TRUE] |> as.Date(origin = as.Date("1970-01-01")),
                end = ss[1,,drop = TRUE] |> as.Date(origin = as.Date("1970-01-01")))
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
                                "stress_NRTSixHourGlobal")[1],
                      root = nbs_base_url()){
  
  file.path(root, nbs_product(product[1]))             
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
#' @param X ncdf4 object
#' @return 2 element (lon, lat) resolution vector
nbs_res <- function(X){
  c(0.25, 0.25)
}

#### functions above
#### R6 class below

#' Compute a navigation list given a bounding box
#' 
#' @export
#' @param X NBS2 class object
#' @param bb 4 element vector of (xmin, ymin, xmax, ymax) or an sf-like object
#'   from which a bbox can be extracted
#' @param time index, Date or POSIXct single time
#' @param varid char, the name of the variable to extract
NBS2_get_nav = function(X, bb = c(288, 297, 39, 46), varid = "u_wind", time = 1){
  
  stopifnot(varid %in% names(X$NC$var))
  
  is_real_time = inherits(time, c("Date", "POSIXt"), which = TRUE) |>
    as.logical() |>
    any()
  if (is_real_time) time = findInterval(time, X$get_time())
  if (time <= 0) stop("time must be at or later than:", 
                      format(X$get_time()[1], "%Y-%m-%d"))
  
  if (inherits(bb, "bbox")) bb = as.vector(bb)
  
  res = X$get_res()
  r2 = res/2
  lon = X$get_lon()
  lat = X$get_lat()
  closest_index = function(x, vec){
    which.min(abs(vec-x))
  } 
  
  xi = c(1,3)
  yi = c(2,4)
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
    bbox = sf::st_bbox(bbox, crs = 4326),
    start = c(ix[1], iy[1], 1, time),
    count = c(nx, ny, 1, 1) )
} # NBS2_get_nav

#' Extract an array or stars oject
#' 
#' @export
#' @param X NBS2 class object
#' @param time numeric, Date or POSIXct single time to extract
#' @param bb 4 element vector of (xmin, ymin, xmax, ymax) or an sf-like object
#'   from which a bbox can be extracted
#' @param varid char, the name of the variable to extract
#' @param nav NULL or list, if not a list compute the navigation list as needed
#' @param form char, one of 'array' or 'stars' (default) specifying the output type
#' @return stars or array object
NBS2_get_var = function(X,
                        time = 1, 
                        bb = c(288, 39, 297, 46), 
                        varid = 'u_wind',
                        nav = NULL,
                        form = c("stars", "array")[1]){
  
  if(FALSE){
    time = 1
    bbox = c(288, 297, 39, 46)
    varid = 'u_wind'
    nav = NULL
    form = c("stars", "array")[1]
  }
  
  is_real_time = inherits(time, c("Date", "POSIXt"), which = TRUE) |>
    as.logical() |>
    any()
  if (is_real_time) time = findInterval(time, X$get_time())
  if (time <= 0) stop("time must be at or later than:", 
                      format(X$get_time()[1], "%Y-%m-%d"))
  
  
  if (is.null(nav)) nav = NBS2_get_nav(X, bb = bb, varid = varid, time = time)
  
  #nav$start[4] <- time
  m <- ncdf4::ncvar_get(X$NC, nav$varid,
                        start = nav$start,
                        count = nav$count)
  if (tolower(form[1]) == 'array') return(m)
  
  stars::st_as_stars(nav$bbox,
                     values = m,
                     nx = nav$count[1],
                     ny = nav$count[2]) |>
    rlang::set_names(varid) |>
    stars::st_flip("y")
} # NBS2_get_var


#' R6 class for accessing NBS v2 OpenDAP
#'
#' @description R6 class for accessing NBS v2 OpenDAP
#' @export
NBS2 = R6::R6Class("NBS2",
  public = list(
    #' @field product char, the product name (see [nbs_tally])
    product = NULL,
    #' @field base_uri char, the base uri (see [nbs_base_url])
    base_uri= NULL,
    #' @field NC ncdf4 object
    NC = NULL,
    
    #' @description
    #' Create a new NBS2 object.
    #' @param product char, the product name (see [nbs_tally])
    #' @param base_uri char, the base uri (see [nbs_base_url])
    #' @return A new `NBS2` object with an opened ncdf4 object.                 
    initialize = function(product = "uvcomp_SCIMonthlyGlobal",  
                          base_uri = nbs_base_url()){
      self$product = product[1]
      self$base_uri = base_uri[1]
      self$open_nc()
    },  #init
    
    #' @description
    #' Cleanup a NBS2 object
    finalize = function(){
        self$close_nc()
    },
    
    #' @description build a URL for the resource
    build_uri = function(){
        nbs_product_url(self$product, root = self$base_uri)
    },
      
    #' @description close the ncdf4 object
    close_nc = function(){
      if (inherits(self$NC, "ncdf4")) try(ncdf4::nc_close(self$NC))
      invisible(self)
    },
      
    #' @description open the ncdf4 connection
    open_nc = function(){
      uri = self$build_uri()
      self$NC = try(ncdf4::nc_open(uri))
      if (inherits(self$NC, "try-error")) stop("error opening NCDF")
      invisible(self)
    },
      
    #' @description retrieve the spatial resolution
    #' @return two element numeric vector of (res_x, res_y)
    get_res = function(){
      atts = ncdf4::ncatt_get(self$NC, 0)
      
      lon = atts[['geospatial_lon_resolution']] |> as.numeric()
      lat = atts[['geospatial_lat_resolution']] |> as.numeric()
      
      c(lon, lat)
    }, # get_res

    #' @description retrieve a vector of longitudes  
    #' @return numeric vector  
    get_lon = function(){
      self$NC$dim$lon$vals
    },
    
    #' @description retrieve a vector of latitudes  
    #' @return numeric vector
    get_lat = function(){
      self$NC$dim$lat$vals
    },
    
    #' @description retrieve the time origin
    #' @return Date or POSIXct class element
    get_epoch = function(){
      nbs_epoch(self$NC)
    },
    
    #' @description retrieve the time vector
    #' @return Date or POSIXct class vector 
    get_time = function(){
      nbs_time(self$NC)
    }, # get_time
    
    
    #' @description retrieve a navigation list for extracting rasters
    #' @param bb 4 element vector of (xmin, ymin, xmax, ymax) or an sf-like object
    #'   from which a bbox can be extracted
    #' @param time numeric, Date or POSIXct single time to extract
    #' @param varid char, the name of the variable to extract
    get_nav = function(bb = c(288, 297, 39, 46), varid = "u_wind", time = 1){
      NBS2_get_nav(self, bb = bb, varid = varid, time = time)
      
    }, # get_nav
    
    #' @description Extract an array or stars oject
    #' @param time numeric, Date or POSIXct single time to extract
    #' @param bb 4 element vector of (xmin, ymin, xmax, ymax) or an sf-like object
    #'   from which a bbox can be extracted
    #' @param varid char, the name of the variable to extract
    #' @param nav NULL or list, if not a list compute the navigation list as needed
    #' @param form char, one of 'array' or 'stars' (default) specifying the output type
    #' @return stars or array object
    get_var = function(time = 1, 
                       bb = c(288, 297, 39, 46), 
                       varid = 'u_wind',
                       nav = NULL,
                       form = c("stars", "array")[1]){
      NBS2_get_var(self, 
                   time = time,
                   bb = bb,
                   varid = varid,
                   nav = nav,
                   form = form)
    } # get_var
    
) # public
    
    
)# NBS2



