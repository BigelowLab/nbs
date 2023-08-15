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

  
  ss = lapply(xx, function(x) nbs_trange(x) |> as.Date())
  
  tn <- sapply(xx, nbs_tcount)
  ok <- lapply(xx, ncdf4::nc_close)
  
  
  dplyr::tibble(name = names(xx), 
                longname = names(pp), 
                vars = vv,
                time_count = tn,
                start = ss[1,,drop = TRUE],
                end = ss[2,, drop = TRUE])
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
#' @return character time unit (days or hours)
nbs_time_units = function(x){
  strsplit(x$dim$time$units, " ")[[1]][1]
}

#' @rdname nbs_time
#' @return Date or POSIXct start
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


#' @rdname nbs_time
#' @return Date or POSIXct vector
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

#' @rdname nbs_time
#' @return Date or POSIXct start and stop values
nbs_trange = function(x){
  range(nbs_time(x))
} # nbs_trange

#' @rdname nbs_time
#' @return the number of time steps
nbs_tcount = function(x){
  x$dim$time$len
} # nbs_tcount



#' Retrieve the spatial resolution stated in the global metadata
#' 
#' @export
#' @param X ncdf4 object
#' @return 2 element [lon, lat] resolution vector
nbs_res <- function(X){
  c(0.25, 0.25)
}

#### functions above
#### R6 class below


NBS2_get_nav = function(X, bb = c(288, 297, 39, 46), varid = "u_wind"){
  
  stopifnot(varid %in% names(X$NC$var))
  
  res = X$get_res()
  r2 = res/2
  lon = X$get_lon()
  lat = X$get_lat()
  closest_index = function(x, vec){
    which.min(abs(vec-x))
  } 
  
  ix = unname(sapply(bb[1:2] + c(-r2[1], r2[1]), closest_index, lon))
  nx = ix[2] - ix[1] + 1
  xmin = lon[ix[1]] - r2[1]
  xmax = lon[ix[2]] + r2[1]
  
  iy = unname(sapply(bb[3:4] + c(-r2[2], r2[2]), closest_index, lat))
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
  bbox = c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  
  list(
    bb = bb,
    varid = varid,
    bbox = sf::st_bbox(bbox, crs = 4326),
    start = c(ix[1], iy[1], 1, 1),
    count = c(nx, ny, 1, 1) )
} # NBS2_get_nav


NBS2_get_var = function(X,
                        time = 1, 
                        bb = c(288, 297, 39, 46), 
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
  
  is_real_time = inherits(date, c("Date", "POSIXt"), which = TRUE) |>
    as.logical() |>
    any()
  if (is_real_time) time = findInterval(time, X$get_time())
  if (time <= 0) stop("time must be at or later than:", 
                      format(X$get_time()[1], "%y-%m-%d"))
  
  
  if (is.null(nav)) nav = X$get_nav(bb = bb, varid = varid)
  
  nav$start[4] <- time
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


# R6 class for accessing BSW monthly aggregations
NBS2 = R6::R6Class("NBS2",
  public = list(
    product = NULL,
    base_uri= NULL,
    NC = NULL,
    time_unit = NULL, 
                     
    initialize = function(product = "uvcomp_SCIMonthlyGlobal",  
                          base_uri = nbs_base_url()){
      self$product = product[1]
      self$base_uri = base_uri[1]
      self$open_nc()
    },  #init
      
    finalize = function(){
        self$close_nc()
    },
      
    build_uri = function(){
        nbs_product_url(self$product, root = self$base_uri)
    },
      
    close_nc = function(){
      if (inherits(self$NC, "ncdf4")) try(ncdf4::nc_close(self$NC))
      invisible(self)
    },
      
    open_nc = function(){
      uri = self$build_uri()
      self$NC = try(ncdf4::nc_open(uri))
      if (inherits(self$NC, "try-error")) stop("error opening NCDF")
      invisible(self)
    },
      
    get_res = function(){
      atts = ncdf4::ncatt_get(self$NC, 0)
      
      lon = atts[['geospatial_lon_resolution']] |> as.numeric()
      lat = atts[['geospatial_lat_resolution']] |> as.numeric()
      
      c(lon, lat)
    }, # get_res
    
    get_lon = function(){
      self$NC$dim$lon$vals
    },
    
    get_lat = function(){
      self$NC$dim$lat$vals
    },
    
    get_epoch = function(){
      nbs_epoch(self$NC)
    },
    
    get_time = function(){
      nbs_time(self$NC)
    }, # get_time
    
    
    get_nav = function(bb = c(288, 297, 39, 46), varid = "u_wind"){
      NBS2_get_nav(self, bb = bb, varid = varid)
      
    }, # get_nav
    
    
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



