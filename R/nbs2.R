#### R6 class below

#' Compute a navigation list given a bounding box
#' 
#' @export
#' @param X NBS2 class object
#' @param bb 4 element vector of (xmin, ymin, xmax, ymax) or an sf-like object
#'   from which a bbox can be extracted
#' @param time index, Date or POSIXct single time
#' @param varid char, the name of the variable to extract
NBS2_get_nav = function(X, bb = c(xmin = 288, ymin = 39, xmax = 297, ymax = 46), varid = "u_wind", time = 1){
  
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
#' @param verbose logical, if TRUE output messages
#' @return stars or array object
NBS2_get_var = function(X,
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
  
  is_real_time = inherits(time, c("Date", "POSIXt"), which = TRUE) |>
    as.logical() |>
    any()
  if (is_real_time) time = findInterval(time, X$get_time())
  if (time <= 0) stop("time must be at or later than:", 
                      format(X$get_time()[1], "%Y-%m-%d"))
  if (time > X$NC$dim$time$len) stop("time at or before:", 
                                     format(X$get_time()[X$NC$dim$time$len], "%Y-%m-%d"))
  
  
  if (is.null(nav)) {
    nav = NBS2_get_nav(X, bb = bb, varid = varid, time = time)
  } else {
    nav$start[4] = time
    nav$varid = varid
  }
  
  m <- try(ncdf4::ncvar_get(X$NC, nav$varid,
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
} # NBS2_get_var


#' R6 class for accessing NBS v2 OpenDAP
#'
#' @description R6 class for accessing NBS v2 OpenDAP
#' @export
NBS2 <- R6::R6Class("NBS2",
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
                      
                      #' @description retrieve variable names
                      #' @param exclude NULL or char, one or mor evariables to exclude
                      #' @return charcater vector of available variables
                      get_varnames = function(exclude = c("crs", "mask")){
                        nm = names(self$NC$var)
                        if (!is.null(exclude)) nm = nm[!(nm %in% exclude)]
                        nm
                      }, # get_varnames
                      
                      #' @description generate a likely database
                      #' @return tibble database
                      generate_database = function(){
                        nbs_generate_database(X$NC)
                      },
                      
                      #' @description retrieve a navigation list for extracting rasters
                      #' @param bb 4 element vector of (xmin, ymin, xmax, ymax) or an sf-like object
                      #'   from which a bbox can be extracted
                      #' @param time numeric, Date or POSIXct single time to extract
                      #' @param varid char, the name of the variable to extract
                      get_nav = function(bb = c(xmin = 288, ymin = 39, xmax = 297, ymax = 46), 
                                         varid = "u_wind", 
                                         time = 1){
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
                                         bb =  c(xmin = 288, ymin = 39, xmax = 297, ymax = 46), 
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



