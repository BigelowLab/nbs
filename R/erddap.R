# https://coastwatch.noaa.gov/erddap/griddap/documentation.html
# https://coastwatch.noaa.gov/erddap/griddap/noaacwBlendedWindsDaily.nc?windspeed%5B(2023-09-30T18:00:00Z):1:(2023-09-30T18:00:00Z)%5D%5B(10.0):1:(10.0)%5D%5B(20):1:(70)%5D%5B(290):1:(359.75)%5D,mask%5B(2023-09-30T18:00:00Z):1:(2023-09-30T18:00:00Z)%5D%5B(10.0):1:(10.0)%5D%5B(20):1:(70)%5D%5B(290):1:(359.75)%5D,u_wind%5B(2023-09-30T18:00:00Z):1:(2023-09-30T18:00:00Z)%5D%5B(10.0):1:(10.0)%5D%5B(20):1:(70)%5D%5B(290):1:(359.75)%5D,v_wind%5B(2023-09-30T18:00:00Z):1:(2023-09-30T18:00:00Z)%5D%5B(10.0):1:(10.0)%5D%5B(20):1:(70)%5D%5B(290):1:(359.75)%5D
# https://coastwatch.noaa.gov/erddap/griddap/noaacwBlendedWindsMonthly.nc?windspeed%5B(2023-06-01T00:00:00Z):1:(2023-07-15T00:00:00Z)%5D%5B(10.0):1:(10.0)%5D%5B(-89.75):1:(89.75)%5D%5B(0.0):1:(359.75)%5D,mask%5B(2023-06-01T00:00:00Z):1:(2023-07-15T00:00:00Z)%5D%5B(10.0):1:(10.0)%5D%5B(-89.75):1:(89.75)%5D%5B(0.0):1:(359.75)%5D,u_wind%5B(2023-06-01T00:00:00Z):1:(2023-07-15T00:00:00Z)%5D%5B(10.0):1:(10.0)%5D%5B(-89.75):1:(89.75)%5D%5B(0.0):1:(359.75)%5D,v_wind%5B(2023-06-01T00:00:00Z):1:(2023-07-15T00:00:00Z)%5D%5B(10.0):1:(10.0)%5D%5B(-89.75):1:(89.75)%5D%5B(0.0):1:(359.75)%5Dx$
list_bwerddap = function(){
  uri = 'https://coastwatch.noaa.gov/erddap/search/index.html?page=1&itemsPerPage=1000&searchFor=temperature'
}

#' Retrieve the erddap base url (with trailing slash!)
#' 
#' @export
#' @return base URL for erdapp server at CoastWatch central operations
ed_base_url = function(){
  "https://coastwatch.noaa.gov/erddap/"
}

#' Read in a table of griddap products
#' 
#' @seealso [Coastwatch listing page](https://coastwatch.noaa.gov//erddap/search/index.html?page=1&itemsPerPage=1000&searchFor=noaacwBlendedWind)
#' @export
#' @param filename char, the name of the file to read
#' @return tibble
read_noaacwBlendedWind = function(filename = system.file("erddap/noaacwBlendedWind.csv", package = "nbs")){
  readr::read_csv(filename, show_col_types = FALSE)
}


#' Parse a min-max string
#' 
#' @export
#' @param x character in the form of "min, max"
#' @return two element numeric range
parse_range = function(x = "0, 1"){
  strsplit(x, ",", fixed = TRUE)[[1]] |>
    as.numeric()
}

#' Retrieve the time origin 
#' 
#' @param x info class object - see \code{\link[rerddap]{info}}
#' @return POSIXct origin
ed_time_origin = function(x){
  as.POSIXct(x$alldata[["time"]] |>
               dplyr::filter(.data$attribute_name == "time_origin") |>
               dplyr::pull(),
             format = "%d-%B-%Y %H:%M:%S",
             tz = 'UTC')
}


#' Format time for erddap
#'
#' @export 
#' @param x time or Date object
#' @return character time as YYYY-mm-ddTHH:MM:SSZ"
ed_format_time = function(x = Sys.time() |> as.POSIXct(tz = "UTC")){
  format(x, format = "%Y-%m-%dT%H:%M:%SZ")
}


#' Retrieve the range for a dimension or variable
#' 
#' @param x info class object - see \code{\link[rerddap]{info}}
#' @param what char the name of one dimension or variable
#' @return two element vector of min and max
ed_range = function(x, what = 'time'){
  if (!(what %in% names(x$alldata))) stop("name or dimension not found in alldata:", what)
  s = x$alldata[[what]]
  r = dplyr::filter(s, .data$attribute_name == 'actual_range') |>
    dplyr::pull(dplyr::all_of("value")) |>
    parse_range()
  
  if (tolower(what[1]) == 'time'){
    origin = ed_time_origin(x)
    r = r + origin
  } 
  
  r
}


#' Retrieve the known Blended Winds dataset ids
#' 
#' @export
#' @return character vector of Dataset Id values)
ed_dataset_ids = function(){
  c("noaacwBlendednrtWindStress6hr", 
  "noaacwBlendedWindStress6hr", 
  "noaacwBlendednrtWindStressDaily", 
  "noaacwBlendedWindStressDaily", 
  "noaacwBlendedWindStressMonthly", 
  "noaacwBlendednrtWinds6hr", 
  "noaacwBlendedWinds6hr", 
  "noaacwBlendednrtWindsDaily", 
  "noaacwBlendedWindsDaily", 
  "noaacwBlendedWindsMonthly")
}


#' Retrieve a table of access info for one or more griddap datasets
#' 
#' @export
#' @param what character vector of one or more datasets ids or "all"
#'   to retrieve all of them
#' @param url the url for the server
ed_get_datasets = function(what = c("all",ed_dataset_ids()),
                       url = ed_base_url()){
  dd = rerddap::ed_datasets(which = "griddap", url = url)
  if(!("all" %in% what)){
    dd = dplyr::filter(dd, .data$Dataset.ID %in% what)
  }
  dd
}



#' Fetch a griddap resource
#' 
#' @export
#' @param id char specifies one product such as "noaacwBlendedWindsMonthly" (u, v and speed)
#' @param bb named numeric or bbox class provides spatial bounding box
#' @param fields one or more character field names
#' @param times two element Date or POSIXct vector with start and stop
#' @param cleanup logical or NULL.  If TRUE then always cleanup temporary files.
#'   If FALSE never clean up temporary files (which will be cleaned when you
#'   gracefully exix R).  If NULL then only cleanup temporary files if the 
#'   result does not inherit from "stars_proxy".
#' @return stars object
ed_retrieve = function(id = "noaacwBlendedWindsMonthly",
                    fields = NULL,
                    bb = NULL,
                    times = NULL,
                    cleanup = NULL){
  
  if (FALSE){
    id = "noaacwBlendedWindsMonthly"
    fields = NULL
    bb = NULL
    times = NULL
  }
  
  d = ed_get_datasets(what = id)
  if (nrow(d) == 0) stop("No griddap info found for:", id)
  
  X = try(rerddap::info(id, url = ed_base_url()))
  if (inherits(X, "try-error")) stop("unable to get info on: ", id)
  
  
  if (is.null(fields)){
    fields = dplyr::filter(X[["variables"]], .data$data_type == "float") |>
      dplyr::pull(dplyr::all_of("variable_name"))
  }
  
  if (is.null(times)){
    times = ed_range(X, "time")
    if (grepl("6hr", id, ignore.case = TRUE)){
      times = c(times[1], times[1] + 3600 * 5) # the first 6h
    } else if (grepl("Daily", id, ignore.case = TRUE)){
      times = c(times[1], times[1] + 3600 * 23) # the first day
    } else {
      times = c(times[1], times[1] + 24 * 3600 * 1)  # the first month
    }
  } else {
    thetimes = ed_range(X, "time")
    times[1] = if(times[1] < thetimes[1]) times[1] = thetimes[1]
    times[2] = if(times[2] > thetimes[2]) times[2] = thetimes[2]
  }
  
  if (is.null(bb)){
    longitude = ed_range(X, "longitude")
    latitude = ed_range(X, "latitude")
  } else {
    xr = ed_range(X, "longitude")
    yr = ed_range(X, "latitude")
    latitude = bb[c("ymin", "ymax")]
    if (latitude[1] < yr[1]) latitude[1] = yr[1]
    if (latitude[2] > yr[2]) latitude[2] = yr[2]
    longitude = bb[c("xmin", "xmax")]
    if (longitude[1] < xr[1]) longitude[1] = xr[1]
    if (longitude[2] > xr[2]) longitude[2] = xr[2]
  }
  
  r = rerddap::griddap(X,
                         time = ed_format_time(times), 
                         latitude = latitude,
                         longitude = longitude,
                         zlev = ed_range(X, "zlev"),
                         fields = fields,
                         url = X$base_url,
                         fmt = "nc",
                         store = rerddap::disk(tempdir()),
                         read = FALSE)
  
  x = suppressMessages(stars::read_ncdf(r$summary$filename, var = fields))
  if (is.null(cleanup))cleanup = !inherits(x, "stars_proxy")
  if (cleanup) unlink(r$summary$filename)
  x
}

#' Fetch one or more ERDDAP resources
#' 
#' @param ... arguments for 
#' @param path char, the output path
#' @param verbose logical, if TRUE output messages
ed_fetch = function(..., 
                    path = nbs_path("world"),
                    verbose = FALSE){
  
}
