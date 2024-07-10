#' Retrieve the current month
#' 
#' @export
#' @param x Date object
#' @param offset numeric, days to offset the current month
#' @return Date class object
current_month = function(x = Sys.Date(), offset = 0){
  x = x |>
    format("%Y-%m-01") |>
    as.Date() 
  x + offset
}

#' Fetch NBS data using THREDDS/OPeNDAP services (preferred)
#' 
#' Data are fetched and written to disk.  You can provide either URLs for 
#' opendap data or parameters to establish a search.
#' 
#' @export
#' @param uri char, one or more OPeNDAP URLs to data to fetch or the output
#'   of \code{\link{query_nbs}}
#' @param ... arguments passed to \code{\link{query_nbs}} if \code{code} is NULL
#' @param params one or more parameters such as "windspeed" o r"x_tau" to download 
#'   (or "all" to get all available for the given product)
#' @param bb numeric, named 4 element numeric of xmin, xmax, ymin, ymax or
#'   a \code{\link[sf]{st_bbox}} class object.
#' @param path output path where to write the data
#' @param verbose logical, if TRUE output helpful messages
#' @return tibble database 
fetch_nbs = function(uri = NULL, 
                     ...,
                     params = "all",
                     bb = c(xmin = 0, ymin = -90, xmax = 360, ymax = 90),
                     path = nbs_path("world"),
                     verbose = FALSE){
  
  if (FALSE){
    uri = query_nbs(period = "six-hour",
                    dates = as.Date(c("1995-01-01", "1995-12-31")),
                    product = "uvcomp")
    bb = c(xmin = 290, ymin = -60, xmax = 360, ymax = 10)
    params = "all"
    path = nbs_path("sio")
  }

  if (is.null(uri)){
    uri = query_nbs(...) |> dplyr::pull()
  } else if (inherits(uri, "data.frame")){
    uri = dplyr::pull(uri, dplyr::all_of("url"))
  }
  
  
  # for each URI
  #  open the connection
  #  generate the database and output filenames
  #  for each parameter extract and save
  # return the database
  
  
  lapply(uri,
    function(u){
      if (verbose) cat("opening", basename(u), "\n")
      x = try(ncdf4::nc_open(u))
      if (inherits(x, "try-error")){
        print(x)
        return(NULL)
      }
      
      if (params == "all"){
        p = nbs_vars(x)
      } else {
        p = nbs_vars(x)
        p = p[p %in% params]
      }
      times = nbs_time(x)
      nav = nbs_get_nav(x, bb = bb)
      db = nbs_generate_database(x) |>
        dplyr::filter(.data$param %in% p) |>
      dplyr::mutate(filename = compose_filename(.data, path)) |>
        dplyr::mutate(time = as.POSIXct(paste(format(.data$date, "%Y-%m-%d"), .data$hour, sep = "T"),
                                        format = "%Y-%m-%dT%H%M%S", tz = "UTC"))
      
      ok = sapply(unique(dirname(db$filename)), dir.create, showWarnings = FALSE, recursive = TRUE)
      
      db = dplyr::rowwise(db) |>
        dplyr::group_map(
          function(tbl, key){
            s = nbs_get_var(x, bb = bb, varid = tbl$param, time = tbl$time) |>
              stars::write_stars(tbl$filename) 
            dplyr::select(tbl, -dplyr::any_of(c("time", "filename")))
          }
        , .keep = TRUE)
      if (verbose) cat("  closing", basename(u), "\n")
      ncdf4::nc_close(x)
      db
    }) |>
    dplyr::bind_rows() |>
    append_time()
  
}
