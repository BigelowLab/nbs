#' Given one or more filenames, decompose into a database
#'
#' @export
#' @param x char vector of filenames in pattern 'source.param.date.ext'
#' @return tibble database composed of the following fields
#' \itemize{
#' \item{source char the datasetid}
#' \item{date} Date
#' \item{hour char in %H%M%S (HHMMSS) format} 
#' \item{per char period as in one of "sizh", "day", "month"}
#' \item{param char, u_wind, windspeed, etc}
#' \item{nrt logical TRUE is near real-time}
#' }
decompose_filename <- function(x = 
                                 c("/foo/bar/uvcomp_SCIMonthlyGlobal.u_wind.1981-09-01T000000tif",
                                   "/foo/bar/uvcomp_SCIDailyGlobal.v_wind.1981-09-01T000000.tif",
                                   "/foo/bar/stress_SCIDailyGlobal.x_tau.1981-09-01T180000.tif")){
  
  x = gsub(".tif", "", basename(x))
  Choices = c("SixHour", "Daily", "Monthly")
  choices = c("sixh", "day", "month")
  ss = strsplit(x, ".", fixed = TRUE)
  per = sapply(x,
                function(f){
                  ix = sapply(Choices, function(choice) grepl(choice, f, fixed = TRUE))
                  choices[ix]
                }) |>
    unname()
  time = as.POSIXct(sapply(ss, '[[', 3), tz = "UTC", format = "%Y-%m-%dT%H:%M:%S")
  dplyr::tibble(
    source = sapply(ss, '[[', 1),
    date = as.Date(time),
    hour = format(time,"%H%M%S"),
    per = per,
    param = sapply(ss, '[[', 2),
    nrt = grepl("NRT", basename(x), fixed = TRUE))
}


#' Given a database and path, compose filenames
#'
#' @export
#' @param x tibble database
#' @param path char, path description
#' @param ext char the extension for the files
#' @return fully qualified paths
compose_filename <- function(x = decompose_filename(), 
                             path = "/foo/bar",
                             ext = "tif"){
  
  file.path(path,
            format(x$date, "%Y"),
            format(x$date, "%m%d"),
            sprintf("%s.%s.%sT%s.%s",
                    x$source, x$param, format(x$date, "%Y-%m-%d"), x$hour, ext))
  
}


#' Build and optionally save a database
#' 
#' @export
#' @param path char, path to the dataset
#' @param pattern char pattern to search for (as regular expression)
#' @param save_db logical, if TRUE save the database in the specified path as database.csv.gz
build_database <- function(path, 
                           pattern = "^.*\\.tif$",
                           save_db = FALSE){
  
  x <- list.files(path,
                  recursive = TRUE,
                  pattern = pattern) |>
    decompose_filename()
  
  if (save_db) write_database(x, path)
  x
}

#' Add time as a variable to a database
#' 
#' @export
#' @param x tibble database
#' @return tibble database with time added
append_time = function(x){
  dplyr::mutate(x, time = as.POSIXct(paste0(format(.data$date, "%Y-%m-%dT"), .data$hour), 
                                     format = "%Y-%m-%dT%H%M%S", tz = "UTC"))
}

#' Read a NBS database
#' @export
#' @param path char, path description to the database
#' @param filename char, name of the file in the path
#' @param add_time logical, if TRUE add a time column to the database
#' @return tibble
read_database <- function(path, filename = "database.csv.gz",
                          add_time = TRUE){
  x = readr::read_csv(file.path(path, filename),
                  show_col_types = FALSE)
  if (add_time){
    x = append_time(x)
  }
  x
}


#' Write a database
#' @export
#' @param x tibble database
#' @param path char, path description to the database
#' @param filename char, the filename to save to
#' @param drop char one or more column names to drop or "none" to keep them all
#' @return the input database
write_database <- function(x, path, filename = "database.csv.gz",
                           drop = "time"){
  if (!"none" %in% colnames(x)){
    newx = dplyr::select(x, -dplyr::any_of(drop))
    readr::write_csv(newx, file.path(path, filename))
  } else {
    readr::write_csv(x, file.path(path, filename))
  }
  invisible(x)
}