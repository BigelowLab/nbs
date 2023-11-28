#' Read one or more NBS files as stars
#' 
#' @export
#' @param x tibble database of the data elements to read as rasters
#' @param path char, the path to the data storage
#' @return stars object
read_nbs = function(x, path){
  
  ss = x |>
    dplyr::group_by(.data$param) |>
    dplyr::group_map(
      function(tbl, key){
       ff = compose_filename(tbl, path)
       if (nrow(tbl) > 1){
         s = stars::read_stars(ff, along = list(time = tbl$time))
       } else {
         s = stars::read_stars(ff)
       }
       names(s) <- tbl$param[1]
       s
      }, .keep = TRUE )
  if (length(ss) > 1){
    ss = do.call(c, append(ss, list(along = NA_integer_)))
  }
  ss
}