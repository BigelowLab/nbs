#' Set the core data path
#'
#' @export
#' @param path the path that defines the location of the data
#' @param filename the name the file to store the path as a single line of text
#' @return NULL invisibly
set_root_path <- function(path = "/mnt/s1/projects/ecocast/coredata/nbs",
                          filename = "~/.nbsdata"){
  cat(path, sep = "\n", file = filename)
  invisible(NULL)
}

#' Get the core data path from a user specified file
#'
#' @export
#' @param filename the name the file to store the path as a single line of text
#' @return character data path
root_path <- function(filename = "~/.nbsdata"){
  readLines(filename)
}


#' Retrieve a path
#'
#' @export
#' @param ... file segements to add to the root path. See \code{\link[base]{file.path}}
#' @param root the root path to the NBS data
#' @return character path description
nbs_path <- function(..., root = root_path()) {
  file.path(root, ...)
}