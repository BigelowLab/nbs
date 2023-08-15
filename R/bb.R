#' Convert bounding box [0,360] longitudes to [-180, 180]
#'
#' Bounding boxes are 4 element vectors of [left, right, bottom, top]
#'
#' @export
#' @param x numeric bounding box vector, no check is done for being withing -180, 180 range
#' @return numeric bounding box vector
bb_to180 <- function(x) {
  ix = seq_len(2)  
  x[ix] <- to180(x[ix])
  if (identical(x[ix[1]], 180)) x[ix[1]] <- -180  # western edge
  if (identical(x[ix[2]], -180)) x[ix[2]] <- 180  # eastern edge
  x
}

#' Convert [-180,180] bounding box longitudes to [0,360]
#'
#' Bounding boxes are 4 element vectors of [left, right, bottom, top]
#'
#' @export
#' @param x numeric bounding box vector, no check is done for being withing 0,360 range
#' @return numeric bounding box vector
bb_to360 <- function(x) {
  ix <- seq_len(2)
  x[ix] <- to360(x[ix])
  if (identical(x[ix[1]], 360)) x[ix[1]] <- 0   # western edge
  if (identical(x[ix[2]], 0)) x[ix[2]] <- 360   # eastern edge
  x
}

#' Convert [0,360] longitudes to [-180, 180]
#'
#' @export
#' @param x numeric vector, no check is done for being withing -180, 180 range
#' @return numeric vector
to180 <- function(x) {ix <- x > 180 ; x[ix] <- x[ix] - 360; x}

#' Convert [-180,180] longitudes to [0,360]
#'
#' @export
#' @param x numeric vector, no check is done for being withing 0,360 range
#' @return numeric vector
to360 <- function(x) {ix <- x < 0 ; x[ix] <- x[ix] + 360; x}


#' Convert a 4-element bbox vector to a sf bbox object
#'
#' @export
#' @param bb a 4-element numeric vector of [left, right, bottom, top] coordinates
#' @param crs character/numeric, the coordinate reference system
#' @return sf bbox object
bb_to_bbox <- function(bb = c(-72, -63, 39, 46),
                       crs = 4326){
  
  sf::st_bbox(c(xmin = bb[1], xmax = bb[2], ymin = bb[3], ymax = bb[4]),
              crs = crs)
}


#' Split a bounding box into two at \code{at}
#'
#' @export
#' @param bb numeric, 4 element bounding box of [left, right, bottom, top] coordinates
#' @param at numeric, longitude to split around
#' @return list of one or two bounding box vectors
bb_split <- function(bb = c(-170, 50, -60, 60),
                     at = 0){
  if (bb_straddles(bb, at = at)){
    x <- list(
      bb1 = c(bb[1], at, bb[c(3,4)]),
      bb2 = c(at, bb[c(2,3,4)]))
  } else {
    x <- list(bb1 = bb)
  }
  x
}

#' Test if a bounding box straddles a longitude
#'
#' @export
#' @param bb numeric, 4 element bounding box of [left, right, bottom, top] coordinates
#' @param at numeric, longitude to test straddle-ness
#' @return logical
bb_straddles <- function(bb = c(-170, 50, -60, 60),
                         at = 0){
  bb[1] < at && bb[2] > at
}



#' Translate a stars from [0,360] to [-180, 180] 
#' 
#' @export
#' @param x stars object
#' @return a spatially ranslated version of the input \code{x}
stars_to180 <- function(x){
  
  g = stars::st_dimensions(x)
  x0 = to180(g$x$offset)
  if (identical(x0, 180)) {
    x0 <- -180  # western edge
  } else if (identical(x0, -180)) {
    x0 <- 180  # eastern edge
  }
  g$x$offset <- x0
  stars::st_dimensions(x) <- g
  x
}


#' Translate a stars from [-180, 180] to [0,360] 
#' 
#' @export
#' @param x stars object
#' @return a spatially ranslated version of the input \code{x}
stars_to360 <- function(x){

  g = stars::st_dimensions(x)
  x0 = to360(g$x$offset)
  if (identical(x0, 360)) {
    x0 <- 0   # western edge
  } else if (identical(x0, 0)) {
    x0 <- 360   # eastern edge
  }
  g$x$offset <- x0
  stars::st_dimensions(x) <- g
  
  x
}

