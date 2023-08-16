#' Convert bounding box (0,360) longitudes to (-180, 180)
#'
#' @description Bounding boxes are 4 element vectors of (xmin, ymin, xmax, ymax)
#'
#' @export
#' @rdname bb_to180
#' @param x numeric bounding box vector, no check is done for being withing -180, 180 range
#' @return `bb_to180` returns a numeric bounding box vector
bb_to180 <- function(x) {
  ix = c(1,3) 
  x[ix] <- to180(x[ix])
  if (identical(x[ix[1]], 180)) x[ix[1]] <- -180  # western edge
  if (identical(x[ix[2]], -180)) x[ix[2]] <- 180  # eastern edge
  x
}

#' @rdname bb_to180
#' @export
#' @param x numeric bounding box vector, no check is done for being withing 0,360 range
#' @return `bb_to360` returns a numeric bounding box vector
bb_to360 <- function(x) {
  ix <- c(1,3)
  x[ix] <- to360(x[ix])
  if (identical(x[ix[1]], 360)) x[ix[1]] <- 0   # western edge
  if (identical(x[ix[2]], 0)) x[ix[2]] <- 360   # eastern edge
  x
}

#' @rdname bb_to180
#' @export
#' @param x numeric vector, no check is done for being withing -180, 180 range
#' @return `to180` returns a numeric vector
to180 <- function(x) {ix <- x > 180 ; x[ix] <- x[ix] - 360; x}

#' @rdname bb_to180
#' @export
#' @param x numeric vector, no check is done for being withing 0,360 range
#' @return `to360` returns a numeric vector
to360 <- function(x) {ix <- x < 0 ; x[ix] <- x[ix] + 360; x}


#' @rdname bb_to180
#' @export
#' @param bb a 4-element numeric vector of (xmin, ymin, xmax, ymax) coordinates
#' @param crs character/numeric, the coordinate reference system
#' @return bb_to_bbox returns a sf bbox object
bb_to_bbox <- function(bb = c(-72, 39, -63, 46),
                       crs = 4326){
  
  sf::st_bbox(c(xmin = bb[1], xmax = bb[3], ymin = bb[2], ymax = bb[4]),
              crs = crs)
}


#' @rdname bb_to180
#' @export
#' @param bb numeric, 4 element bounding box of (xmin, ymin, xmax, ymax) coordinates
#' @param at numeric, longitude to split around
#' @return `bb_split` returns a list of one or two bounding box vectors
bb_split <- function(bb = c(-170, -60, 50,  60),
                     at = 0){
  if (bb_straddles(bb, at = at)){
    x <- list(
      bb1 = c(bb[1], bb[2], at, bb[4]),
      bb2 = c(at, bb[c(2,3,4)]))
  } else {
    x <- list(bb1 = bb)
  }
  x
}

#' @rdname bb_to180
#' @export
#' @param bb numeric, 4 element bounding box of (xmin, ymin, xmax, ymax) coordinates
#' @param at numeric, longitude to test straddle-ness
#' @return `bb_staddles` returns a logical
bb_straddles <- function(bb = c(-170, -60, 50, 60), at = 0){ 
  bb[1] < at && bb[3] > at
}



#' Translate a stars from (0,360) to (-180, 180) 
#'
#' @rdname bb_to180
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


#' Translate a stars from (-180, 180) to (0,360) 
#' 
#' @rdname bb_to180
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

