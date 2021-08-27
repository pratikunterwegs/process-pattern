#' Find total path distance.
#'
#' @param data_ A data.table with x and y coordinates, arranged by time.
#' @param landsize The landscape size in grid cells per side.
#'
#' @return The total path distance.
#' @export
#' 
get_distance = function(data_, landsize = 512) {
  
  data.table::setDT(data_)
  data.table::setorder(data_, t)
  # get dx dy
  data_[, c("dx", "dy") := list(
    c(0, diff(x)),
    c(0, diff(y))
  )]
  
  # any change in x or y is one unit distance
  total_distance = sum((abs(data_$dx) > 0) | (abs(data_$dy) > 0))
  
  total_distance
}

#' Find path displacement.
#'
#' @param data_ A data.frame or data.table with x and y coordinates, arranged
#' by time.
#' @param landsize The landscape size in grid cells per side.
#'
#' @return The linear distance between the first and last coordinate.
#' @export
#' 
get_displacement = function(data_, landsize = 512) {
  data.table::setDT(data_)
  data.table::setorder(data_, t)
  last_ = as.integer(floor(nrow(data_)))
  
  x1 = data_$x[1]
  x2 = data_$x[last_]
  y1 = data_$y[1]
  y2 = data_$y[last_]
  
  dx = data.table::fifelse(abs(x2 - x1) > (landsize / 2), 
                           (landsize - abs(x2 - x1)), (abs(x2 - x1)))
  dy = data.table::fifelse(abs(y2 - y1) > (landsize / 2), 
                           (landsize - abs(y2 - y1)), (abs(y2 - y1)))
  
  displacement = sqrt(dx^2 + dy^2)
  displacement
}

#' Get cumulative distance
#'
#' @param data_ A data.frame or data.table with x and y coordinates, arranged
#' by time.
#' @param landsize The landscape size in grid cells per side.
#'
#' @return A vector of cumulative distances over time.
#' @export
#' 
get_cumulative_distance = function(data_, landsize = 512) {
  
  data.table::setDT(data_)
  
  # get dx dy
  data_[, c("dx", "dy") := list(
    c(0, diff(x)),
    c(0, diff(y))
  )]
  
  # any change in x or y is one unit distance
  # TRUE converts to 1L, FALSE to 0L
  cumulative_distance = cumsum(as.integer(abs(data_$dx) > 0) | 
                                 (abs(data_$dy) > 0))
  data.table(
    cuml_dist = cumulative_distance,
    t = data_$t
  )
}

#' Get the slope of cumulative distance over time.
#'
#' @param data_ A datatable of cumulative distances and timestamps,
#' called \code{cuml\_dist} and \code{t}.
#'
#' @return An \code{lm} fit.
#' @export
get_avg_speed = function(data_) {
  stats::coef(
    stats::lm(cuml_dist ~ t, data = data_)
  )
}

#' Scale and wrap coordinates.
#'
#' @param v The coordinate vector.
#'
#' @return Wrapped, scaled coordinates.
#' @export
scale_wrap_coords = function(v) {
  v = c(0, diff(v))
  v[v < -10] = 1
  v[v > 10] = -1
  cumsum(v)
}
