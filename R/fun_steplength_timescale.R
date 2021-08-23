
#' Function to get steplengths and turning angles.
#'
#' @param data_ Agent position data with x and y coordinates.
#' @param landsize Landscape size, max cells per side, default is 512.
#' @param tscale The timesteps by which to subsample the data; e.g. 3 returns
#' every third timestep.
#'
#' @return A list with steplengths and turning angles.
#' @export
get_steplengths = function(data_, landsize = 512, tscale = 3) {
  
  data.table::setDT(data_)
  data.table::setorder(data_, t)
  
  # subsample the data
  data_ = data_[t %% tscale == 0,]

  # get dx and dy
  data_[, c("dx", "dy") := list(
    c(0, diff(x)),
    c(0, diff(y))
  )]

  # handle wrapped landscape -- if dx or dy > max movement
  # max movement is tscale * sqrt(2), or really, tscale
  data_[, c("dx", "dy") := list(
    fifelse(abs(dx) > (tscale * sqrt(2)), landsize - (abs(dx)), dx),
    fifelse(abs(dy) > (tscale * sqrt(2)), landsize - (abs(dy)), dy)
  )]
  
  # return list data
  list(
    steplength = floor(sqrt(data_$dx ^ 2 + data_$dy ^ 2)),
    angle = atan2(data_$dy,data_$ dx) * 180 / pi
  )
}

#' Fit a distribution to observed steplengths.
#'
#' @param steplengths A vector of steplengths.
#' @param dist_ The distribution to be used, default is negative binomial.
#'
#' @return The distribution and fit parameters.
#' @export
fit_geom_dist = function(steplengths, dist_ = "nbinom") {
  
  fitdistrplus::fitdist(
    steplengths, distr = dist_
  )
  
}
