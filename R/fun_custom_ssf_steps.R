
#' Pick alternate steps for SSF fitting.
#'
#' @param x The current X coordinate.
#' @param y The current Y coordinate.
#' @param x2 The real step end X coordinate.
#' @param y2 The real step end Y coordinate.
#' @param landsize The landscape size for wrapping.
#' @param chebyshev_distance The distance over which movement is possible.
#' @param n_samples How many samples; restricted to 
#' \code{8 * chebyshev_distance}.
#'
#' @return A data.table of alternate steps.
#' @export
#'
pick_alt_steps = function(x, y, x2, y2, landsize, 
                          chebyshev_distance = 1, n_samples = 8) {
  
  assertthat::assert_that(
    n_samples <= (chebyshev_distance * 8),
    msg = "pick_alt_steps: more samples asked than available"
  )
  x_alt = seq(x - chebyshev_distance, 
              x + chebyshev_distance, 1) %% landsize
  y_alt = seq(y - chebyshev_distance, 
              y + chebyshev_distance, 1) %% landsize
  
  steps = data.table::CJ(
    x_ = x_alt,
    y_ = y_alt
  )
  
  # remove actual step destination
  steps = steps[(x_ != x2) | (y_ != y2),]
  
  # sample n_steps if more than n_samples available
  if (nrow(steps) > n_samples) {
    steps = steps[sample.int(n = nrow(steps), size = n_samples, replace = F),]
  }
  
  steps
}
