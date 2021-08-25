
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
pick_alt_steps = function(x, y, x2, y2, landsize = 512, 
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
  
  # bind real steps and mark case
  steps = rbind(steps, list(x2, y2))
  steps$case_ = c(rep(FALSE, n_samples), TRUE)
  
  steps
}

prep_path_ssf = function(data, landsize = 512,
                         chebyshev_distance = 1, n_samples = 8) {
  # get x2 y2
  data[, `:=`(
    x2 = data.table::shift(x, type = "lead"),
    y2 = data.table::shift(y, type = "lead")
  )]
  # remove last as no known end
  data = data[complete.cases(data),]
  
  # select alt steps per coord
  alt_steps = Map(pick_alt_steps, data$x, data$y, data$x2, data$y2, 
      landsize = landsize, chebyshev_distance = chebyshev_distance,
      n_samples = n_samples)
  
  # assign step id
  alt_steps = Map(function(df, sid_) {
    df[, step_id_ := sid_]
  }, alt_steps, seq_len(length(alt_steps)))
  
  # bind list and return
  data.table::rbindlist(alt_steps)
}
