
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
  
  # which steps to pick
  which_steps = 0
  if (chebyshev_distance == 1) {
     which_steps = seq(8)
  } else {
     which_steps = chebyshev_distance * seq(n_samples)
  }

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
    steps = steps[which_steps,]
  }
  
  # bind real steps and mark case
  steps = rbind(steps, list(x2, y2))
  steps$case_ = c(rep(FALSE, n_samples), TRUE)
  
  steps
}

#' Prepare data for SSF.
#'
#' @param data The data.table with x, y, time columns.
#' @param landsize The landscape size, fixed to 512.
#' @param chebyshev_distance The chebyshev distance.
#' @param n_samples The number of sampled, fixed to 8. Other values are not
#' supported.
#'
#' @return A data.table with selected and alternative steps.
#' @export
prep_path_ssf = function(data, landsize = 512,
                         chebyshev_distance = 1, n_samples = 8) {
  
  assertthat::assert_that(
    n_samples == 8,
    msg = "prep_ssf: only 8 samples supported"
  )
  # set order by time
  setorder(data, t)
  # get x2 y2
  data$x2 = data.table::shift(data$x, type = "lead")
  data$y2 = data.table::shift(data$y, type = "lead")
  # remove last as no known end
  data = data[complete.cases(data),]
  
  # select alt steps per coord
  alt_steps = Map(pick_alt_steps, data$x, data$y, data$x2, data$y2, 
      landsize = landsize, chebyshev_distance = chebyshev_distance,
      n_samples = n_samples)
  
  # assign step id
  alt_steps = Map(function(df, sid_) {
    df$step_id_ = sid_
    df
  }, alt_steps, seq_len(length(alt_steps)))
  
  # bind list and return
  data.table::rbindlist(alt_steps)
}
