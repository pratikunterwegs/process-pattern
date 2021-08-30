#### helper functions for Kleptomove ####

#' Get generation data.
#'
#' @param filepath Simulation output path.
#' @param which_gen Which generation to get.
#'
#' @return A \code{generation} object.
#' @export
#'
get_wt_data <- function(filepath,
                        gen_start = 50,
                        gen_end = 250,
                        gen_step = 10,
                        gen_add = 249,
                        n_agents = 100) {

  # get filepath of the function sourceME.r
  toSource <- list.files(
    path = filepath,
    pattern = "sourceMe.R",
    full.names = TRUE
  )

  # source it
  source(toSource, local = T)
  
  # get data using the custom generation function
  tmp_data = lapply(c(seq(gen_start, gen_end, gen_step), gen_add), function(g) {
    
    # generation data
    g_data = generation(g)
    
    # data table of weights
    dt_ = data.table::as.data.table(
      g_data$agents$ann[seq_len(n_agents),]
    )
    
    # change names
    data.table::setnames(dt_, sprintf("wt_%i", seq(ncol(dt_))))
    
    # add fitness
    dt_[, intake := g_data$agents$fit[seq_len(n_agents)]]
    
    # add gen
    dt_[, gen := g]

    # add id
    dt_[, id := seq(nrow(dt_)) - 1]
    
    dt_
    
  })
  
  # bind to list
  tmp_data = data.table::rbindlist(tmp_data)
  
  tmp_data
}

#' Scale weights, TANH transform, cut, and return numeric.
#'
#' @param x The values to handle.
#' @param scale The factor by which to multiply raw weights.
#' @param min_w_val The max value.
#' @param max_w_val The min value.
#' @param steps The steps between max and min values.
#'
#' @return The LOWER value of the cut bins.
#' @export
#'
cut_wt_lower = function(x, scale = 20, 
                        min_w_val = -1.01,
                        max_w_val = 1.01,
                        steps = 50) {
  x = x * scale
  x = tanh(x)
  x_class = cut(
    x, 
    seq(min_w_val, max_w_val, length.out = steps),
    right = TRUE
  )
  x_lower = as.numeric(
    stringi::stri_extract_last(x_class, regex = "[-0-9]+\\.\\d{2}")
  )
  
  x_lower
}
