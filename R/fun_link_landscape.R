#' Read landscape data from PNG file.
#'
#' @param landscape_file Path to the landscape file.
#' @param value_name The name to assign to the image data per cell.
#' @param layer Which layer to read. Set by default to 1, the red channel.
#' @return A data.table of X, Y and cell \emph{r} value.
#' @export
read_landscape <- function(landscape_file, layer = 1, 
                           value_name = "cell_r") {
  
  # read the layer-th layer
  land <- png::readPNG(landscape_file)[, , layer]
  
  # make data.table
  land <- data.table::as.data.table(land)
  # set names
  setnames(land, as.character(seq(ncol(land))))
  # add x col
  land$x <- seq(nrow(land))
  # melt and make long
  land <- data.table::melt(land,
                           id.vars = "x",
                           variable.name = "y",
                           value.name = value_name
  )
  
  # make x and y numeric
  # subtract one because positions start at 0
  land[, c("x", "y") := list(
    as.numeric(x) - 1, 
    as.numeric(y) - 1
  )]
  # return
  land
  
}

#' Link position data and landscape data.
#'
#' @param pos_data Position data with x and y coordinates.
#' @param land_data Landscape data with x and y coordinates.
#' @return A data.table with x and y coordinates linked with landscape data.
#' @export
#'
link_landscape = function(pos_data, land_data) {
  data.table::merge.data.table(
    pos_data, land_data, 
    by = intersect(names(pos_data), names(land_data)),
    all.x = TRUE,
    all = F
  )
}

#' Get summary stats on the landscape visited by the agent.
#'
#' @param pos_data Agent position data with x and y cols.
#' @param land_data Landscape data with x and y cols, needs one other col
#' at minimum to give summary stats.
#'
#' @return A data.table of mean and sd of the landscape variables.
#' @export
get_path_env_summary = function(pos_data, land_data) {
  
  # check landscape
  assertthat::assert_that(
    length(colnames(land_data)) >= 3
  )
  
  # get locations visited by agent
  tmp_land_ = data.table::merge.data.table(
    land_data, 
    pos_data[, c("x", "y")], by = c("x", "y")
  )
  
  # melt by cell id to get long df
  tmp_land_ = data.table::melt(tmp_land_, id.vars = c("x", "y"))
  
  # summarise landscape data agnostic to which are present
  land_summary = tmp_land_[, unlist(lapply(.SD, function(v) {
    list(
      mean = mean(v, na.rm = T),
      sd = sd(v, na.rm = T)
    )
  }), recursive = FALSE), by = c("variable"), .SDcols = "value"]
  
  # return land summary
  land_summary
}