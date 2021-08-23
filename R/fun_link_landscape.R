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
  land[, c("x", "y") := list(
    as.numeric(x),
    as.numeric(y)
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
