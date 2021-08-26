
#' Process config file for type and growth rate.
#'
#' @param path Where to find the config file.
#'
#' @return A list with the growth rate and type.
#' @export
read_config = function(path) {
  config_file = glue::glue("{path}/config.ini")
  config_file = readLines(config_file)
  
  type = config_file[grep("agents.forage", config_file)]
  type = ifelse(stringr::str_extract(type, "\\d{1}") == "0", "exploit", "interf")
  
  growth = config_file[grep("item_growth", config_file)]
  growth = stringr::str_extract(growth, "0\\.\\d+")
  
  rep_ = stringr::str_extract(path, "(\\d{3})")
  
  c(
    growth = growth,
    type = type,
    rep_ = rep_
  )
}
