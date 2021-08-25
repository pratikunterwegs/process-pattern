#### tinkering with path metrics ####
library(data.table)
library(ggplot2)
library(glue)
library(stringr)

# source functions
source("R/fun_dist_disp.R")

files = glue("data/{str_pad(seq(50, 190, 10), 5, '0', side = 'left')}_pos.csv")

data = lapply(files, fread)

# get total movement
data = rbindlist(data)
data = data[, list(paths = list(.SD)), by = c("id", "g")]
data[, c("displacement", "distance") := list(
  vapply(paths, get_displacement, FUN.VALUE = 1.0),
  vapply(paths, get_distance, FUN.VALUE = 1L)
)]

# cumulative distance
data[, cdist_data := lapply(
  paths, get_cumulative_distance
)]

# plot histogram of distance and displacement
ggplot(data)+
  geom_histogram(
    aes(
      displacement
    )
  )+
  coord_cartesian(
    # xlim = c(50, 150)
  )+
  facet_wrap(~g)

# plot cumulative distance curves
data_cdist = data[, unlist(cdist_data, recursive = F), by = c("id", "g")]

# plot cumulative distance over time
ggplot(data_cdist[(t > 0) & ((t %% 10 == 0) | (t == 1))])+
  geom_hline(
    yintercept = 200,
    col = 2, lty = 2
  )+
  geom_path(
    aes(t, cuml_dist, group = id),
    alpha = 0.1,
    col = "steelblue"
  )+
  # scale_y_log10()+
  facet_wrap(~g)

# read landscape and link to movement
quality_data <- read_landscape(
  "data/data_parameters/kernels32.png"
)

# test link
link_landscape(data$paths[[6]], quality_data)

# environmental data
# cumulative distance
data[, paths := lapply(
  paths, link_landscape, land_data = quality_data
)]

# add cell r to cumulative distance
data[, cdist_data := Map(function(cd, p) {
  cd$cell_r = p$cell_r
  cd
}, cdist_data, paths)]

# plot cell r over time
data_cdist = data[, unlist(cdist_data, recursive = F), by = c("id", "g")]

# plot cumulative distance over time
ggplot(data_cdist[(t > 0) & ((t %% 10 == 0) | (t == 1))])+
  # geom_hline(
  #   yintercept = 200,
  #   col = 2, lty = 2
  # )+
  geom_path(
    aes(t, cell_r, group = id),
    alpha = 0.1,
    col = "steelblue"
  )+
  # scale_y_log10()+
  facet_wrap(~g)
