#### tinkering with path metrics ####
library(data.table)
library(ggplot2)
library(glue)
library(stringr)

files = glue("data/{str_pad(seq(0, 190, 10), 5, '0', side = 'left')}_pos.csv")

data = lapply(files, fread)

# # function to find movement at certain sampling scale
# subsampled_movement = function(data, landsize = 512, tscale = 1) {
#   # subsample the data
#   data_ = data[t %% tscale == 0,]
#   
#   # get dx dy
#   data_[, c("dx", "dy") := list(
#     c(0, diff(x)),
#     c(0, diff(y))
#   )]
#   
#   # handle wrapped landscape -- if dx or dy > max movement
#   # max movement is 5 * sqrt(2), or really, tscale
#   data_[, c("dx", "dy") := list(
#     fifelse(abs(dx) > (tscale * sqrt(2)), landsize - (abs(dx)), dx),
#     fifelse(abs(dy) > (tscale * sqrt(2)), landsize - (abs(dy)), dy)
#   )]
#   
#   # make floor as movement is integer
#   data_[, moved := floor(sqrt(dx ^ 2 + dy ^ 2))]
#   
#   # get angle
#   data_[, angle := atan2(dy, dx) * 180 / pi]
#   
#   # get speed
#   data_[, speed := moved / c(0, diff(t))]
#   
#   # scaled movement
#   data_[, c("x_sc", "y_sc") := list(
#     cumsum(dx) + rnorm(length(dx), 0, 0.2),
#     cumsum(dy) + rnorm(length(dy), 0, 0.2)
#   )]
#   
#   data_
# }

# # subsample movement
# data = lapply(data, function(le) {
#   le = split(le, by = "id")
#   le = lapply(le, subsampled_movement, tscale = 5)
#   le = rbindlist(le)
#   le
# })

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
link_landscape(data_, quality_data)

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
