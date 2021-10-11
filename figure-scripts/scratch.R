
## Plot energy over time

```{r}
# plot_intake_gen = lapply(data, function(df) {
#   ggplot(df)+
#     geom_bin2d(
#       aes(
#         g, intake,
#         # group = rep_,
#         fill = ..count../1000
#       ),
#       binwidth = c(10, 1)
#     )+
#     scico::scale_fill_scico(
#       palette = "davos",
#       direction = -1,
#       trans = "sqrt",
#       labels = scales::percent,
#       breaks = c(0.01, 0.15),
#       limits = c(0.005, 0.15),
#       na.value = "transparent"
#     )+
#     theme_test(
#       base_size = 8
#     )+
#     theme(
#       legend.key.width = unit(5, units = "mm"),
#       legend.key.height = unit(1, units = "mm")
#     )+
#     guides(
#       fill = guide_colorbar(
#         title.vjust = 1
#       )
#     )+
#     coord_cartesian(
#       expand = F,
#       xlim = c(-10, 260),
#       ylim = c(0, 50)
#     )+
#     labs(
#       y = "Per capita intake",
#       x = "Generation",
#       fill = "% Agents"
#     )
# })
# 
# plot_intake_gen =
#   wrap_plots(
#   plot_intake_gen, 
#   guides = "collect") +
#   plot_layout(tag_level = "new") & 
#   theme(legend.position = "bottom")
```

## Plot distance moved

```{r}
# plot_dist_gen = lapply(data, function(df) {
#   ggplot(df)+
#     geom_bin2d(
#       aes(
#         g, distance,
#         # group = rep_,
#         fill = ..count../1000
#       ),
#       binwidth = c(10, 5)
#     )+
#     scale_fill_continuous_sequential(
#       palette = "Heat 2",
#       # trans = "sqrt",
#       labels = scales::percent,
#       breaks = c(0.01, 0.10),
#       limits = c(0.005, 0.10),
#       na.value = "transparent"
#     )+
#     theme_test(
#       base_size = 8
#     )+
#     theme(
#       legend.key.width = unit(5, units = "mm"),
#       legend.key.height = unit(1, units = "mm")
#     )+
#     guides(
#       fill = guide_colorbar(
#         title.vjust = 1
#       )
#     )+
#     coord_cartesian(
#       expand = F,
#       xlim = c(-10, 260),
#       ylim = c(0, 400)
#     )+
#     labs(
#       y = "Distance moved",
#       x = "Generation",
#       fill = "% Agents"
#     )
# })
  
# plot_dist_gen =
#   wrap_plots(plot_dist_gen, guides = "collect")+
#   plot_layout(tag_level = "new") &
#     theme(legend.position = "bottom")
```