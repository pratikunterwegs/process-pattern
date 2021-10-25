### Energy and movement

```{r}
# energy and movement per generation
data_energy = Map(
  data, repl, spread,
  f = function(d, repl, spread) {
    d = d[, c("energy", "moved", "gen")]
    d = melt(d, id.vars = "gen")
    d = d[, list(mean = mean(value), sd = sd(value)),
          by = c("gen", "variable")]
    d$repl = repl
    d$spread = spread
    d
  }
)

data_energy = rbindlist(data_energy)
```

```{r}
data_energy = split(data_energy, by = "variable")

plot_energy = Map(
  data_energy, names(data_energy),
  f = function(df, nm) {
    ggplot(df)+
      geom_ribbon(
        aes(
          gen,
          ymin = mean - sd,
          ymax = mean + sd
        ),
        alpha = 0.5,
        fill = "dodgerblue"
      )+
      geom_line(
        aes(
          gen, mean
        )
      )+
      facet_grid(
        repl ~ spread,
        labeller = label_both
      )+
      labs(
        title = nm
      )
  }
)

plot_energy = wrap_plots(
  plot_energy
)+
  plot_annotation(
    tag_levels = "A"
  )

# save figures
ggsave(
  plot_energy,
  filename = "figures/fig_sc_0_energy_metrics.png"
)
```
