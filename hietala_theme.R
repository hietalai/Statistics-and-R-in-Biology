require(ggplot2)
hietala_theme <- theme_bw() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5, size = 11),
        axis.title.x = element_text(size = 11),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray70"),
        panel.grid.minor.y = element_line(color = "gray80"),
        axis.text = element_text(size = 10, color = "black"))

theme_set(hietala_theme)
# rm(hietala_theme)