
ggplot(data = dbys, aes(x = Kaitz, group = Wave, color = Wave )) +
  geom_line(stat = "density") + 
  theme_classic() +
  scale_colour_hue(name = "Years") +
  labs(title = "Density of the Kaitz Index of States seperated by Years ",
       y = "Density",
       x = "Kaitz") +
  coord_cartesian(xlim = c(0.43,0.7))



