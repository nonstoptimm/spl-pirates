#Generate graphic for kaitz over time

# install.packages("ggplot2")
library("ggplot2")


ggplot(data = dbys, aes(x= Wave, y = Kaitz, color = State.of.Residence, group = State.of.Residence)) +
  geom_line() +
  theme(panel.background = element_rect(fill = "white")) +
  labs(title = "Kaitz Index over Years",
       y = "Kaitz-Index",
       x = "Years") +
  geom_vline(xintercept = 6, color = "red") +
  theme_classic() +
  coord_cartesian(xlim = c(1.6,7)) +
  scale_colour_hue(name = "States",
                      labels = c("Schleswig-Holstein", 
                                 "Hamburg", 
                                 "Lower Saxony", 
                                 "Bremen", 
                                 "North-RhineWestfalia", 
                                 "Hessen", 
                                 "Rheinland-Pfalz", 
                                 "Baden-Wuerttemberg", 
                                 "Bavaria", 
                                 "Saarland", 
                                 "Berlin", 
                                 "Brandenburg", 
                                 "Mecklemburg-Vorpommern", 
                                 "Saxony", 
                                 "Saxony-Anhalt", 
                                 "Thuringia")) 
