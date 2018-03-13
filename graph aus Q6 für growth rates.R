########## NOT USED ########## graph growth rates both treatments 
ggplot(data = Treatment.analysis1, aes(x=Wave, y = Avg.Delta.Log.Full.Employment, group = factor(binary_treatment1), colour = factor(binary_treatment1))) +
  geom_line() +
  geom_point() +
  labs(title = "Growth Rate of Employment of binary Treatmentgroups",
       y = "Growth Rate",
       x = "Years") +
  geom_vline(xintercept = 5, color = "red") +
  theme_classic() +
  scale_colour_discrete(name = "Treatment",
                        labels = c("lower Kaitz than median", "higher Kaitz than median")) +
  coord_cartesian(xlim = c(1.6,7), ylim = c(-0.50,0.50))





ggplot(data = Treatment.analysis2, aes(x=Wave, y = Avg.Delta.Log.Full.Employment, group = factor(binary_treatment2), colour = factor(binary_treatment2))) +
  geom_line() +
  geom_point() +
  labs(title = "Growth Rate of Employment of binary Treatmentgroups",
       y = "Growth Rate",
       x = "Years") +
  geom_vline(xintercept = 5, color = "red") +
  theme_classic() +
  scale_colour_discrete(name = "Treatment",
                        labels = c("lower Kaitz than median", "higher Kaitz than median")) +
  coord_cartesian(xlim = c(1.6,7), ylim = c(-0.51,1.62))
