

dens1 <- Affected.by.minwage %>%
          select(Employment.Status, Neo.Employment.Effect1,Neo.Employment.Effect2,Neo.Employment.Effect3,Neo.Employment.Effect4,Neo.Employment.Effect5)

tdens1 <- as.data.frame(t(dens1))
colnames(tdens1) = c("fulltime","parttime","marginal")
tdens1 <- tdens1[-1,]
tdens1$elast <- c(-0.2, -0.5, -0.75, -1, -1.2)
tdens1$fulltime <- as.numeric(as.vector(tdens1$fulltime))
tdens1$parttime <- as.numeric(as.vector(tdens1$parttime))
tdens1$marginal <- as.numeric(as.vector(tdens1$marginal)) 



ggplot(data = tdens1, aes(x=elast*(-1))) +
  geom_path(aes(y = fulltime*(-1), color ="fulltime")) +
  geom_path(aes(y = parttime*(-1), color ="parttime")) +
  geom_path(aes(y = marginal*(-1), color = "marginal")) +
  theme_classic() +
  labs(title = "Simulation Of Minimum Wage On Employment ",
       x = "Elasticity",
       y = " negative Change in Employment in percent") +
  scale_colour_hue(name = "Employment Status",
                   labels = c("Full time","Part Time", "Marginal", "Unemployed"))

  