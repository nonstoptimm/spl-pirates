#### Simulation of Minimum Wage ####

#Employment effect if Neo Classical Labor market:
# Formula: 1 - (wmin / w)^(-x), with wmin: minimum wage, m:average gross hourly rate and x: labor demand elasticity

x = c(-0.2, -0.5, -0.75, -1, -1.2)
Labor.Demand.Elasticity = x

Means$Neo.Employment.Effect1 = 1 - ((8.5 / Means$avg_Hourly.earnings)^(1 * -0.2))
Means$Neo.Employment.Effect2 = 1 - (8.5 / Means$avg_Hourly.earnings)^(1 * -0.5)
Means$Neo.Employment.Effect3 = 1 - (8.5 / Means$avg_Hourly.earnings)^(1 * -0.75)
Means$Neo.Employment.Effect4 = 1 - (8.5 / Means$avg_Hourly.earnings)^(1 * -1)
Means$Neo.Employment.Effect5 = 1 - (8.5 / Means$avg_Hourly.earnings)^(1 * -1.2)


#Employment effect if Monopsonic Labor Market:
install.packages("starganzer")
library(stargazer)
t(Means)
stargazer(t(Means), title="Descriptive statistics", type = "text", 
          dep.var.labels = c("Employment Status","Full Time", "Part Time", "Marginal", "Unemployed"),
          covariate.labels = c("n()", "mean age", "mean sex", "mean qualification", "mean hourly earning", "mean monthly earning"))
