#### Simulation of Minimum Wage ####

## Average wages for employees affected by minimum wage for each employment status
Affected.by.minwage = sumsub2013 %>%
  group_by(Employment.Status, Subject.to.minwage) %>%
  summarise(n(),
            avg_Age = mean(Age, na.rm=TRUE), 
            avg_Sex = mean(Sexnum, na.rm=TRUE),
            avg_Hourly.earnings = mean(Hourly.earnings),
            avg_monthly.earnings = mean(Current.Gross.Labor.Income.in.Euro, na.rm = TRUE),
            avg_subject.minwage = mean(Subject.to.minwage)
            )

Affected.by.minwage = select(filter(Affected.by.minwage, Subject.to.minwage == 1), c(Employment.Status ,avg_Hourly.earnings))

#Employment effect if Neo Classical Labor market in %
# Formula: 1 - (wmin / w)^(-x), with wmin: minimum wage, m:average gross hourly rate and x: labor demand elasticity

x = c(-0.2, -0.5, -0.75, -1, -1.2)
Labor.Demand.Elasticity = x

Affected.by.minwage$Neo.Employment.Effect1 = 1 - (8.5 / Affected.by.minwage$avg_Hourly.earnings)^(-1 * -0.2)
Affected.by.minwage$Neo.Employment.Effect2 = 1 - (8.5 / Affected.by.minwage$avg_Hourly.earnings)^(-1 * -0.5)
Affected.by.minwage$Neo.Employment.Effect3 = 1 - (8.5 / Affected.by.minwage$avg_Hourly.earnings)^(-1 * -0.75)
Affected.by.minwage$Neo.Employment.Effect4 = 1 - (8.5 / Affected.by.minwage$avg_Hourly.earnings)^(-1 * -1)
Affected.by.minwage$Neo.Employment.Effect5 = 1 - (8.5 / Affected.by.minwage$avg_Hourly.earnings)^(-1 * -1.2)


#Employment effect if Monopsonic Labor Market:


