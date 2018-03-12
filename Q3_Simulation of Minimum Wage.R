#### Simulation of Minimum Wage ####

## Average wages for employees affected by minimum wage for each employment status
minwage_affect = function(x) { 
  x %>%
  group_by(Employment.Status, Subject.to.minwage) %>%
  summarise(n(),
            avg_Age = mean(Age, na.rm=TRUE), 
            avg_Sex = mean(Sexnum, na.rm=TRUE),
            avg_Hourly.earnings = mean(Hourly.earnings),
            avg_monthly.earnings = mean(Current.Gross.Labor.Income.in.Euro, na.rm = TRUE),
            avg_subject.minwage = mean(Subject.to.minwage)
            )
  }

# Apply Function to create Affected.by.minwage
Affected.by.minwage = minwage_affect(sumsub2013)

# Only Show Average Earnings
Affected.by.minwage = select(filter(Affected.by.minwage, Subject.to.minwage == 1), c(Employment.Status, avg_Hourly.earnings))

#Employment effect if Neo Classical Labor market in %
# Formula: 1 - (wmin / w)^(-x), with wmin: minimum wage, m:average gross hourly rate and x: labor demand elasticity
#Input Data Frame
x = c(-0.2, -0.5, -0.75, -1, -1.2)

# Wozu ist diese da?
Labor.Demand.Elasticity = x

# Create Function to apply the formula with the input data x
employ_effect <- function(x, Affected.by.minwage) {
  i = 1
  for(list in 1:length(x)) {
    curr_col = 1 - (8.5 / Affected.by.minwage$avg_Hourly.earnings)^(-1 * x[i])
    Affected.by.minwage[,paste("Neo.Employment.Effect", i, sep="")] = curr_col
    i = i + 1
  }
  return(Affected.by.minwage)
}

# Apply the Function to the dataset
Affected.by.minwage = employ_effect(x, Affected.by.minwage)

#Employment effect if Monopsonic Labor Market:
# Two alternative formulas, depend on average wage "w" and market power "m"
# If w^min > w(1+0.5m) use: 1 - (w^min / w(1 +m))^(-x)
# If w^min <= w(1+0.5m) use: (w^min - w / 0.5m * w) * ( 1 - (1+0.5 / (1+m))^-x)
# First calculate w(1+0.5m) for each group with m=0.2
# Create Function for it
employ_effect_monopsonic <- function(x, Affected.by.minwage) {
  Affected.by.minwage$NewWage = NA
  Affected.by.minwage$NewWage = Affected.by.minwage$avg_Hourly.earnings*(1 + 0.5*0.2)
  Affected.by.minwage$NewWage < Affected.by.minwage$avg_Hourly.earnings
  i = 1
  for(list in 1:length(x)) {
    curr_col = 1 - (8.5 / Affected.by.minwage$NewWage)^(-1 * x[i])
    Affected.by.minwage[,paste("Mon.Employment.Effect", i, sep="")] = curr_col
    i = i + 1
  }
  return(Affected.by.minwage)
}

# Apply the function
Affected.by.minwage = employ_effect_monopsonic(x, Affected.by.minwage)
