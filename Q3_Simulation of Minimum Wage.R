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
Labor.Demand.Elasticity = c(-0.2, -0.5, -0.75, -1, -1.2)

# Create Function to apply the formula with the input data x
employ_effect <- function(x, y) {
  i = 1
  for(list in 1:length(x)) {
    curr_col = 1 - (8.5 / y$avg_Hourly.earnings)^(-1 * x[i])
    y[,paste("Neo.Employment.Effect", i, sep="")] = curr_col
    i = i + 1
  }
  return(y)
}

# Apply the Function to the dataset
Affected.by.minwage = employ_effect(Labor.Demand.Elasticity, Affected.by.minwage)

#Employment effect if Monopsonic Labor Market:
# Two alternative formulas, depend on average wage "w" and market power "m"
# If w^min > w(1+0.5m) use: 1 - (w^min / w(1 +m))^(-x)
# If w^min <= w(1+0.5m) use: (w^min - w / 0.5m * w) * ( 1 - (1+0.5 / (1+m))^-x)
# First calculate w(1+0.5m) for each group with m=0.2
# Create Function for it
employ_effect_monopsonic <- function(x, y) {
  i = 1
  k = 1
  y$NewWage = NA
  y$NewWage = y$avg_Hourly.earnings*(1 + 0.5*0.2)
  
    var1 = function(x) { 1 - (8.5 / y$NewWage[k])^(-1 * x[i]) }
    var2 = function(x) { ((current_New_Wage - y$avg_Hourly.earnings[k]) / (0.5*0.2 * y$avg_Hourly.earnings[k])) * (1 -((1 + 0.5) /(1 + 0.2))^(-1 * x[i])) }
  if(y$NewWage[k] > 8.50) {
    sapply(x, var1)
  } else {
    sapply(x, var2)
  }  
    sapply(x, var2)
      new_cols = paste("Mon.Employment.Effect", 1:length(x), sep="")
  y[new_cols] = NA
  
  for(values in 1:length(x)) {
    print("Hi")
}
    if(y$NewWage[k] > 8.50) {
      lapply (y[k,]$Mon.Employment.Effect[i] = 1 - (8.5 / y$NewWage[k])^(-1 * x[i])
              lapply(1, var1)
    } else {
      y$Mon.Employment.Effect[i] = ((y$NewWage[k] - y$avg_Hourly.earnings[k]) / (0.5*0.2 * y$avg_Hourly.earnings[k])) * (1 -((1 + 0.5) /(1 + 0.2))^(-1 * x[i]))
    }
  
  for(values in 1:length(x)) {
    x$newcol = NA
    y[,paste("Mon.Employment.Effect", i, sep="")] = $newcol
  }
  lapply(1:5, y[,paste("Mon.Employment.Effect", i, sep="")])
  
  #y$NewWage < y$avg_Hourly.earnings
  i = 1
  k = 1
  for (variable in 1:length(y$NewWage)) {
    if(y$NewWage[k] > 8.50) {
      for(list in 1:length(x)) {
        curr_col = 1 - (8.5 / y$NewWage)^(-1 * x[i])
        y[,paste("Mon.Employment.Effect", i, sep="")] = curr_col
        i = i + 1
      }
    } else {
      for(list in 1:length(x)) {
        curr_col = ((y$NewWage - y$avg_Hourly.earnings) / (0.5*0.2 * y$avg_Hourly.earnings)) * (1 -((1 + 0.5) /(1 + 0.2))^(-1 * x[i]))
        y[,paste("Mon.Employment.Effect", i, sep="")] = curr_col
        i = i + 1
      }
    }
    k = k + 1
  }
  return(y)
}

# Apply the function
Affected.by.minwage = employ_effect_monopsonic(Labor.Demand.Elasticity, Affected.by.minwage)
