# Quantlet 3
# Load Packages used in Q3
library(dplyr)

#### Simulation of Minimum Wage ####
## Function to gather average wages for employees affected by minimum wage for each employment status
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
# Store it as dataframe for later procedure
Affected.by.minwage = as.data.frame(Affected.by.minwage)

#Employment effect if Neo Classical Labor market in %
# Formula: 1 - (wmin / w)^(-x), with wmin: minimum wage, m:average gross hourly rate and x: labor demand elasticity
# Input Data Frame with different values
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

# Apply the employ_effect function for the dataset used before
Affected.by.minwage = employ_effect(Labor.Demand.Elasticity, Affected.by.minwage)

# Employment effect if Monopsonic Labor Market:
# Two alternative formulas, depend on average wage "w" and market power "m"
# If w^min > w(1+0.5m) use: 1 - (w^min / w(1 +m))^(-x)
# If w^min <= w(1+0.5m) use: (w^min - w / 0.5m * w) * ( 1 - (1+0.5 / (1+m))^-x)
# First calculate w(1+0.5m) for each group with m=0.2
# Create Function for it
employ_effect_monopsonic = function(x, y, m) {
  # Create numerated colnames for Mon.Employment.Effect
  new_cols = paste("Mon.Employment.Effect", 1:length(x), sep="")
  # Create dataframe as matrix with number of cols matching to x
  effect_matrix = as.data.frame(matrix(ncol = length(x)))
  # Empty the data frame
  effect_matrix = effect_matrix[FALSE,]
  # Calculate based on Average Hourly Earning, m = Market Power
  y$NewWage = y$avg_Hourly.earnings*(1 + 0.5*m)
  for(lines in 1:nrow(y)) {
  # Assign the values
  if(8.50 > y$NewWage[lines]) {
    #curr_row = y$NewWage[lines] + 1 * x
    curr_row = (1 - (8.5 / y$avg_Hourly.earnings[lines]*(1 + m))^(-x))
    effect_matrix = rbind(effect_matrix, curr_row)
  } else {
    #curr_row = y$NewWage[lines] - y$avg_Hourly.earnings[lines] * x
    curr_row = ((8.5 - y$avg_Hourly.earnings[lines]) / (0.5* m * y$avg_Hourly.earnings[lines])) * (1 -((1 + 0.5) /(1 + y$avg_Hourly.earnings[lines]))^(-1 * x))
    effect_matrix = rbind(effect_matrix, curr_row)
  }
  }
  # Assign names to the data frame
  names(effect_matrix) = new_cols
  output_matrix = cbind(y, effect_matrix)
  return(output_matrix)
}

# Apply the employ_effect_monopsonic function for the dataset used before
Affected.by.minwage = employ_effect_monopsonic(Labor.Demand.Elasticity, Affected.by.minwage, 0.2)