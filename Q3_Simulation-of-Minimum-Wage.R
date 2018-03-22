## Quantlet 3 - SimulationMinimumWageEffect
## Load Packages used in Q3
library(dplyr)
# Execution of Q1 and Q2 is necessary beforehand

# Simulation of Minimum Wage
# Function to gather average wages for employees affected by minimum wage for each employment status
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

# Apply Function to create Affected.by.minwage using sumsub2013
Affected.by.minwage = minwage_affect(sumsub2013)

# Create function to extract average earnings and store it as dataframe
select_average_earnings = function(x) {
  x = select(filter(x, Subject.to.minwage == 1), c(Employment.Status, avg_Hourly.earnings))
  x = as.data.frame(x)
  return(x)
}

# Apply select_average_earnings to Affected.by.minwage
Affected.by.minwage = select_average_earnings(Affected.by.minwage)

# Employment effect if Neo Classical Labor market in %
# Formula: 1 - (wmin / w)^(-x), with wmin: minimum wage, m:average gross hourly rate and x: labor demand elasticity
# Input Data Frame with different values
Labor.Demand.Elasticity = c(-0.2, -0.5, -0.75, -1, -1.2)

# Create Function to apply the formula with the input data x
employ_effect = function(x, y) {
  for(list in 1:length(x)) {
    curr_col = 1 - (8.5 / y$avg_Hourly.earnings)^(-1 * x[list])
    y[,paste("Neo.Employment.Effect", list, sep="")] = curr_col
  }
  return(y)
}

# Apply the employ_effect function for the dataset used before
Affected.by.minwage = employ_effect(Labor.Demand.Elasticity, Affected.by.minwage)

# Employment effect if Monopsonic Labor Market:
# Two alternative formulas, depend on average wage "w" and market power "m"
# If w^min > w(1+0.5m) use: 1 - (w^min / w(1 +m))^(-x)
# If w^min <= w(1+0.5m) use: (w^min - w / 0.5m * w) * ( 1 - (1+0.5 / (1+m))^-x)
# x = Labor.Demand.Elasticity Vector
# y = input data
# m = Market Power
# Create Function employ_effect_monopsonic for it
employ_effect_monopsonic = function(x, y, m) {
  # Create numerated colnames for Mon.Employment.Effect
  new_cols = paste("Mon.Employment.Effect", 1:length(x), sep="")
  # Create dataframe as matrix with number of cols matching to x
  effect_matrix = as.data.frame(matrix(ncol = length(x)))
  # Empty the data frame
  effect_matrix = effect_matrix[FALSE,]
  # Calculate w(1+0.5m) based on Average Hourly Earning, m = Market Power
  y$NewWage = y$avg_Hourly.earnings*(1 + 0.5*m)
  for(lines in 1:nrow(y)) {
  # Assign the values
  if(8.50 > y$NewWage[lines]) {
    curr_row = 1 - (8.5 / (y$avg_Hourly.earnings[lines]*(1 + m)))^(-x)
    effect_matrix = rbind(effect_matrix, curr_row)
  } else {
    curr_row = ((8.5 - y$avg_Hourly.earnings[lines]) / (0.5* m * y$avg_Hourly.earnings[lines])) * (1 -((1 + 0.5*m) /(1 + y$avg_Hourly.earnings[lines]))^(-x[5]))
    effect_matrix = rbind(effect_matrix, curr_row)
  }
  }
  # Assign names to the data frame
  names(effect_matrix) = new_cols
  output_matrix = cbind(y, effect_matrix)
  return(output_matrix)
}

# Apply the employ_effect_monopsonic function for the dataset used before with market power of 0.2
Affected.by.minwage = employ_effect_monopsonic(Labor.Demand.Elasticity, Affected.by.minwage, 0.2)

# Create function to plot Affected.by.minwage with Labor.Demand.Elasticity 
plot_graph_effect_minwage = function(input, elasticities) {
  dens1 = input %>%
    select(Employment.Status, Neo.Employment.Effect1,Neo.Employment.Effect2,Neo.Employment.Effect3,Neo.Employment.Effect4,Neo.Employment.Effect5)
  tdens1 = as.data.frame(t(dens1))
  colnames(tdens1) = c("fulltime","parttime","marginal")
  tdens1 = tdens1[-1,]
  tdens1$elast = elasticities
  tdens1$fulltime = as.numeric(as.vector(tdens1$fulltime))
  tdens1$parttime = as.numeric(as.vector(tdens1$parttime))
  tdens1$marginal = as.numeric(as.vector(tdens1$marginal)) 
  
  ggplot(data = tdens1, aes(x=elast*(-1))) +
    geom_path(aes(y = fulltime*(-1), color ="fulltime")) +
    geom_path(aes(y = parttime*(-1), color ="parttime")) +
    geom_path(aes(y = marginal*(-1), color = "marginal")) +
    theme_classic() +
    labs(title = "Simulation Of Minimum Wage On Employment ",
         x = "Elasticity",
         y = "Negative Change in Employment in Percent") +
    scale_colour_hue(name = "Employment Status",
                     labels = c("Full time", "Marginal", "Part Time", "Unemployed"))
}

# Apply plot_graph_effect_minwage using Affected.by.minwage and Labor.Demand.Elasticity as inputs
plot_graph_effect_minwage_output = plot_graph_effect_minwage(Affected.by.minwage, Labor.Demand.Elasticity)
# Save the plot created above into a png-file
# ggsave("plots/plot_graph_effect_minwage_output.png", plot_graph_effect_minwage_output)

