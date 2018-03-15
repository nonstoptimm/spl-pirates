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
employ_effect_monopsonic <- function(x, y) {
  i = 1
  k = 1
  # Create numerated colnames for Mon.Employment.Effect
  new_cols = paste("Mon.Employment.Effect", 1:length(x), sep="")
  # Create dataframe as matrix with number of cols matching to x
  effect_matrix = as.data.frame(matrix(ncol = length(x)))
  # Empty the data frame
  effect_matrix = effect_matrix[FALSE,]
  # Create new column "New Wage" as NA
  # y$NewWage = NA
  # Calculate based on Average Hourly Earning
  y$NewWage = y$avg_Hourly.earnings*(1 + 0.5*0.2)
  for(lines in 1:nrow(y)) {
    curr_wage = y$NewWage[lines]
    curr_earning = y$avg_Hourly.earnings[lines]
    print(curr_wage)
    print(curr_earning)
    
    ifelse(curr_wage > 8.50, (sapply(x, function(x, curr_wage) { 1 - (8.5 / curr_wage)^(-1 * x) }, curr_wage = curr_wage)), (sapply(x, function(x, curr_wage, curr_earning) { ((curr_wage - curr_earning) / (0.5*0.2 * curr_earning)) * (1 -((1 + 0.5) /(1 + 0.2))^(-1 * x)) },  curr_wage = curr_wage, curr_earning = curr_earning)))
    
    effect_matrix = as.data.frame(matrix(ncol = length(x)))
    # Empty the data frame
    effect_matrix = effect_matrix[FALSE,]
    values = c()
    k = 1
   for(rows in 1:nrow(y)) {
    for(elasticity in x) {
      value = ifelse(y$NewWage[k] > 8.50, 1 - (8.5 / y$NewWage[k])^(-1 * elasticity),((y$NewWage[k] - y$avg_Hourly.earnings[k]) / (0.5*0.2 * y$avg_Hourly.earnings[k])) * (1 -((1 + 0.5) /(1 + 0.2))^(-1 * elasticity)))
      values = c(values, value)
    }
    effect_matrix = rbind(effect_matrix, values)
    values = c()
    k = k + 1
   }
    
    for(rows in 1:nrow(y)){
      print("Hi!")
    }
    
    
    y[,paste("Function", i, sep="")]
    
    # Creating the formulas mentioned above as functions
    #var1 = function(x) { 1 - (8.5 / curr_wage)^(-1 * x) }
    #var2 = function(x, curr_wage, curr_earning) { ((curr_wage - curr_earning) / (0.5*0.2 * curr_earning)) * (1 -((1 + 0.5) /(1 + 0.2))^(-1 * x)) }
    # Assign the values
    if(curr_wage > 8.50) {
      #var1 = function(x, curr_wage) { 1 - (8.5 / curr_wage)^(-1 * x) }
      #curr_row = sapply(x, var1, curr_wage = y$avg_Hourly.earnings[lines])
      print(sapply(x, function(x) { 1 - (8.5 / y$NewWage[lines])^(-1 * x) }), curr_wage = y$NewWage[lines])
      #effect_matrix = rbind(effect_matrix, curr_row)
      print("L1")
    } else {
      print(curr_wage)
      print(curr_earning)
      #var2 = function(x,c1,c2) ((current_New_Wage - get(c2)) / (0.5*0.2 * get(c2))) * (1 -((1 + 0.5) /(1 + 0.2))^(-1 * x))
      temp = sapply(x, var2, curr_wage=y$NewWage[k], curr_earning=y$avg_Hourly.earnings[k])
      print(temp)
      print(curr_wage)
      print(curr_earning)
      #print(sapply(x, function(x, curr_wage, curr_earning) { ((curr_wage - curr_earning) / (0.5*0.2 * curr_earning)) * (1 -((1 + 0.5) /(1 + 0.2))^(-1 * x)) }),  curr_wage = curr_wage, curr_earning = curr_earning)
      #effect_matrix = rbind(effect_matrix, curr_row)
      print("L2")
    }
    #k = k + 1
  }
  # Assign names to the data frame
  names(effect_matrix) = new_cols
  output_matrix = cbind(y, effect_matrix)
  return(output_matrix)
}


# Apply the employ_effect_monopsonic function for the dataset used before
Affected.by.minwage = employ_effect_monopsonic(Labor.Demand.Elasticity, Affected.by.minwage)