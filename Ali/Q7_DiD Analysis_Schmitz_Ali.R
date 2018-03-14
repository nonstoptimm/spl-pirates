# D&D Analysis by Schmitz(2016)
# log(y_it) = bite_i + D_t^MW * beta_l + Sum_t D_t^year * gamma_t + alpha + e_i,t
#Control for State: Average labor income in different periods

#install.packages("plm")
#library(plm)

## Data Pre Processing for DiD estimation
# Create sub-dataframe with only variables of interest
data_selector = function(analyze_tc) {
  select(filter(analyze_tc), c(Wave, State.of.Residence, Observations, Fraction, Kaitz, Delta.Log.Full.Employment, Delta.Log.Part.Employment, Delta.Log.Marginal.Employment, binary_treatment1, binary_treatment2
  ))
}

DiD_Schmitz = data_selector(analyze_tc)

#Generate numeric variable for years, thus convert the variable wave
DiD_Schmitz$year = as.numeric(as.character(DiD_DIW$Wave))

#Generate dummy for year for indicator when minimum wage was introduced
DiD_Schmitz$year15.dummy = ifelse(DiD_DIW$year >= 2015, 1, 0)


### Regression
## Regression 1: We regress on Regular Employment
# Regression 1.1.1 (baseline): Using Kaitz Index
did_1.1.1 = lm(Delta.Log.Full.Employment ~ (Kaitz*year15.dummy), data = DiD_Schmitz)
summary(did1.1.1)
# Regression 1.2.1: Kaitz and Control Population


## Regression 2: We regress on Part Employment
# Regression 2.1.1 (baseline): Using Kaitz Index
did_2.1.1 = lm(Delta.Log.Part.Employment ~ (Kaitz*year15.dummy), data = DiD_Schmitz)
summary(did_2.1.1)



## Regression 3: We regress on Marginal Employment
# Regression 3.1.1 (baseline): Using Kaitz Index
did_3.1.1 = lm(Delta.Log.Marginal.Employment ~ (Kaitz*year15.dummy), data = DiD_Schmitz)
summary(did_3.1.1)
