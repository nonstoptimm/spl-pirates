# D&D Analysis by Schmitz(2016)
# log(y_it) = bite_i + D_t^MW * beta_l + Sum_t D_t^year * gamma_t + alpha + e_i,t
#Control for State: Average labor income in different periods

#install.packages("plm")
#library(plm)

DiD_estimation_Schmitz = analyze_tc

# Estimation1
str(analyze_tc)
# Generate numeric variable for years, thus convert the variable wave
analyze_tc$year = as.numeric(as.character(analyze_tc$Wave))

# Creat dummy for year for indicator when minimum wage was introduced
analyze_tc$year.dummy = ifelse(analyze_tc$year >= 2015, 1, 0)

# Generate numeric variable for States
analyze_tc$States.Num = as.numeric(analyze_tc$State.of.Residence)


### Regression1 ###
# We regress on the following variables: Log.Full.Employment, Log.Part.Employment, Log.Marginal.Employment and
# Delta.Log.Full.Employment, Delta.Log.Part.Employment, Delta.Log.Marginal.Employment

# Regression1.1 (Baseline): Only Kaitz on Emplyoment Rate
did1.1 = lm(Marginal.Employment.Rate ~ (Kaitz*year.dummy), data = analyze_tc)
summary(did1.1)

## Regression 1.2: Kaitz with Control for and Year ##
did1.2 = lm(Marginal.Employment.Rate ~ (Kaitz*year.dummy) + Kaitz*year, data = analyze_tc)
summary(did1.2)


# Estimation2
# Generate dummy for year for indicator when minimum wage was introduced
analyze_tc$year.dummy = ifelse(analyze_tc$year >= 2015, 1, 0)

# DiD Variable
analyze_tc$did1 = analyze_tc$year.dummy * analyze_tc$binary_treatment1

### Regression1 ###
##Regression with Standart Treatment Dummy
did2.1 = lm(Marginal.Employment.Rate ~ binary_treatment1 + year.dummy + did, data = analyze_tc)
summary(did2.1)



# Estimation3
# DiD Variable
analyze_tc$did2 = analyze_tc$year.dummy * analyze_tc$binary_treatment2
did3.1 = lm(Marginal.Employment.Rate ~ binary_treatment2 + year.dummy + did, data = analyze_tc)
summary(did3.1)
