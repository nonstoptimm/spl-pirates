# D&D Analysis
# install.packages("plm")
library(plm)

## This quantlet is about an estimation of the minimum wage effect on Employment.
# We use regression estimations of Schmitz(2017) and Caliendo(2017)
# Schmitz uses the log change of employment, Caliendo the log employment rate
# We estimate some regressions from them

## Data Pre Processing for DiD estimation
# Create sub-dataframe with only variables of interest
data_selector = function(analyze_tc) {
  select(filter(analyze_tc), c(Wave, State.of.Residence, Observations, Fraction, Kaitz, 
                               Delta.Log.Full.Employment, Delta.Log.Part.Employment, Delta.Log.Marginal.Employment, 
                               binary_treatment1, binary_treatment2, 
                               Log.Full.Employment.Rate, Log.Part.Employment.Rate, Log.Marginal.Employment.Rate)
  )
}
# Apply data selector for sub-dataframe
estimation = data_selector(analyze_tc)

# Use data selector
data_selector = function(analyze_tc, wave) {
  select(filter(analyze_tc, Wave == wave), c(Wave, State.of.Residence, Kaitz, Fraction))
}
# Create subset for 2013
analyze_2013 = data_selector(analyze_tc, 2013)
# Append Kaitz from 2013 back to estimation dataset
# Need it for Schmitz(2017)
estimation$Kaitz.13 = analyze_2013$Kaitz

# Create subset for 2014
analyze_2014 = data_selector(analyze_tc, 2014)
# Append Kaitz and Fraction from 2014 back to estimation dataset
# Need it for Caliendo(2017) estimation
estimation$Kaitz.14 = analyze_2014$Kaitz
estimation$Fraction.14 = analyze_2014$Fraction

#Function for Pre Processing
pre_processing_estimation = function(x) {
  #Generate numeric variable for years, thus convert the variable wave
  x$year = as.numeric(as.character(x$Wave))
  #Generate dummy for year for indicator when minimum wage was introduced
  x$year15.dummy = ifelse(x$year >= 2015, 1, 0)
  #Generate Interaction Variable Fraction for Caliando2017
  x$DiD.estimator.Fraction = (x$year15.dummy * x$Fraction.14)
  #Generate Interaction Variable Kaitz for Caliando2017
  x$DiD.estimator.Kaitz = (x$year15.dummy * x$Kaitz.14)
  #Generate Interaction Variable Kaitz for Schmitz2017
  x$DiD.estimator.Kaitz.Schmitz = (x$year15.dummy * x$Kaitz.13)
  #Generate Interaction Variable Kaitz for Schmitz2017
  ##Generate Control Variables
    # Control for years:
    x$year13.dummy = ifelse(x$year >= 2013, 1, 0)
    x$year12.dummy = ifelse(x$year >= 2012, 1, 0)
    # Population in log
    x$Log.Population = log(x$Observations)
  return(x)
  
}

#Apply it to data
estimation = pre_processing_estimation(estimation)



####Regressions ####
##Note: Both estimation models use a fixed effect model. We will assume this holds for now and apply it.
## We will test for the properties of the models in Q8

### Caliendo(2017): Use the log employment rates of each Employment Group
## Four regressions per Group: One baseline and one with control variables, with each Bite Index
## Regression 1: We regress on Log Regular Employment Rate
# Regression 1.1.1 (baseline): Using Kaitz Index
did_1.1.1 = plm(Log.Full.Employment.Rate ~ DiD.estimator.Kaitz, data = estimation, index=c("State.of.Residence", "Wave"), model = "within")
summary(did_1.1.1)
# Regression 1.1.2: Using Kaitz Index and Control Variables
did_1.1.2 = plm(Log.Full.Employment.Rate ~ DiD.estimator.Kaitz + Log.Population + year13.dummy + year12.dummy, data = estimation, index=c("State.of.Residence", "Wave"), model = "within")
summary(did_1.1.2)
# Regression 1.2.1 (baseline): Using Fraction Index
did_1.2.1 = plm(Log.Full.Employment.Rate ~ DiD.estimator.Fraction, data = estimation, index=c("State.of.Residence", "Wave"), model = "within")
summary(did_1.2.1)
# Regression 1.2.2: Using Fraction Index and Control Variables
did_1.2.2 = plm(Log.Full.Employment.Rate ~ DiD.estimator.Fraction + Log.Population + year13.dummy + year12.dummy, data = estimation, index=c("State.of.Residence", "Wave"), model = "within")
summary(did_1.2.2)

## Regression 2: We regress on Log Part Time Employment Rate
# Regression 2.1.1 (baseline): Using Kaitz Index
did_2.1.1 = plm(Log.Part.Employment.Rate ~ DiD.estimator.Kaitz, data = estimation, index=c("State.of.Residence", "Wave"), model = "within")
summary(did_2.1.1)
# Regression 2.1.2: Using Kaitz Index and Control Variables
did_2.1.2 = plm(Log.Part.Employment.Rate ~ DiD.estimator.Kaitz + Log.Population + year13.dummy + year12.dummy, data = estimation, index=c("State.of.Residence", "Wave"), model = "within")
summary(did_2.1.2)
# Regression 2.2.1 (baseline): Using Fraction Index
did_2.2.1 = plm(Log.Part.Employment.Rate ~ DiD.estimator.Fraction, data = estimation, index=c("State.of.Residence", "Wave"), model = "within")
summary(did_2.2.1)
# Regression 2.2.2: Using Fraction Index and Control Variables
did_2.2.2 = plm(Log.Part.Employment.Rate ~ DiD.estimator.Fraction + Log.Population + year13.dummy + year12.dummy, data = estimation, index=c("State.of.Residence", "Wave"), model = "within")
summary(did_2.2.2)


## Regression 3: We regress on Log marginal Employment Rate
# Regression 3.1.1 (baseline): Using Kaitz Index
did_3.1.1 = plm(Log.Marginal.Employment.Rate ~ DiD.estimator.Kaitz, data = estimation, index=c("State.of.Residence", "Wave"), model = "within")
summary(did_3.1.1)
# Regression 3.1.2: Using Kaitz Index and Control Variables
did_3.1.2 = plm(Log.Marginal.Employment.Rate ~ DiD.estimator.Kaitz + Log.Population + year13.dummy + year12.dummy, data = estimation, index=c("State.of.Residence", "Wave"), model = "within")
summary(did_3.1.2)
# Regression 3.2.1 (baseline): Using Fraction Index
did_3.2.1 = plm(Log.Marginal.Employment.Rate ~ DiD.estimator.Fraction, data = estimation, index=c("State.of.Residence", "Wave"), model = "within")
summary(did_3.2.1)
# Regression 3.2.2: Using Fraction Index and Control Variables
did_3.2.2 = plm(Log.Marginal.Employment.Rate ~ DiD.estimator.Fraction + Log.Population + year13.dummy + year12.dummy, data = estimation, index=c("State.of.Residence", "Wave"), model = "within")
summary(did_3.2.2)



### Schmitz(2017): Use the Change in Log Employment of each Employment Group
## Three regressions per Group: One baseline and two robust regressions, with binary treatment variable
## Regression 4: We regress on Change in Log Regular Employment
# Regression 4.1 (baseline): Using Kaitz2013
did_4.1 = plm(Delta.Log.Full.Employment ~ DiD.estimator.Kaitz.Schmitz, data = estimation, index=c("State.of.Residence", "Wave"), model = "within")
summary(did_4.1)
# Regression 4.2: Using Kaitz2013 and standart binary treatment
did_4.2 = plm(Delta.Log.Full.Employment ~ binary_treatment1 + year15.dummy + (binary_treatment1*year15.dummy), data = estimation, index=c("State.of.Residence", "Wave"), model = "within")
summary(did_4.2)
# Regression 4.3: Using Kaitz2013 and robust binary treatment
did_4.3 = plm(Delta.Log.Full.Employment ~ binary_treatment2 + year15.dummy + (binary_treatment2*year15.dummy), data = estimation, index=c("State.of.Residence", "Wave"), model = "within")
summary(did_4.3)

## Regression 5: We regress on Change in Log Part Time Employment
# Regression 5.1 (baseline): Using Kaitz2013
did_5.1 = plm(Delta.Log.Part.Employment ~ DiD.estimator.Kaitz.Schmitz, data = estimation, index=c("State.of.Residence", "Wave"), model = "within")
summary(did_5.1)
# Regression 5.2: Using Kaitz2013 and standart binary treatment
did_5.2 = plm(Delta.Log.Part.Employment ~ binary_treatment1 + year15.dummy + (binary_treatment1*year15.dummy), data = estimation, index=c("State.of.Residence", "Wave"), model = "within")
summary(did_5.2)
# Regression 5.3: Using Kaitz2013 and robust binary treatment
did_5.3 = plm(Delta.Log.Part.Employment ~ binary_treatment2 + year15.dummy + (binary_treatment2*year15.dummy), data = estimation, index=c("State.of.Residence", "Wave"), model = "within")
summary(did_5.3)

## Regression 6: We regress on Change in Log Marginal Employment
# Regression 6.1 (baseline): Using Kaitz2013
did_6.1 = plm(Delta.Log.Marginal.Employment ~ DiD.estimator.Kaitz.Schmitz, data = estimation, index=c("State.of.Residence", "Wave"), model = "within")
summary(did_6.1)
# Regression 6.2: Using Kaitz2013 and standart binary treatment
did_6.2 = plm(Delta.Log.Marginal.Employment ~ binary_treatment1 + year15.dummy + (binary_treatment1*year15.dummy), data = estimation, index=c("State.of.Residence", "Wave"), model = "within")
summary(did_6.2)
# Regression 6.3: Using Kaitz2013 and robust binary treatment
did_6.3 = plm(Delta.Log.Marginal.Employment ~ binary_treatment2 + year15.dummy + (binary_treatment2*year15.dummy), data = estimation, index=c("State.of.Residence", "Wave"), model = "within")
summary(did_6.3)


##### Output ####

stargazer(did_1.1.1, did_1.1.2, did_1.1.3, did_1.1.4, title="Results", type="text", align=TRUE, 
          no.space=TRUE, 
          keep.stat= c("n","adj.rsq","rsq"), 
          dep.var.labels = ("Panel A: Regular Employment"), 
          covariate.labels=c("Bite", "D2015", "Population(log,t)", "Bite x D2013", "Bite x D2012", "Bite x D2015"))


stargazer(did_1.1.1, did_1.1.2, did_1.1.3, did_1.1.4, title="Results", type="text", align=TRUE, 
          no.space=TRUE, 
          keep.stat= c("n","adj.rsq","rsq"), 
          dep.var.labels = ("Panel A: Full Time Employment"), 
          covariate.labels=c("Bite.K", "D2015", "Population(log,t)", "Bite x D2013", "Bite x D2012", "Bite x D2015"))


