## Quantlet 7 - DiffDiffEstimation Load Packages used in Q7
library(plm)
library(stargazer)
# Execution of Q1, Q4, Q5 and Q6 is necessary beforehand!

# Data Pre-Processing for DiD estimation Create sub-dataframe with only variables of interest
data_selector = function(analyze_tc) {
    select(filter(analyze_tc), c(Wave, State.of.Residence, Observations, Fraction, Kaitz, Delta.Log.Full.Employment, 
        Delta.Log.Part.Employment, Delta.Log.Marginal.Employment, binary_treatment1, binary_treatment2, 
        Log.Full.Employment.Rate, Log.Part.Employment.Rate, Log.Marginal.Employment.Rate))
}
# Apply data selector for sub-dataframe
estimation = data_selector(analyze_tc)

# Create data_filter
data_filter = function(analyze_tc, wave) {
    select(filter(analyze_tc, Wave == wave), c(Wave, State.of.Residence, Kaitz, Fraction))
}
# Create subset for 2013
analyze_2013 = data_filter(analyze_tc, 2013)
# Append Kaitz from 2013 back to estimation dataset Need it for Schmitz(2017)
estimation$Kaitz.13 = analyze_2013$Kaitz

# Create subset for 2014
analyze_2014 = data_filter(analyze_tc, 2014)
# Append Kaitz and Fraction from 2014 back to estimation dataset Need it for Caliendo(2017) estimation
estimation$Kaitz.14 = analyze_2014$Kaitz
estimation$Fraction.14 = analyze_2014$Fraction

# Function for Pre Processing
pre_processing_estimation = function(x) {
    # Generate numeric variable for years, thus convert the variable wave
    x$year = as.numeric(as.character(x$Wave))
    # Generate dummy for year for indicator when minimum wage was introduced
    x$year15.dummy = ifelse(x$year >= 2015, 1, 0)
    # Generate Interaction Variable Fraction for Caliando2017
    x$DiD.estimator.Fraction = (x$year15.dummy * x$Fraction.14)
    # Generate Interaction Variable Kaitz for Caliando2017
    x$DiD.estimator.Kaitz = (x$year15.dummy * x$Kaitz.14)
    # Generate Interaction Variable Kaitz for Schmitz2017
    x$DiD.estimator.Kaitz.Schmitz = (x$year15.dummy * x$Kaitz.13)
    # Generate Interaction Variable Binary Treatment Schimtz2017
    x$binary1 = (x$binary_treatment1 * x$year15.dummy)
    x$binary2 = (x$binary_treatment2 * x$year15.dummy)
    ## Generate Control Variables Control for years:
    x$year14.dummy = ifelse(x$year >= 2014, 1, 0)
    x$year13.dummy = ifelse(x$year >= 2013, 1, 0)
    x$year12.dummy = ifelse(x$year >= 2012, 1, 0)
    # COntrol for time (Schmitz2017)
    x$linear.trend = (x$Kaitz.13 * x$year)
    x$quadratic.trend = (x$Kaitz.13 * (x$year * x$year))
    # Interaction Variables (Caliendo2017):
    x$Interaction_Kaitz_y14 = (x$Kaitz.14 * x$year14.dummy)
    x$Interaction_Kaitz_y13 = (x$Kaitz.14 * x$year13.dummy)
    x$Interaction_Kaitz_y12 = (x$Kaitz.14 * x$year12.dummy)
    x$Interaction_Fraction_y14 = (x$Fraction.14 * x$year14.dummy)
    x$Interaction_Fraction_y13 = (x$Fraction.14 * x$year13.dummy)
    x$Interaction_Fraction_y12 = (x$Fraction.14 * x$year12.dummy)
    # Population in log
    x$Log.Population = log(x$Observations)
    return(x)
}

# Apply it to data
estimation = pre_processing_estimation(estimation)

# Regressions Note: Both estimation models use a fixed effect model. We will assume this holds for now
# and apply it.  We will test for the properties of the models in Q8

### Caliendo(2017): Use the log employment rates of each Employment Group Four regressions per Group: One
### baseline and one with control variables, with each Bite Index Regression 1: We regress on Log Regular
### Employment Rate Regression 1.1.1 (baseline): Using Kaitz Index
did_1.1.1 = plm(Log.Full.Employment.Rate ~ DiD.estimator.Kaitz, data = estimation, index = c("State.of.Residence", 
    "Wave"), model = "within")
# Regression 1.1.2: Using Kaitz Index and Control Variables
did_1.1.2 = plm(Log.Full.Employment.Rate ~ DiD.estimator.Kaitz + Log.Population + Interaction_Kaitz_y13 + 
    Interaction_Kaitz_y12, data = estimation, index = c("State.of.Residence", "Wave"), model = "within")
# Regression 1.2.1 (baseline): Using Fraction Index
did_1.2.1 = plm(Log.Full.Employment.Rate ~ DiD.estimator.Fraction, data = estimation, index = c("State.of.Residence", 
    "Wave"), model = "within")
# Regression 1.2.2: Using Fraction Index and Control Variables
did_1.2.2 = plm(Log.Full.Employment.Rate ~ DiD.estimator.Fraction + Log.Population + Interaction_Fraction_y13 + 
    Interaction_Fraction_y12, data = estimation, index = c("State.of.Residence", "Wave"), model = "within")

# Regression 2: We regress on Log Part Time Employment Rate Regression 2.1.1 (baseline): Using Kaitz
# Index
did_2.1.1 = plm(Log.Part.Employment.Rate ~ DiD.estimator.Kaitz, data = estimation, index = c("State.of.Residence", 
    "Wave"), model = "within")
# Regression 2.1.2: Using Kaitz Index and Control Variables
did_2.1.2 = plm(Log.Part.Employment.Rate ~ DiD.estimator.Kaitz + Log.Population + Interaction_Kaitz_y13 + 
    Interaction_Kaitz_y12, data = estimation, index = c("State.of.Residence", "Wave"), model = "within")
# Regression 2.2.1 (baseline): Using Fraction Index
did_2.2.1 = plm(Log.Part.Employment.Rate ~ DiD.estimator.Fraction, data = estimation, index = c("State.of.Residence", 
    "Wave"), model = "within")
# Regression 2.2.2: Using Fraction Index and Control Variables
did_2.2.2 = plm(Log.Part.Employment.Rate ~ DiD.estimator.Fraction + Log.Population + Interaction_Fraction_y13 + 
    Interaction_Fraction_y12, data = estimation, index = c("State.of.Residence", "Wave"), model = "within")

# Regression 3: We regress on Log marginal Employment Rate Regression 3.1.1 (baseline): Using Kaitz
# Index
did_3.1.1 = plm(Log.Marginal.Employment.Rate ~ DiD.estimator.Kaitz, data = estimation, index = c("State.of.Residence", 
    "Wave"), model = "within")
# Regression 3.1.2: Using Kaitz Index and Control Variables
did_3.1.2 = plm(Log.Marginal.Employment.Rate ~ DiD.estimator.Kaitz + Log.Population + Interaction_Kaitz_y13 + 
    Interaction_Kaitz_y12, data = estimation, index = c("State.of.Residence", "Wave"), model = "within")
# Regression 3.2.1 (baseline): Using Fraction Index
did_3.2.1 = plm(Log.Marginal.Employment.Rate ~ DiD.estimator.Fraction, data = estimation, index = c("State.of.Residence", 
    "Wave"), model = "within")
# Regression 3.2.2: Using Fraction Index and Control Variables
did_3.2.2 = plm(Log.Marginal.Employment.Rate ~ DiD.estimator.Fraction + Log.Population + Interaction_Fraction_y13 + 
    Interaction_Fraction_y12, data = estimation, index = c("State.of.Residence", "Wave"), model = "within")

# Schmitz(2017): Use the Change in Log Employment of each Employment Group Regression: We regress on
# Change in Log Employment Status Using Kaitz2013
did_4.1 = plm(Delta.Log.Full.Employment ~ DiD.estimator.Kaitz.Schmitz + linear.trend + quadratic.trend, 
    data = estimation, index = c("State.of.Residence", "Wave"), model = "within")
did_5.1 = plm(Delta.Log.Part.Employment ~ DiD.estimator.Kaitz.Schmitz + linear.trend + quadratic.trend, 
    data = estimation, index = c("State.of.Residence", "Wave"), model = "within")
did_6.1 = plm(Delta.Log.Marginal.Employment ~ DiD.estimator.Kaitz.Schmitz + linear.trend + quadratic.trend, 
    data = estimation, index = c("State.of.Residence", "Wave"), model = "within")

# Regression: We regress on Change in Log Employment Status Using Kaitz2013 and standart binary
# treatment
did_4.2 = plm(Delta.Log.Full.Employment ~ binary1, data = estimation, index = c("State.of.Residence", "Wave"), 
    model = "within")
did_5.2 = plm(Delta.Log.Part.Employment ~ binary1, data = estimation, index = c("State.of.Residence", "Wave"), 
    model = "within")
did_6.2 = plm(Delta.Log.Marginal.Employment ~ binary1, data = estimation, index = c("State.of.Residence", 
    "Wave"), model = "within")

# Regression: We regress on Change in Log Employment Status Using Kaitz2013 and robust binary treatment
did_4.3 = plm(Delta.Log.Full.Employment ~ binary2, data = estimation, index = c("State.of.Residence", "Wave"), 
    model = "within")
did_5.3 = plm(Delta.Log.Part.Employment ~ binary2, data = estimation, index = c("State.of.Residence", "Wave"), 
    model = "within")
did_6.3 = plm(Delta.Log.Marginal.Employment ~ binary2, data = estimation, index = c("State.of.Residence", 
    "Wave"), model = "within")

# Output Regression 1: We regress on Log Regular Employment Rate
stargazer(did_1.1.1, did_1.1.2, did_1.2.1, did_1.2.2, title = "Effects on Full Time Employment using Kaitz and Fraction Index", 
    type = "text", align = TRUE, no.space = FALSE, keep.stat = c("n", "adj.rsq", "rsq"), dep.var.labels = ("Panel A: Log Regular Employment Rate"), 
    order = c(1, 7, 2, 3, 4, 5, 6), covariate.labels = c("Bite.K x D2015", "Bite.F x D2015", "Population(log,t)", 
        "Bite.K x D2013", "Bite.K x D2012", "Bite.F x D2013", "Bite.F x D2012"), column.labels = c("Kaitz", 
        "Fraction"), column.separate = c(2, 2))

# Regression 2: We regress on Log Part Time Employment Rate
stargazer(did_2.1.1, did_2.1.2, did_2.2.1, did_2.2.2, title = "Effects on Part Time Employment using Kaitz and Fraction Index", 
    type = "text", align = TRUE, no.space = FALSE, keep.stat = c("n", "adj.rsq", "rsq"), dep.var.labels = ("Panel B: Log Part Time Employment Rate"), 
    covariate.labels = c("Bite.K x D2015", "Bite.F x D2015", "Population(log,t)", "Bite.K x D2013", "Bite.K x D2012", 
        "Bite.F x D2013", "Bite.F x D2012"), order = c(1, 7, 2, 3, 4, 5, 6), column.labels = c("Kaitz", 
        "Fraction"), column.separate = c(2, 2))

# Regression 3: We regress on Log marginal Employment Rate
stargazer(did_3.1.1, did_3.1.2, did_3.2.1, did_3.2.2, title = "Effects on Marginal Employment using Kaitz and Fraction Index", 
    type = "text", align = TRUE, no.space = FALSE, keep.stat = c("n", "adj.rsq", "rsq"), dep.var.labels = ("Panel C: Log Marginal Employment Rate"), 
    covariate.labels = c("Bite.K x D2015", "Bite.F x D2015", "Population(log,t)", "Bite.K x D2013", "Bite.K x D2012", 
        "Bite.F x D2013", "Bite.F x D2012"), order = c(1, 7, 2, 3, 4, 5, 6), column.labels = c("Kaitz", 
        "Fraction"), column.separate = c(2, 2))

# Regression 4: We regress on Change in Log Employment Status Using Kaitz2013
stargazer(did_4.1, did_5.1, did_6.1, title = "Effects on Employment Outcomes using Kaitz2013", type = "text", 
    align = TRUE, no.space = TRUE, keep.stat = c("n", "adj.rsq", "rsq"), dep.var.labels.include = FALSE, 
    dep.var.caption = ("Panel D: Log Employment Status"), covariate.labels = c("Bite x D2015", "Linear Trend", 
        "Quadratic Trend"), column.labels = c("Full", "Part", "Marginal"))

# Regression 5: We regress on Change in Log Employment Status Using Kaitz2013 and standart binary
# treatment
stargazer(did_4.2, did_5.2, did_6.2, title = "Effects on Employment Outcomes using Binary Treatment", type = "text", 
    align = TRUE, no.space = FALSE, keep.stat = c("n", "adj.rsq", "rsq"), dep.var.labels.include = FALSE, 
    dep.var.caption = ("Panel E: Log Employment Status"), covariate.labels = c("Treated x 2015"), column.labels = c("Full", 
        "Part", "Marginal"))

# Regression 6: We regress on Change in Log Employment Status Using Kaitz2013 and robust binary
# treatment
stargazer(did_4.3, did_5.3, did_6.3, title = "Effects on Employment Outcomes using Robust Binary Treatment", 
    type = "text", align = TRUE, no.space = FALSE, keep.stat = c("n", "adj.rsq", "rsq"), dep.var.labels.include = FALSE, 
    dep.var.caption = ("Panel F: Log Employment Status"), covariate.labels = c("Treated x 2015"), column.labels = c("Full", 
        "Part", "Marginal"))


