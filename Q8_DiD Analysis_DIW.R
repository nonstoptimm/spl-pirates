# Quantlet 8
# D&D Analysis by DIW(2017)
# Identification estimation:

## Data Pre Processing for DiD estimation
# Create sub-dataframe with only variables of interest
data_selector = function(analyze_tc) {
  select(filter(analyze_tc), c(Wave, State.of.Residence, Observations, Fraction, Kaitz, Log.Full.Employment, Log.Part.Employment, Log.Marginal.Employment, binary_treatment1
  ))
}
# Apply the Function
DiD_DIW = data_selector(analyze_tc)

#Generate numeric variable for years, thus convert the variable wave
DiD_DIW$year = as.numeric(as.character(DiD_DIW$Wave))

#Generate dummy for year for indicator when minimum wage was introduced
DiD_DIW$year15.dummy = ifelse(DiD_DIW$year >= 2015, 1, 0)

##Generate Control Variables
# Control for years:
DiD_DIW$year13.dummy = ifelse(DiD_DIW$year >= 2013, 1, 0)
DiD_DIW$year12.dummy = ifelse(DiD_DIW$year >= 2012, 1, 0)
# Population in log
DiD_DIW$Log.Population = log(DiD_DIW$Observations)

### Regression
## Regression 1: We regress on Regular Employment
# Regression 1.1.1 (baseline): Using Kaitz Index
did_1.1.1 = lm(Log.Full.Employment ~ (Kaitz*year15.dummy), data = DiD_DIW)
summary(did_1.1.1)
# Regression 1.1.2: Kaitz and Control: Population
did_1.1.2 = lm(Log.Full.Employment ~ (Kaitz*year15.dummy) + Log.Population, data = DiD_DIW)
summary(did_1.1.2)
# Regression 1.1.3: Kaitz, Control: Population and Year2013
did_1.1.3 = lm(Log.Full.Employment ~ (Kaitz*year15.dummy) + Log.Population + year13.dummy, data = DiD_DIW)
summary(did_1.1.3)
# Regression 1.1.4: Kaitz, Control: Population, Year2013, Year2012
did_1.1.4 = lm(Log.Full.Employment ~ (Kaitz*year15.dummy) + Log.Population + year13.dummy + year12.dummy, data = DiD_DIW)
summary(did_1.1.4)

# Regression 1.2.1 (baseline): Using Fraction Index
did_1.2.1 = lm(Log.Full.Employment ~ (Fraction*year15.dummy), data = DiD_DIW)
summary(did_1.2.1)
# Regression 1.2.2: Kaitz and Control: Population
did_1.2.2 = lm(Log.Full.Employment ~ (Fraction*year15.dummy) + Log.Population, data = DiD_DIW)
summary(did_1.2.2)
# Regression 1.2.3: Kaitz, Control: Population and Year2013
did_1.2.3 = lm(Log.Full.Employment ~ (Fraction*year15.dummy) + Log.Population + year13.dummy, data = DiD_DIW)
summary(did_1.2.3)
# Regression 1.2.4: Kaitz, Control: Population, Year2013, Year2012
did_1.2.4 = lm(Log.Full.Employment ~ (Fraction*year15.dummy) + Log.Population + year13.dummy + year12.dummy, data = DiD_DIW)
summary(did_1.2.4)

## Regression 2: We regress on Part Employment
# Regression 2.1.1 (baseline): Using Kaitz Index
did_2.1.1 = lm(Log.Part.Employment ~ (Kaitz*year15.dummy), data = DiD_DIW)
summary(did_2.1.1)
# Regression 2.1.2: Kaitz and Control: Population
did_2.1.2 = lm(Log.Part.Employment ~ (Kaitz*year15.dummy) + Log.Population, data = DiD_DIW)
summary(did_2.1.2)
# Regression 2.1.3: Kaitz, Control: Population and Year2013
did_2.1.3 = lm(Log.Part.Employment ~ (Kaitz*year15.dummy) + Log.Population + year13.dummy, data = DiD_DIW)
summary(did_2.1.3)
# Regression 2.1.4: Kaitz, Control: Population, Year2013, Year2012
did_2.1.4 = lm(Log.Part.Employment ~ (Kaitz*year15.dummy) + Log.Population + year13.dummy + year12.dummy, data = DiD_DIW)
summary(did_2.1.4)

# Regression 2.2.1 (baseline): Using Fraction Index
did_2.2.1 = lm(Log.Part.Employment ~ (Fraction*year15.dummy), data = DiD_DIW)
summary(did_2.2.1)
# Regression 2.2.2: Kaitz and Control: Population
did_2.2.2 = lm(Log.Part.Employment ~ (Fraction*year15.dummy) + Log.Population, data = DiD_DIW)
summary(did_2.2.2)
# Regression 2.2.3: Kaitz, Control: Population and Year2013
did_2.2.3 = lm(Log.Part.Employment ~ (Fraction*year15.dummy) + Log.Population + year13.dummy, data = DiD_DIW)
summary(did_2.2.3)
# Regression 2.2.4: Kaitz, Control: Population, Year2013, Year2012
did_2.2.4 = lm(Log.Part.Employment ~ (Fraction*year15.dummy) + Log.Population + year13.dummy + year12.dummy, data = DiD_DIW)
summary(did_2.2.4)

## Regression 3: We regress on Marginal Employment
# Regression 3.1.1 (baseline): Using Kaitz Index
did_3.1.1 = lm(Log.Marginal.Employment ~ (Kaitz*year15.dummy), data = DiD_DIW)
summary(did_3.1.1)
# Regression 3.1.2: Kaitz and Control: Population
did_3.1.2 = lm(Log.Marginal.Employment ~ (Kaitz*year15.dummy) + Log.Population, data = DiD_DIW)
summary(did_3.1.2)
# Regression 3.1.3: Kaitz, Control: Population and Year2013
did_3.1.3 = lm(Log.Marginal.Employment ~ (Kaitz*year15.dummy) + Log.Population + year13.dummy, data = DiD_DIW)
summary(did_3.1.3)
# Regression 3.1.4: Kaitz, Control: Population, Year2013, Year2012
did_3.1.4 = lm(Log.Marginal.Employment ~ (Kaitz*year15.dummy) + Log.Population + year13.dummy + year12.dummy, data = DiD_DIW)
summary(did_3.1.4)

# Regression 3.2.1 (baseline): Using Fraction Index
did_3.2.1 = lm(Log.Marginal.Employment ~ (Fraction*year15.dummy), data = DiD_DIW)
summary(did_3.2.1)
# Regression 3.2.2: Kaitz and Control: Population
did_3.2.2 = lm(Log.Marginal.Employment ~ (Fraction*year15.dummy) + Log.Population, data = DiD_DIW)
summary(did_3.2.2)
# Regression 3.2.3: Kaitz, Control: Population and Year2013
did_3.2.3 = lm(Log.Marginal.Employment ~ (Fraction*year15.dummy) + Log.Population + year13.dummy, data = DiD_DIW)
summary(did_3.2.3)
# Regression 3.2.4: Kaitz, Control: Population, Year2013, Year2012
did_3.2.4 = lm(Log.Marginal.Employment ~ (Fraction*year15.dummy) + Log.Population + year13.dummy + year12.dummy, data = DiD_DIW)
summary(did_3.2.4)
