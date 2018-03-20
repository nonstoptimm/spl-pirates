# Quantlet 8
# Load Packages used in Q8
library(dplyr)
# D&D Analysis by DIW(2017)
# Identification estimation:

## Data Pre Processing for DiD estimation
# Create sub-dataframe with only variables of interest, make input$Wave numeric and log input$Observations as input$Log.Population
data_did_processor = function(input) {
  input = select(filter(input), c(Wave, State.of.Residence, Observations, Fraction, Kaitz, Log.Full.Employment, Log.Part.Employment, Log.Marginal.Employment, binary_treatment1
  ))
  #Generate numeric variable for years, thus convert the variable wave
  input$year = as.numeric(as.character(input$Wave))
  # Population in log
  input$Log.Population = log(input$Observations)
  return(input)
}
# Apply the Function to analyze_tc
DiD_DIW = data_did_processor(analyze_tc)

# Function to creare dummy variables, different input years are possible
generate_dummy = function(input, year) {
  curryear = paste("year", year, sep = "")
  dummycol = ifelse(input$year >= year, 1, 0)
  input[, paste(curryear, "dummy", sep = ".")] = dummycol
  return(input)
}

#Generate dummy for year for indicator when minimum wage was introduced
DiD_DIW = generate_dummy(DiD_DIW, 2015)

##Generate Control Variables
# Control for years:
DiD_DIW = generate_dummy(DiD_DIW, 2013)
DiD_DIW = generate_dummy(DiD_DIW, 2012)

### Regression
## Regression 1: We regress on Regular Employment
# Regression 1.1.1 (baseline): Using Kaitz Index
did_1.1.1 = lm(Log.Full.Employment ~ (Kaitz*year2015.dummy), data = DiD_DIW)
summary(did_1.1.1)
# Regression 1.1.2: Kaitz and Control: Population
did_1.1.2 = lm(Log.Full.Employment ~ (Kaitz*year2015.dummy) + Log.Population, data = DiD_DIW)
summary(did_1.1.2)
# Regression 1.1.3: Kaitz, Control: Population and Year2013
did_1.1.3 = lm(Log.Full.Employment ~ (Kaitz*year2015.dummy) + Log.Population + year2013.dummy, data = DiD_DIW)
summary(did_1.1.3)
# Regression 1.1.4: Kaitz, Control: Population, Year2013, Year2012
did_1.1.4 = lm(Log.Full.Employment ~ (Kaitz*year2015.dummy) + Log.Population + year2013.dummy + year2012.dummy, data = DiD_DIW)
summary(did_1.1.4)

# Regression 1.2.1 (baseline): Using Fraction Index
did_1.2.1 = lm(Log.Full.Employment ~ (Fraction*year2015.dummy), data = DiD_DIW)
summary(did_1.2.1)
# Regression 1.2.2: Kaitz and Control: Population
did_1.2.2 = lm(Log.Full.Employment ~ (Fraction*year2015.dummy) + Log.Population, data = DiD_DIW)
summary(did_1.2.2)
# Regression 1.2.3: Kaitz, Control: Population and Year2013
did_1.2.3 = lm(Log.Full.Employment ~ (Fraction*year2015.dummy) + Log.Population + year2013.dummy, data = DiD_DIW)
summary(did_1.2.3)
# Regression 1.2.4: Kaitz, Control: Population, Year2013, Year2012
did_1.2.4 = lm(Log.Full.Employment ~ (Fraction*year2015.dummy) + Log.Population + year2013.dummy + year2012.dummy, data = DiD_DIW)
summary(did_1.2.4)

## Regression 2: We regress on Part Employment
# Regression 2.1.1 (baseline): Using Kaitz Index
did_2.1.1 = lm(Log.Part.Employment ~ (Kaitz*year2015.dummy), data = DiD_DIW)
summary(did_2.1.1)
# Regression 2.1.2: Kaitz and Control: Population
did_2.1.2 = lm(Log.Part.Employment ~ (Kaitz*year2015.dummy) + Log.Population, data = DiD_DIW)
summary(did_2.1.2)
# Regression 2.1.3: Kaitz, Control: Population and Year2013
did_2.1.3 = lm(Log.Part.Employment ~ (Kaitz*year2015.dummy) + Log.Population + year2013.dummy, data = DiD_DIW)
summary(did_2.1.3)
# Regression 2.1.4: Kaitz, Control: Population, Year2013, Year2012
did_2.1.4 = lm(Log.Part.Employment ~ (Kaitz*year2015.dummy) + Log.Population + year2013.dummy + year2012.dummy, data = DiD_DIW)
summary(did_2.1.4)

# Regression 2.2.1 (baseline): Using Fraction Index
did_2.2.1 = lm(Log.Part.Employment ~ (Fraction*year2015.dummy), data = DiD_DIW)
summary(did_2.2.1)
# Regression 2.2.2: Kaitz and Control: Population
did_2.2.2 = lm(Log.Part.Employment ~ (Fraction*year2015.dummy) + Log.Population, data = DiD_DIW)
summary(did_2.2.2)
# Regression 2.2.3: Kaitz, Control: Population and Year2013
did_2.2.3 = lm(Log.Part.Employment ~ (Fraction*year2015.dummy) + Log.Population + year2013.dummy, data = DiD_DIW)
summary(did_2.2.3)
# Regression 2.2.4: Kaitz, Control: Population, Year2013, Year2012
did_2.2.4 = lm(Log.Part.Employment ~ (Fraction*year2015.dummy) + Log.Population + year2013.dummy + year2012.dummy, data = DiD_DIW)
summary(did_2.2.4)

## Regression 3: We regress on Marginal Employment
# Regression 3.1.1 (baseline): Using Kaitz Index
did_3.1.1 = lm(Log.Marginal.Employment ~ (Kaitz*year2015.dummy), data = DiD_DIW)
summary(did_3.1.1)
# Regression 3.1.2: Kaitz and Control: Population
did_3.1.2 = lm(Log.Marginal.Employment ~ (Kaitz*year2015.dummy) + Log.Population, data = DiD_DIW)
summary(did_3.1.2)
# Regression 3.1.3: Kaitz, Control: Population and Year2013
did_3.1.3 = lm(Log.Marginal.Employment ~ (Kaitz*year2015.dummy) + Log.Population + year2013.dummy, data = DiD_DIW)
summary(did_3.1.3)
# Regression 3.1.4: Kaitz, Control: Population, Year2013, Year2012
did_3.1.4 = lm(Log.Marginal.Employment ~ (Kaitz*year2015.dummy) + Log.Population + year2013.dummy + year2012.dummy, data = DiD_DIW)
summary(did_3.1.4)

# Regression 3.2.1 (baseline): Using Fraction Index
did_3.2.1 = lm(Log.Marginal.Employment ~ (Fraction*year2015.dummy), data = DiD_DIW)
summary(did_3.2.1)
# Regression 3.2.2: Kaitz and Control: Population
did_3.2.2 = lm(Log.Marginal.Employment ~ (Fraction*year2015.dummy) + Log.Population, data = DiD_DIW)
summary(did_3.2.2)
# Regression 3.2.3: Kaitz, Control: Population and Year2013
did_3.2.3 = lm(Log.Marginal.Employment ~ (Fraction*year2015.dummy) + Log.Population + year2013.dummy, data = DiD_DIW)
summary(did_3.2.3)
# Regression 3.2.4: Kaitz, Control: Population, Year2013, Year2012
did_3.2.4 = lm(Log.Marginal.Employment ~ (Fraction*year2015.dummy) + Log.Population + year2013.dummy + year2012.dummy, data = DiD_DIW)
summary(did_3.2.4)