#### Data editing ####

### This file is about the editing of values of our dataset.
### Note that variables can have negative values according to SOEP info are seperate information why the values are missing.
### We want to set them as missing and adjust the data accordingly before starting with the analysis

## Values according to SOEP Info:
# https://data.soep.de/soep-core
# -1: no answer /don`t know
# -2: does not apply
# -3 : implausible value
# -4: inadmissable multiple response
# -5: not included in this version of the questionnaire
# -6 : version of questionnaire with modified filtering
# -8: question not part of the survey programm this year
# For simplicity we set all of these values to missing as we do not need to know this detailed information.
# Its also easier to exclude observations with missing values later on for econometric and statistic analysis.

#Use a loop to check for all variables in dataframe 'merged_all'
#class(merged_all)
#length(merged_all)
str(merged_all)

#As we have numeric as well as string variables in the dataframe, we need to treat them seperatly
nums <- sapply(merged_all, is.numeric)
factors <- sapply(merged_all, is.factor)
#Numeric variables
print(nums)
# merged_all[merged_all <= -1] = NA
# merged_all[merged_all == -2] = NA
# merged_all[merged_all == -3] = NA
# merged_all[merged_all == -4] = NA
# merged_all[merged_all == -5] = NA
# merged_all[merged_all == -6] = NA
# merged_all[merged_all == -8] = NA

#Factor variables
###Still have to figure out how to adjust the factor variables in a nice way



### Timm Code
#install.packages("dplyr")
library(dplyr)


#Removes Spaces in Variable Names - Necessary for the dplyr package, which is handy for later analysis of our data
valid_column_names <- make.names(names=names(merged_all), unique=TRUE, allow_ = TRUE)
names(merged_all) <- valid_column_names


# Drop missing values for variables of interest
#merged_all[complete.cases(merged_all$X)]

summary(merged_all$Current.Gross.Labor.Income.in.Euro)

# Drop Current.Gross.Labor.Income.in.Euro and Actual.Work.Time.Per.Week
# Best way would be to code this dynamic
Reduced_merged = merged_all[complete.cases(merged_all$Current.Gross.Labor.Income.in.Euro) & complete.cases(merged_all$Actual.Work.Time.Per.Week)]

# Only for better data keeping
rm (merged2010, merged2011, merged2012, merged2013, merged2014, merged2015, merged2016)

