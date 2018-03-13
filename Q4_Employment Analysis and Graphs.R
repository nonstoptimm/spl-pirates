##### Employment Graphs #####
## This Quantlet produces output of differenst employment measures ##

## We look at people in our dataset that are either full time employed, part time employed, marginal employed or not employed.
# Data pre-processing for analysis
data_selector = function(merged_all) {
  select(filter(merged_all), c(Wave, never.Changing.Person.ID, State.of.Residence, Employment.Status,Labor.Force.Status,
                                             Actual.Work.Time.Per.Week, Current.Gross.Labor.Income.in.Euro 
                                             ))
}

# Create for dataframe with only variables of interest
Reduced_merged = data_selector(merged_all)


## Adjust Labor Force Variable
##Sort out people not in working force anymore
adj_labor_force = function(x) { 
  table(x$Labor.Force.Status)
  levels(x$Labor.Force.Status)
  x$LaborForce_num = NA
  x$LaborForce_num = as.numeric(x$Labor.Force.Status)
  summary(x$LaborForce_num)
  ## Just in case there would be missing values
  x$LaborForce_num[x$LaborForce_num <= 6] = NA
  ##Too old -> 4720 NA
  x$LaborForce_num[x$LaborForce_num == 8] = NA
  return(x)
}

# Apply Labor Force Status to the sub2013 Dataset
Reduced_merged = adj_labor_force(Reduced_merged)


# Adjust Work Time Variable
summary(merged_all$Actual.Work.Time.Per.Week)
Reduced_merged$Actual.Work.Time.Per.Week[Reduced_merged$Actual.Work.Time.Per.Week <= -3] = NA
Reduced_merged$Actual.Work.Time.Per.Week[Reduced_merged$Actual.Work.Time.Per.Week == -2] = 0
Reduced_merged$Actual.Work.Time.Per.Week[Reduced_merged$Actual.Work.Time.Per.Week == -1] = NA
summary(Reduced_merged$Actual.Work.Time.Per.Week)

table(Reduced_merged$Employment.Status)

# Adjust Data Income
summary(merged_all$Current.Gross.Labor.Income.in.Euro)
Reduced_merged$Current.Gross.Labor.Income.in.Euro[Reduced_merged$Current.Gross.Labor.Income.in.Euro <= -3] = NA
summary(Reduced_merged$Current.Gross.Labor.Income.in.Euro)


# Drop NAs
drop_sub_na = function(x) { x[complete.cases(x), ] }
Reduced_merged_noNA = drop_sub_na(Reduced_merged)



## We focus our analysis to three different employment statuses (full time, part time, marginal) and the non employeed
table(Reduced_merged_noNA$Employment.Status)
levels(Reduced_merged_noNA$Employment.Status)

# shows all observations of Employment status for each year
Employment.yearly = Reduced_merged %>%
  group_by(Wave) %>%
  summarise(Observations =  n(),
            Full.Employment = length(Employment.Status[as.numeric(Employment.Status) == 7]),
            Part.Employment = length(Employment.Status[as.numeric(Employment.Status) == 8]),
            Marginal.Employment = length(Employment.Status[as.numeric(Employment.Status) == 10]),
            Not.Employment = length(Employment.Status[as.numeric(Employment.Status) == 15])
  )

## Generate different employment measures, Log Employment and % change of Log Employment and Employment Rates

#Log Values of full time employment
Employment.yearly$Log.Full.Employment = log(Employment.yearly$Full.Employment)
# % change of log full time employment
Employment.yearly$Delta.Log.Full.Employment <- c(0, diff(Employment.yearly$Log.Full.Employment))
#Employment Rate of full time employment
Employment.yearly$Full.Employment.Rate = Employment.yearly$Full.Employment / Employment.yearly$Observations



#Log Values of part time employment
Employment.yearly$Log.Part.Employment = log(Employment.yearly$Part.Employment)
# % change of log part time employment
Employment.yearly$Delta.Log.Part.Employment <- c(0, diff(Employment.yearly$Log.Part.Employment))
#Employment Rate of part time employment
Employment.yearly$Part.Employment.Rate = Employment.yearly$Part.Employment / Employment.yearly$Observations


#Log Values of marginal employment
Employment.yearly$Log.Marginal.Employment = log(Employment.yearly$Marginal.Employment)
# % change of log marginal employment
Employment.yearly$Delta.Log.Marginal.Employment <- c(0, diff(Employment.yearly$Log.Marginal.Employment))
#Employment Rate of marginal employment
Employment.yearly$Marginal.Employment.Rate = Employment.yearly$Marginal.Employment / Employment.yearly$Observations


#Log Values of not employment
Employment.yearly$Log.Not.Employment = log(Employment.yearly$Not.Employment)
# % change of log not employment
Employment.yearly$Delta.Log.Not.Employment <- c(0, diff(Employment.yearly$Log.Not.Employment))
#Employment Rate of not employment
Employment.yearly$Not.Employment.Rate = Employment.yearly$Not.Employment / Employment.yearly$Observations


### Output Graphs by year ###

# illustrate Log Employment of all three groups
ggplot(data = Employment.yearly, aes(x = Wave, group = 1 )) +
  geom_line(aes(y = Log.Full.Employment, color = "Full time Employment")) +
  geom_line(aes(y = Log.Part.Employment, color ="Part time Employment")) +
  geom_line(aes(y = Log.Marginal.Employment, color ="Marginal Employment")) + 
  geom_line(aes(y = Log.Not.Employment, color ="Marginal Employment")) +
  theme_classic() +
  labs(title = "Log Employment over time",
       y = "Log Employment",
       x = "Years") +
  scale_colour_hue(name = "Status group") +
  geom_vline(xintercept = 5, color = "red") 

# illustrate % change of Log Employment of all three groups
ggplot(data = Employment.yearly, aes(x = Wave, group = 1 )) +
  geom_line(aes(y = Delta.Log.Full.Employment, color = "Full time Employment")) +
  geom_line(aes(y = Delta.Log.Part.Employment, color ="Part time Employment")) +
  geom_line(aes(y = Delta.Log.Marginal.Employment, color ="Marginal Employment")) + 
  geom_line(aes(y = Delta.Log.Not.Employment, color ="Marginal Employment")) +
  theme_classic() +
  labs(title = "Growth rate of employment over time",
       y = "percentage change of employment growth rate",
       x = "Years") +
  scale_colour_hue(name = "Status group") +
  geom_vline(xintercept = 5, color = "red") 


# illustrate Emplyoment Rates of all three groups
ggplot(data = Employment.yearly, aes(x = Wave, group = 1 )) +
  geom_line(aes(y = Full.Employment.Rate, color = "Full time Employment")) +
  geom_line(aes(y = Part.Employment.Rate, color ="Part time Employment")) +
  geom_line(aes(y = Marginal.Employment.Rate, color ="Marginal Employment")) +
  geom_line(aes(y = Not.Employment.Rate, color ="Marginal Employment")) +
  theme_classic() +
  labs(title = "Employment rates over time",
       y = "Employment rate",
       x = "Years") +
  scale_colour_hue(name = "Status group") +
  geom_vline(xintercept = 5, color = "red") 





### Employment Rates by year and state
Employment.yearly.state = Reduced_merged %>%
  group_by(Wave, State.of.Residence) %>%
  summarise(Observations =  n(),
            Full.Employment = length(Employment.Status[as.numeric(Employment.Status) == 7]),
            Part.Employment = length(Employment.Status[as.numeric(Employment.Status) == 8]),
            Marginal.Employment = length(Employment.Status[as.numeric(Employment.Status) == 10]),
            Not.Employment = length(Employment.Status[as.numeric(Employment.Status) == 15])
  )

## Generate different employment measures, Log Employment and % change of Log Employment and Employment Rates

#Log Values of full time employment
Employment.yearly.state$Log.Full.Employment = log(Employment.yearly.state$Full.Employment)
# % change of log full time employment
Employment.yearly.state$Delta.Log.Full.Employment <- c(0, diff(Employment.yearly.state$Log.Full.Employment))
#Employment Rate of full time employment
Employment.yearly.state$Full.Employment.Rate = Employment.yearly.state$Full.Employment / Employment.yearly.state$Observations



#Log Values of part time employment
Employment.yearly.state$Log.Part.Employment = log(Employment.yearly.state$Part.Employment)
# % change of log part time employment
Employment.yearly.state$Delta.Log.Part.Employment <- c(0, diff(Employment.yearly.state$Log.Part.Employment))
#Employment Rate of part time employment
Employment.yearly.state$Part.Employment.Rate = Employment.yearly.state$Part.Employment / Employment.yearly.state$Observations


#Log Values of marginal employment
Employment.yearly.state$Log.Marginal.Employment = log(Employment.yearly.state$Marginal.Employment)
# % change of log marginal employment
Employment.yearly.state$Delta.Log.Marginal.Employment <- c(0, diff(Employment.yearly.state$Log.Marginal.Employment))
#Employment Rate of marginal employment
Employment.yearly.state$Marginal.Employment.Rate = Employment.yearly.state$Marginal.Employment / Employment.yearly.state$Observations


#Log Values of not employment
Employment.yearly.state$Log.Not.Employment = log(Employment.yearly.state$Not.Employment)
# % change of log not employment
Employment.yearly.state$Delta.Log.Not.Employment <- c(0, diff(Employment.yearly.state$Log.Not.Employment))
#Employment Rate of not employment
Employment.yearly.state$Not.Employment.Rate = Employment.yearly.state$Not.Employment / Employment.yearly.state$Observations




#### OUTPUT Graphs for each state of the employment variables over time #####
