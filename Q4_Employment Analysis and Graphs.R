##### Employment Graphs #####
## This Quantlet produces output of differenst employment measures ##

## As before we only look at people in our dataset that are actually employed
# Data pre-processing for analysis
merged_all[merged_all$Current.Gross.Labor.Income.in.Euro <= 0] = NA
merged_all[merged_all$Actual.Work.Time.Per.Week <= 0] = NA
Reduced_merged = merged_all[complete.cases(merged_all$Current.Gross.Labor.Income.in.Euro) | complete.cases(merged_all$Actual.Work.Time.Per.Week)]

## We focus our analysis to three different employment statuses (full time, part time, marginal)

# shows all observations of Employment status for each year
Employment.yearly = Reduced_merged %>%
  group_by(Wave) %>%
  summarise(Observations =  n(),
            Full.Employment = length(Employment.Status[as.numeric(Employment.Status) == 7]),
            Part.Employment = length(Employment.Status[as.numeric(Employment.Status) == 8]),
            Marginal.Employment = length(Employment.Status[as.numeric(Employment.Status) == 10])
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



### Output Graphs by year ###

# illustrate Log Employment of all three groups
ggplot(data = Employment.yearly, aes(x = Wave, group = 1 )) +
  geom_line(aes(y = Log.Full.Employment, color = "Full time Employment")) +
  geom_line(aes(y = Log.Part.Employment, color ="Part time Employment")) +
  geom_line(aes(y = Log.Marginal.Employment, color ="Marginal Employment")) + 
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
            Marginal.Employment = length(Employment.Status[as.numeric(Employment.Status) == 10])
  )


