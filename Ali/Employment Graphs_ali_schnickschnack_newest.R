##### Employment Graphs #####
## This Quantlet produces output of differenst employment measures ##

## As before we only look at people in our dataset that are actually employed
# Data pre-processing for analysis
merged_all[merged_all$Current.Gross.Labor.Income.in.Euro <= 0] = NA
merged_all[merged_all$Actual.Work.Time.Per.Week <= 0] = NA
Reduced_merged = merged_all[complete.cases(merged_all$Current.Gross.Labor.Income.in.Euro) | complete.cases(merged_all$Actual.Work.Time.Per.Week)]

## We focus our analysis to three different employment statuses (full time, part time, marginal)

# shows all observations of Employment status for each year
Employment.Status = Reduced_merged %>%
  group_by(Wave) %>%
  summarise(Observation =  n(),
            Full.Employment = length(Employment.Status[as.numeric(Employment.Status) == 7]),
            Part.Employment = length(Employment.Status[as.numeric(Employment.Status) == 8]),
            Marginal.Employment = length(Employment.Status[as.numeric(Employment.Status) == 10])
  )

## Generate different employment measures, note that 1 = FullEmployment, 2 = Part.Time.Employment, 3 = Marginal.Employment

#Log Values of full time employment
Employment.Status$Log.Full.Employment = log(Employment.Status$Full.Employment)
# % change of log full time employment
Employment.Status$Delta.Log.Full.Employment <- c(0, diff(Employment.Status$Log.Full.Employment))


#Log Values of part time employment
Employment.Status$Log.Part.Employment = log(Employment.Status$Part.Employment)
# % change of log part time employment
Employment.Status$Delta.Log.Part.Employment <- c(0, diff(Employment.Status$Log.Part.Employment))


#Log Values of marginal employment
Employment.Status$Log.Marginal.Employment = log(Employment.Status$Marginal.Employment)
# % change of log marginal employment
Employment.Status$Delta.Log.Marginal.Employment <- c(0, diff(Employment.Status$Log.Marginal.Employment))




### Output Graphs ###

# illustrate Log Employment of all three groups
ggplot(data = Employment.Status, aes(x = Wave, group = 1 )) +
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
ggplot(data = Employment.Status, aes(x = Wave, group = 1 )) +
  geom_line(aes(y = Delta.Log.Full.Employment, color = "Full time Employment")) +
  geom_line(aes(y = Delta.Log.Part.Employment, color ="Part time Employment")) +
  geom_line(aes(y = Delta.Log.Marginal.Employment, color ="Marginal Employment")) + 
  theme_classic() +
  labs(title = "Growth rate of employment over time",
       y = "percentage change of employment growth rate",
       x = "Years") +
  scale_colour_hue(name = "Status group") +
  geom_vline(xintercept = 5, color = "red") 


