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
            Part.Time.Employment = length(Employment.Status[as.numeric(Employment.Status) == 8]),
            Marginal.Employment = length(Employment.Status[as.numeric(Employment.Status) == 10])
  )

## Generate different employment measures, note that 1 = FullEmployment, 2 = Part.Time.Employment, 3 = Marginal.Employment

#Absolute Change in Employment of full time employment
Employment.Status$delta.Employment <- c(0, diff(Employment.Status$Full.Employment))
# employment rate of full time employment 
Employment.Status$Employment.Rate = Employment.Status$Full.Employment/Employment.Status$Observation
# log employment rate of full time employment 
Employment.Status$log.Employment.Rate = log(Employment.Status$Employment.Rate)
# % change of full time employment rate 
Employment.Status$delta.E.R <- c(0, diff(Employment.Status$Employment.Rate))
# log % change of full time employment rate 
Employment.Status$log.delta.E.R <- c(0, diff(log(Employment.Status$Employment.Rate)))

# employment rate of part time employment 
Employment.Status$Employment.Rate2 = Employment.Status$Part.Time.Employment/Employment.Status$Observation
# log employment rate of part time employment 
Employment.Status$log.Employment.Rate2 = log(Employment.Status$Employment.Rate2)
# % change of part time employment rate 
Employment.Status$delta.E.R2 <- c(0, diff(Employment.Status$Employment.Rate2))
# log % change of part time employment rate 
Employment.Status$log.delta.E.R2 <- c(0, diff(log(Employment.Status$Employment.Rate2)))

# employment rate of marginal employment 
Employment.Status$Employment.Rate3 = Employment.Status$Marginal.Employment/Employment.Status$Observation
# log employment rate of marginal employment 
Employment.Status$log.Employment.Rate3 = log(Employment.Status$Employment.Rate3)
# % change of marginal employment rate 
Employment.Status$delta.E.R3 <- c(0, diff(Employment.Status$Employment.Rate3))
# log % change of marginal employment rate 
Employment.Status$log.delta.E.R3 <- c(0, diff(log(Employment.Status$Employment.Rate3)))



### Output Graphs ###

# illustrate employment rate of all three groups
ggplot(data = Employment.Status, aes(x = Wave, group = 1 )) +
  geom_line(aes(y = Employment.Rate, color = "Full time Employment")) +
  geom_line(aes(y = Employment.Rate2, color ="Part time Employment")) +
  geom_line(aes(y = Employment.Rate3, color ="Marginal Employment")) + 
  theme_classic() +
  labs(title = "Employment rate over time",
       y = "change of employment rate",
       x = "Years") +
  scale_colour_hue(name = "Status group") +
  geom_vline(xintercept = 6, color = "red") 

# illustrate % change of all three employment rates
ggplot(data = Employment.Status, aes(x = Wave, group = 1 )) +
  geom_line(aes(y = delta.E.R, color = "Full time Employment")) +
  geom_line(aes(y = delta.E.R2, color ="Part time Employment")) +
  geom_line(aes(y =delta.E.R3, color ="Marginal Employment")) + 
  theme_classic() +
  labs(title = "Growth rate of employment over time",
       y = "percentage change of employment growth rate",
       x = "Years") +
  scale_colour_hue(name = "Status group") +
  geom_vline(xintercept = 6, color = "red") 

# illustrate log % change of  all three employment rates
ggplot(data = Employment.Status, aes(x = Wave, group = 1 )) +
  geom_line(aes(y = log.delta.E.R, color = "logarithmic full time Employment")) +
  geom_line(aes(y = log.delta.E.R2, color ="logarithmic part time Employment")) +
  geom_line(aes(y = log.delta.E.R3, color ="logarithmic marginal Employment")) + 
  theme_classic() +
  labs(title = "log change of growth rate over time",
       y = "log percentage change of employment growth rate",
       x = "Years") +
  scale_colour_hue(name = "Status group") +
  geom_vline(xintercept = 6, color = "red")

