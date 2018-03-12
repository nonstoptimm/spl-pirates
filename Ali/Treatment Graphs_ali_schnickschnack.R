###Graphical Analysis for Employment Rates of Treatment and Control States###
install.packages("ggplot2")
library(ggplot2)

#Compute Variable of hourly earnings
Reduced_merged$Hourly.earnings = Reduced_merged$Current.Gross.Labor.Income.in.Euro/(4.3 * Reduced_merged$Actual.Work.Time.Per.Week)
summary(Reduced_merged$Hourly.earnings)

#For more exact analyzes drop observations from first and last percentil of hourly earnings
Reduced_merged$Hourly.earnings[Reduced_merged$Hourly.earnings > quantile((Reduced_merged$Hourly.earnings), c(.99)) | Reduced_merged$Hourly.earnings < quantile((Reduced_merged$Hourly.earnings), c(.01))] = NA
Reduced_merged = Reduced_merged[complete.cases(Reduced_merged$Hourly.earnings)]




### Process of Generating Binary Treatment Variable
## Median of Kaitz Index from year 2013, to avoid anticipation effects
## Subset of dbys for 2013

dbys2013 = select(filter(dbys, Wave == 2013), c(Wave, State.of.Residence, Kaitz))
#abc = filter(dbys, Wave == 2013) = subset for all variables

#Generate Binary Treatment Identificator1
median(`dbys2013`$Kaitz)
dbys2013$binary_treatment1[dbys2013$Kaitz > median(`dbys2013`$Kaitz)] = 1
dbys2013$binary_treatment1[is.na(dbys2013$binary_treatment1)] = 0


###Generate Robust Binary Treatment Identificator2
## Use Kaitz Index above 60% Percentil for Treatment and below 40% percentil for Control
quantile(dbys2013$Kaitz, c(.40, .60)) 
dbys2013$binary_treatment2[dbys2013$Kaitz > quantile((`dbys2013`$Kaitz), c(.60))] = 1
dbys2013$binary_treatment2[dbys2013$Kaitz < quantile((`dbys2013`$Kaitz), c(.40))] = 0






##Collapse Dataframe by year and State for Employment Variables as well as Wage Variables

analyze_tc = Reduced_merged %>%
  group_by(Wave, State.of.Residence) %>%
  summarise(Observation =  n(),
            Hourly_earnings = mean(Hourly.earnings, na.rm=TRUE), 
            AvgInc = mean(Current.Gross.Labor.Income.in.Euro, na.rm=TRUE),
            Avg.Weekly.Working.Time = mean(Actual.Work.Time.Per.Week, na.rm=TRUE),
            Full.Employment = length(Employment.Status[as.numeric(Employment.Status) == 7]),
            Part.Time.Employment = length(Employment.Status[as.numeric(Employment.Status) == 8]),
            Marginal.Employment = length(Employment.Status[as.numeric(Employment.Status) == 10])
  )


# employment rate of full time employment 
analyze_tc$Employment.Rate = analyze_tc$Full.Employment/analyze_tc$Observation
# log employment rate of full time employment 
analyze_tc$log.Employment.Rate = log(analyze_tc$Employment.Rate)
# % change of full time employment rate 
analyze_tc$delta.E.R <- c(0, diff(analyze_tc$Employment.Rate))
# log % change of full time employment rate 
analyze_tc$log.delta.E.R <- c(0, diff(log(analyze_tc$Employment.Rate)))

# employment rate of part time employment 
analyze_tc$Employment.Rate2 = analyze_tc$Part.Time.Employment/analyze_tc$Observation
# log employment rate of part time employment 
analyze_tc$log.Employment.Rate2 = log(analyze_tc$Employment.Rate2)
# % change of part time employment rate 
analyze_tc$delta.E.R2 <- c(0, diff(analyze_tc$Employment.Rate2))
# log % change of part time employment rate 
analyze_tc$log.delta.E.R2 <- c(0, diff(log(analyze_tc$Employment.Rate2)))

# employment rate of marginal employment 
analyze_tc$Employment.Rate3 = analyze_tc$Marginal.Employment/analyze_tc$Observation
# log employment rate of marginal employment 
analyze_tc$log.Employment.Rate3 = log(analyze_tc$Employment.Rate3)
# % change of marginal employment rate 
analyze_tc$delta.E.R3 <- c(0, diff(analyze_tc$Employment.Rate3))
# log % change of marginal employment rate 
analyze_tc$log.delta.E.R3 <- c(0, diff(log(analyze_tc$Employment.Rate3)))


#Generate Kaitz Index:
analyze_tc$Kaitz = 8.5/`analyze_tc`$Hourly_earnings


#Add Treatmentvariable 1
analyze_tc$binary_treatment1 <- dbys2013$binary_treatment1

#Add Treatmentvariable 2
analyze_tc$binary_treatment2 <- dbys2013$binary_treatment2


Treatment.analysis1 = analyze_tc %>%
  group_by(Wave, binary_treatment1) %>%
  summarise(Observation =  n(),
    Employment.Rate = mean(Employment.Rate, na.rm=TRUE),
    Growth.Rate = mean(delta.E.R, na.rm = TRUE)
  )

ggplot(data = Treatment.analysis1, aes(x=Wave, y = Employment.Rate, group = factor(binary_treatment1), colour = factor(binary_treatment1))) +
  geom_line() +
  geom_point() +
  labs(title = "Employment Rate of binary Treatmentgroups",
       y = "Employment Rate",
       x = "Years") +
  geom_vline(xintercept = 6, color = "red") +
  theme_classic() +
  scale_colour_discrete(name = "Treatment",
                   labels = c("lower Kaitz than median", "higher Kaitz than median")) +
  coord_cartesian(xlim = c(1.6,7))

ggplot(data = Treatment.analysis1, aes(x=Wave, y = Growth.Rate, group = factor(binary_treatment1), colour = factor(binary_treatment1))) +
  geom_line() +
  geom_point() +
  labs(title = "Growth Rate of Employment of binary Treatmentgroups",
       y = "Growth Rate",
       x = "Years") +
  geom_vline(xintercept = 6, color = "red") +
  theme_classic() +
  scale_colour_discrete(name = "Treatment",
                        labels = c("lower Kaitz than median", "higher Kaitz than median")) +
  coord_cartesian(xlim = c(1.6,7), ylim = c(-0.04,0.04))

Treatment.analysis2 = analyze_tc %>%
  group_by(Wave, binary_treatment2) %>%
  summarise(Observation =  n(),
            Employment.Rate = mean(Employment.Rate, na.rm=TRUE),
            Growth.Rate = mean(delta.E.R, na.rm = TRUE)
  )

ggplot(data = Treatment.analysis2, aes(x=Wave, y = Employment.Rate, group = factor(binary_treatment2), colour = factor(binary_treatment2))) +
  geom_line() +
  geom_point() +
  labs(title = "Employment Rate of binary Treatmentgroups",
       y = "Employment Rate",
       x = "Years") +
  geom_vline(xintercept = 6, color = "red") +
  theme_classic() +
  scale_colour_discrete(name = "Treatment",
                        labels = c("lower Kaitz than median", "higher Kaitz than median")) +
  coord_cartesian(xlim = c(1.6,7))

ggplot(data = complete.cases(Treatment.analysis2), aes(x=Wave, y = Growth.Rate, group = factor(binary_treatment2), colour = factor(binary_treatment2))) +
  geom_line() +
  geom_point() +
  labs(title = "Growth Rate of Employment of binary Treatmentgroups",
       y = "Growth Rate",
       x = "Years") +
  geom_vline(xintercept = 6, color = "red") +
  theme_classic() +
  scale_colour_discrete(name = "Treatment",
                        labels = c("lower Kaitz than median", "higher Kaitz than median")) +
  coord_cartesian(xlim = c(1.6,7), ylim = c(-0.04,0.04))
