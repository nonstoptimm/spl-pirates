###Graphical Analysis for Employment Rates of Treatment and Control Groups###
#install.packages("ggplot2")
library(ggplot2)

### Combine Information of Q4 and Q5
# We use the averaged data from Q4 for the employment rates and add the Fraction and Keitz Index from Q5
analyze_tc = merge(dbys, Employment.yearly.state)


###Treatment Identification with Keitz Index
## Use Median of Keitz Index in 2013 for identification
#1.Standart treatment: If Kaitz higher than Median the State will be treatmant, otherwise control
#2.Robust treatment: If Kaitz higher than 60% - Percetil will be treatment, if under 40% 

data_selector = function(analyze_tc, wave) {
  select(filter(analyze_tc, Wave == wave), c(Wave, State.of.Residence, Kaitz))
}
# Create subset for 2013
analyze_2013 = data_selector(analyze_tc, 2013)


  #Add Treatmentvariable 1
  analyze_2013$binary_treatment1 = NA
  analyze_2013$binary_treatment1[analyze_2013$Kaitz > median(analyze_2013$Kaitz)] = 1
  analyze_2013$binary_treatment1[is.na(analyze_2013$binary_treatment1)] = 0 
  
      ## Better code for it
      if(analyze_2013$Kaitz > median(analyze_2013$Kaitz)){
        analyze_2013$binary_treatment1 == 1
        }else{ analyze_2013$binary_treatment1 == 0
        }
  
  #Add Treatmentvariable 2
  analyze_tc$binary_treatment2 = NA
  analyze_2013$binary_treatment2[analyze_2013$Kaitz > quantile(analyze_2013$Kaitz, c(0.6))] = 1
  analyze_2013$binary_treatment2[analyze_2013$Kaitz < quantile(analyze_2013$Kaitz, c(0.4))] = 0 

#Merge Treatment Identifiers back to main data  
analyze_tc$binary_treatment1 = analyze_2013$binary_treatment1
analyze_tc$binary_treatment2 = analyze_2013$binary_treatment2



## Aggregate into standard Treatment and Control group
Treatment.analysis1 = analyze_tc %>%
  group_by(Wave, binary_treatment1) %>%
  summarise(Observation =  n(),
    Avg.Log.Full.Employment = mean(Log.Full.Employment, na.rm=TRUE),
    Avg.Log.Part.Employment = mean(Log.Part.Employment, na.rm=TRUE),
    Avg.Log.Marginal.Employment = mean(Log.Marginal.Employment, na.rm=TRUE),
    Avg.Delta.Log.Full.Employment = mean(Delta.Log.Full.Employment, na.rm=TRUE),
    Avg.Delta.Log.Part.Employment = mean(Delta.Log.Part.Employment, na.rm=TRUE),
    Avg.Delta.Log.Marginal.Employment = mean(Delta.Log.Marginal.Employment, na.rm=TRUE)
  )

## Agregate into robust Treatment and control group
Treatment.analysis2 = analyze_tc %>%
  group_by(Wave, binary_treatment2) %>%
  summarise(Observation =  n(),
            Avg.Log.Full.Employment = mean(Log.Full.Employment, na.rm=TRUE),
            Avg.Log.Part.Employment = mean(Log.Part.Employment, na.rm=TRUE),
            Avg.Log.Marginal.Employment = mean(Log.Marginal.Employment, na.rm=TRUE),
            Avg.Delta.Log.Full.Employment = mean(Delta.Log.Full.Employment, na.rm=TRUE),
            Avg.Delta.Log.Part.Employment = mean(Delta.Log.Part.Employment, na.rm=TRUE),
            Avg.Delta.Log.Marginal.Employment = mean(Delta.Log.Marginal.Employment, na.rm=TRUE)
  )


ggplot(data = Treatment.analysis1, aes(x=Wave, y = Avg.Log.Full.Employment, group = factor(binary_treatment1), colour = factor(binary_treatment1))) +
  geom_line() +
  geom_point() +
  labs(title = "Log Employment of binary Treatmentgroups",
       y = "Log Employment level",
       x = "Years") +
  geom_vline(xintercept = 5, color = "red") +
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
