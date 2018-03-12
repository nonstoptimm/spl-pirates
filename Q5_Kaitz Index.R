##### Bites #####


## Kaitz Index ##
#This file is about the estimation and graphical analysis of the Keitz Indexes for each German State and year
#Kaitz Index is ration between the minimum legal wage and the average wage
#We only keep observations in our data that work, hence have an income above 0 and worktime above 0 
#Note Legend according to SOEP Info:
# https://data.soep.de/soep-core
# -1: no answer /don`t know
# -2: does not apply
# -3 : implausible value
# -4: inadmissable multiple response
# -5: not included in this version of the questionnaire
# -6 : version of questionnaire with modified filtering
# -8: question not part of the survey programm this year
merged_all[merged_all$Current.Gross.Labor.Income.in.Euro <= 0] = NA
merged_all[merged_all$Actual.Work.Time.Per.Week <= 0] = NA
Reduced_merged = merged_all[complete.cases(merged_all$Current.Gross.Labor.Income.in.Euro) | complete.cases(merged_all$Actual.Work.Time.Per.Week)]


#Compute Variable of hourly earnings
Reduced_merged$Hourly.earnings = Reduced_merged$Current.Gross.Labor.Income.in.Euro/(4.3 * Reduced_merged$Actual.Work.Time.Per.Week)
summary(Reduced_merged$Hourly.earnings)

#For more exact analyzes drop observations from first and last percentil of hourly earnings
Reduced_merged$Hourly.earnings[Reduced_merged$Hourly.earnings > quantile((Reduced_merged$Hourly.earnings), c(.99)) | Reduced_merged$Hourly.earnings < quantile((Reduced_merged$Hourly.earnings), c(.01))] = NA
Reduced_merged = Reduced_merged[complete.cases(Reduced_merged$Hourly.earnings)]



###Collapse Dataset by year and state to dbys (data by year and state)
`dbys` = Reduced_merged %>%
  group_by(Wave, State.of.Residence) %>%
  summarise(n(),
            Hourly_earnings = mean(Hourly.earnings, na.rm=TRUE), 
            AvgInc = mean(Current.Gross.Labor.Income.in.Euro, na.rm=TRUE),
            Avg.Weekly.Working.Time = mean(Actual.Work.Time.Per.Week, na.rm=TRUE)
  )


#Generate Kaitz Index for each state and year:
`dbys`$Kaitz = 8.5/`dbys`$Hourly_earnings



### OUTPUT ###


##Density Plots of Kaitz Index with aggreagted Data
ggplot(data = dbys, aes(x = Kaitz, group = Wave, color = Wave )) +
  geom_line(stat = "density") + 
  theme_classic() +
  scale_colour_hue(name = "Years") +
  labs(title = "Density of the Kaitz Index of States seperated by Years ",
       y = "Density",
       x = "Kaitz") +
  coord_cartesian(xlim = c(0.43,0.7))


##Kaitz Indexes over time with aggregated Data

ggplot(data = dbys, aes(x= Wave, y = Kaitz, color = State.of.Residence, group = State.of.Residence)) +
  geom_line() +
  theme(panel.background = element_rect(fill = "white")) +
  labs(title = "Kaitz Index over Years",
       y = "Kaitz-Index",
       x = "Years") +
  geom_vline(xintercept = 5, color = "red") +
  theme_classic() +
  coord_cartesian(xlim = c(1.6,7)) +
  scale_colour_hue(name = "States",
                   labels = c("Schleswig-Holstein", 
                              "Hamburg", 
                              "Lower Saxony", 
                              "Bremen", 
                              "North-RhineWestfalia", 
                              "Hessen", 
                              "Rheinland-Pfalz", 
                              "Baden-Wuerttemberg", 
                              "Bavaria", 
                              "Saarland", 
                              "Berlin", 
                              "Brandenburg", 
                              "Mecklemburg-Vorpommern", 
                              "Saxony", 
                              "Saxony-Anhalt", 
                              "Thuringia")) 





