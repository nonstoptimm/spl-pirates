# ##### Bites #####
# This file is about the estimation and graphical analysis of different bites, namely the Fraction Index and Keitz Index for each German State each year
# The Fraction Index is the ratio of affected individuals by the minimum wage, hence all that earn less than 8.50 Euro per hour.
# Kaitz Index is ration between the minimum legal wage and the average wage

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

# Delete observations with missing values for Income and Working Time
Reduced_merged = merged_all[complete.cases(merged_all$Current.Gross.Labor.Income.in.Euro) | complete.cases(merged_all$Actual.Work.Time.Per.Week)]


#Compute Variable of hourly earnings
Reduced_merged$Hourly.earnings = Reduced_merged$Current.Gross.Labor.Income.in.Euro/(4.3 * Reduced_merged$Actual.Work.Time.Per.Week)
summary(Reduced_merged$Hourly.earnings)

#For more exact analyzes drop observations from first and last percentil of hourly earnings
Reduced_merged$Hourly.earnings[Reduced_merged$Hourly.earnings > quantile((Reduced_merged$Hourly.earnings), c(.99)) | Reduced_merged$Hourly.earnings < quantile((Reduced_merged$Hourly.earnings), c(.01))] = NA
Reduced_merged = Reduced_merged[complete.cases(Reduced_merged$Hourly.earnings)]

## Dummy for Affected by Minimum Wage
# 1 if hourly earnings < 8.50
# Function to make it reusable
dummy_minimum_wage <- function(x) {
  x$Subject.to.minwage = NA
  x$Subject.to.minwage[x$Hourly.earnings < 8.5] = 1
  x$Subject.to.minwage[is.na(x$Subject.to.minwage)] = 0
  return(x)
}

# Apply the function to Reduced_merged and assign it to the same variable
Reduced_merged = dummy_minimum_wage(Reduced_merged)


###Collapse Dataset by year and state to dbys (data by year and state)
`dbys` = Reduced_merged %>%
  group_by(Wave, State.of.Residence) %>%
  summarise(n(),
            Hourly_earnings = mean(Hourly.earnings, na.rm=TRUE), 
            AvgInc = mean(Current.Gross.Labor.Income.in.Euro, na.rm=TRUE),
            Avg.Weekly.Working.Time = mean(Actual.Work.Time.Per.Week, na.rm=TRUE),
            Fraction = mean(Subject.to.minwage)
  )

## Generate Change of Fraction Index
#dbys$Delta.Fraction <- c(0, diff(dbys$Fraction))
#How?

#Generate Kaitz Index for each state and year:
`dbys`$Kaitz = 8.5/`dbys`$Hourly_earnings
#Generate Change of Kaitz Index
#dbys$Delta.Kaitz 
#How?

## Generate a correlation variable of bites
Correlation.Bites = cor(dbys$Fraction, dbys$Kaitz, use="all.obs", method="pearson") ## Maybe also Correlation in each year


### OUTPUT FRACTION and KEITZ  ###

##Density Plots of Fraction Index with aggreagted Data
ggplot(data = dbys, aes(x = Fraction, group = Wave, color = Wave )) +
  geom_line(stat = "density") + 
  theme_classic() +
  scale_colour_hue(name = "Years") +
  labs(title = "Density of the Fraction Index of States seperated by Years ",
       y = "Density",
       x = "Fraction") +
  coord_cartesian(xlim = c(0.1,0.6))

#Test normality assumption
shapiro.test(dbys$Fraction[dbys$Wave==2015])  ## for each year and table this

##Fraction Indexes over time with aggregated Data

ggplot(data = dbys, aes(x= Wave, y = Fraction, color = State.of.Residence, group = State.of.Residence)) +
  geom_line() +
  theme(panel.background = element_rect(fill = "white")) +
  labs(title = "Fraction Index over Years",
       y = "Fraction-Index",
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


##Density Plots of Kaitz Index with aggreagted Data
ggplot(data = dbys, aes(x = Kaitz, group = Wave, color = Wave )) +
  geom_line(stat = "density") + 
  theme_classic() +
  scale_colour_hue(name = "Years") +
  labs(title = "Density of the Kaitz Index of States seperated by Years ",
       y = "Density",
       x = "Kaitz") +
  coord_cartesian(xlim = c(0.43,0.7))

## Test normality assumption
shapiro.test(dbys$Kaitz[dbys$Wave==2016])  ## for each year and table this

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


### Correlations between Bites

