# ##### Bites ##### Q5
# This file is about the estimation and graphical analysis of different bites, namely the Fraction Index and Keitz Index for each German State each year
# The Fraction Index is the ratio of affected individuals by the minimum wage, hence all that earn less than 8.50 Euro per hour.
# Kaitz Index is ration between the minimum legal wage and the average wage


#Note Legend according to SOEP Info:
# https://data.soep.de/soep-core
# -1: no answer /don`t know
# -2: does not apply
# -3 : implausible value
# -4: inadmissable multiple response
# -5: not included in this version of the questionnaire
# -6 : version of questionnaire with modified filtering
# -8: question not part of the survey programm this year

### Data pre-processing for analysis
data_selector = function(merged_all) {
  select(filter(merged_all), c(Wave, never.Changing.Person.ID, State.of.Residence, Employment.Status,Labor.Force.Status,
                               Actual.Work.Time.Per.Week, Current.Gross.Labor.Income.in.Euro 
  ))
}

# Create dataset with only variables of interest
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

# Apply Labor Force Status to the Reduced_merged Dataset
Reduced_merged = adj_labor_force(Reduced_merged)

#We only keep observations in our data that work, hence have an income above 0 and worktime above 0 
Reduced_merged$Current.Gross.Labor.Income.in.Euro[merged_all$Current.Gross.Labor.Income.in.Euro <= 0] = NA
Reduced_merged$Actual.Work.Time.Per.Week[merged_all$Actual.Work.Time.Per.Week <= 0] = NA

# Drop NAs
drop_sub_na = function(x) { x[complete.cases(x), ] }
Reduced_merged_noNA = drop_sub_na(Reduced_merged)


#Compute Variable of hourly earnings
Reduced_merged_noNA$Hourly.earnings = Reduced_merged_noNA$Current.Gross.Labor.Income.in.Euro/(4.3 * Reduced_merged_noNA$Actual.Work.Time.Per.Week)
summary(Reduced_merged_noNA$Hourly.earnings)


#For more exact analyzes drop observations from first and last percentil of hourly earnings
Reduced_merged_noNA$Hourly.earnings[Reduced_merged_noNA$Hourly.earnings > quantile((Reduced_merged_noNA$Hourly.earnings), c(.99)) | Reduced_merged_noNA$Hourly.earnings < quantile((Reduced_merged_noNA$Hourly.earnings), c(.01))] = NA
Reduced_merged_noNA = Reduced_merged_noNA[complete.cases(Reduced_merged_noNA$Hourly.earnings), ]


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
Reduced_merged_noNA = dummy_minimum_wage(Reduced_merged_noNA)


###Collapse Dataset by year and state to dbys (data by year and state)
`dbys` = Reduced_merged_noNA %>%
  group_by(State.of.Residence, Wave) %>%
  summarise(n(),
            Hourly_earnings = mean(Hourly.earnings, na.rm=TRUE), 
            AvgInc = mean(Current.Gross.Labor.Income.in.Euro, na.rm=TRUE),
            Avg.Weekly.Working.Time = mean(Actual.Work.Time.Per.Week, na.rm=TRUE),
            Fraction = mean(Subject.to.minwage)
  )

## Generate Change of Fraction Index
dbys$Delta.Fraction <- c(0, diff(dbys$Fraction))

#Generate Kaitz Index for each state and year
`dbys`$Kaitz = 8.5/`dbys`$Hourly_earnings

## Generate a correlation variable of bites
Correlation.Bites.yearly = dbys %>%
  group_by(Wave) %>%
  summarise(Correlation.Fraction.Kaitz = cor(Fraction, Kaitz, use ="all.obs", method="pearson" ))
# table
Correlation.Bites.yearly$Period = c("2010/2011", "2011/2012", "2012/2013", "2013/2014", "2014/2015", "2015/2016", "2016/2017") 

ggplot(data = Correlation.Bites.yearly, aes(x = Period, group = Correlation.Fraction.Kaitz))+
  geom_bar(aes(y = Correlation.Fraction.Kaitz), stat = "identity") + 
  theme_classic() +
  labs(title = "Correlation of Bites",
       y = "Correlation",
       x = "Years") +
  coord_cartesian(ylim = c(0.83,1)) 


Correlation.Bites.State = dbys %>%
  group_by(State.of.Residence) %>%
  summarise(Correlation.Fraction.Kaitz = cor(Fraction, Kaitz, use ="all.obs", method="pearson" ))
# table 
ggplot(data = Correlation.Bites.State, aes(x = State.of.Residence, group = Correlation.Fraction.Kaitz, fill = State.of.Residence))+
  geom_bar(aes(y = Correlation.Fraction.Kaitz), stat = "identity" ) + 
  theme_classic() +
  labs(title = "Correlation of Bites",
       y = "Correlation",
       x = "State") +
  coord_cartesian(ylim = c(0.3,1)) +
  theme(axis.text.x = element_text(color="white"))+
  scale_fill_hue(name = "States",
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
shapiro.test(dbys$Fraction[dbys$Wave==2015])  ## for each year and table this loop 

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

## Test normality assumption ## Not working yet
for (Wave in dbys) {
  shapiro.test(dbys$Kaitz)
}

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

