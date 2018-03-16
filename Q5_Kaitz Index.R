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

# Create dataset only with variables of interest by applying the data_selector-Function
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

# We only keep observations in our data that work, hence have an income above 0 and worktime above 0 
# Create a function for it
set_working_time_income = function(x) {
  x$Current.Gross.Labor.Income.in.Euro[x$Current.Gross.Labor.Income.in.Euro <= 0] = NA
  x$Actual.Work.Time.Per.Week[x$Actual.Work.Time.Per.Week <= 0] = NA
  return(x)
}

# Apply the function to a dataset
Reduced_merged = set_working_time_income(Reduced_merged)

# Drop NAs
drop_sub_na = function(x) { x[complete.cases(x), ] }
Reduced_merged_noNA = drop_sub_na(Reduced_merged)

# Create Function for computing hourly earnings
#For more exact analyzes drop observations from first and last percentil of hourly earnings
create_hourly_earnings = function(x) {
  x$Hourly.earnings = x$Current.Gross.Labor.Income.in.Euro/(4.3 * x$Actual.Work.Time.Per.Week)
  x$Hourly.earnings[x$Hourly.earnings > quantile((x$Hourly.earnings), c(.99)) | x$Hourly.earnings < quantile((x$Hourly.earnings), c(.01))] = NA
  x = x[complete.cases(x$Hourly.earnings), ]
  return(x)
}
# Apply create_hourly_earnings to a dataset
Reduced_merged_noNA = create_hourly_earnings(Reduced_merged_noNA)

## Dummy for Affected by Minimum Wage
# 1 if hourly earnings < 8.50
# Function to make it reusable
dummy_minimum_wage <- function(x) {
  x$Subject.to.minwage = NA
  x$Subject.to.minwage[x$Hourly.earnings < 8.5] = 1
  x$Subject.to.minwage[is.na(x$Subject.to.minwage)] = 0
  return(x)
}

# Apply the dummy_minimum_wage function to Reduced_merged and assign it to the same variable
Reduced_merged_noNA = dummy_minimum_wage(Reduced_merged_noNA)

###Function to collapse dataset by year and state to dbys (data by year and state)
collapse_dataset = function(x) { x %>%
    group_by(State.of.Residence, Wave) %>%
    summarise(n(),
              Hourly_earnings = mean(Hourly.earnings, na.rm=TRUE), 
              AvgInc = mean(Current.Gross.Labor.Income.in.Euro, na.rm=TRUE),
              Avg.Weekly.Working.Time = mean(Actual.Work.Time.Per.Week, na.rm=TRUE),
              Fraction = mean(Subject.to.minwage)
    )
}

# Apply collapse_dataset to Reduced_merged_noNA dataset
dbys = collapse_dataset(Reduced_merged_noNA)

# Function to generate Index 
generate_index = function(x) {
  ## Generate Change of Fraction Index
  x$Delta.Fraction <- c(0, diff(x$Fraction))
  # Generate Kaitz Index for each state and year
  x$Kaitz = 8.5/x$Hourly_earnings
  # Generate Change of Kaitz Index
  x$Delta.Kaitz <- c(0, diff(x$Kaitz))
  return(x)
}

# Apple the generate_index Function to dbys dataset
dbys = generate_index(dbys)

## Generate a correlation variable of bites 
generate_correlation = function(x) {
  dbys %>%
  group_by(Wave) %>%
  summarise(Correlation.Fraction.Kaitz = cor(Fraction, Kaitz, use ="all.obs", method="pearson" ))
}

# Apply generate_correlation to dbys dataset
Correlation.Bites.yearly = generate_correlation(dbys)

# Count list_years 1 up each
list_years_up = list_years + 1
# Create Year Periods by pasting list_years and list_years_up together with a "/"-separator and assign it to Correlation.Bites.yearly$Period
Correlation.Bites.yearly$Period = paste(list_years, list_years_up, sep = "/")


ggplot(data = Correlation.Bites.yearly, aes(x = Period, group = Correlation.Fraction.Kaitz))+
  geom_bar(aes(y = Correlation.Fraction.Kaitz), stat = "identity") + 
  theme_classic() +
  labs(title = "Correlation of Bites",
       y = "Correlation",
       x = "Years") +
  coord_cartesian(ylim = c(0.00,1)) 


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
  coord_cartesian(ylim = c(0.0,1)) +
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

# Function to test normality assumption of fraction
shapiro_test = function(input, list_years) {
  for(years in 1:length(list_years)) {
  test = shapiro.test(input$Fraction[input$Wave==list_years[years]])  ## for each year and table this loop
  print(list_years[years])
  print(test)
  }
}

# Apply shapiro_test-function to dbys using the years in list_years
shapiro_test(dbys, list_years)
  
##Fraction Indexes over time with aggregated data
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

#Test normality assumption of Kaitz
shapiro_test = function(input, list_years) {
  i = 1
  for(years in list_years) {
    test = shapiro.test(input$Kaitz[input$Wave==list_years[i]])  ## for each year and table this loop
    print(list_years[i])
    print(test)
    i = i + 1
  }
}

# Apply Shapiro Test to dbys using shapiro_test and the list of years
shapiro_test(dbys, list_years)

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

