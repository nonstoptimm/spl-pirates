# Quantlet 5
# Load Packages used in Q5
library(dplyr)
# ##### Bites #####
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
  x %>%
  group_by(Wave) %>%
  summarise(Correlation.Fraction.Kaitz = cor(Fraction, Kaitz, use ="all.obs", method="pearson" ))
}

# Apply generate_correlation to dbys dataset
Correlation.Bites.yearly = generate_correlation(dbys)

# Count list_years 1 up each
list_years_up = list_years + 1
# Create Year Periods by pasting list_years and list_years_up together with a "/"-separator and assign it to Correlation.Bites.yearly$Period
Correlation.Bites.yearly$Period = paste(list_years, list_years_up, sep = "/")

# Plot Function for Correlation of Bites (Yearly)
plot_correlation_bites_yearly = function(x) {
ggplot(data = x, aes(x = Period, group = Correlation.Fraction.Kaitz))+
  geom_bar(aes(y = Correlation.Fraction.Kaitz), stat = "identity") + 
  theme_classic() +
  labs(title = "Correlation of Bites",
       y = "Correlation",
       x = "Years") +
  coord_cartesian(ylim = c(0.00,1)) 
}

# Apply plot_correlation_bites_yearly to create bar chart for the states
plot_output_correlation_bites_yearly = plot_correlation_bites_yearly(Correlation.Bites.yearly)
# Save the plot_output_correlation_bites_yearly
# ggsave("plots/plot_output_correlation_bites_yearly.png", plot_output_correlation_bites_yearly)

# Create function to summarize correlation between Fraction and Kaitz
summarize_corr_fk = function(x) {
  x %>%
  group_by(State.of.Residence) %>%
  summarise(Correlation.Fraction.Kaitz = cor(Fraction, Kaitz, use ="all.obs", method="pearson"))
}

# Apply summarize_corr_fk to dbys dataset
Correlation.Bites.State = summarize_corr_fk(dbys)

# Plot Function for Correlation of Bites (States)
plot_correlation_bites_states = function(input) {
  ggplot(data = input, aes(x = State.of.Residence, group = Correlation.Fraction.Kaitz, fill = State.of.Residence))+
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
}

# Apply plot_correlation_bites_states to create bar chart for the states
plot_output_correlation_bites_states = plot_correlation_bites_states(Correlation.Bites.State)
# Save the plot_output_correlation_bites
# ggsave("plots/plot_output_correlation_bites_states.png", plot_output_correlation_bites_states)

### OUTPUT FRACTION and KEITZ  ###

##Density Plots of Kaitz or Fraction Index with aggreagted data
plot_density_aggregated = function(input, index, xlim) {
  if(index == "Kaitz") {
    input$x = input$Kaitz
  } else if(index == "Fraction") {
    input$x = input$Fraction
  } else {
    print("Index must be either Kaitz or Fraction!")
  }
  ggplot(data = input, aes(x = x, group = Wave, color = Wave )) +
    geom_line(stat = "density") + 
    theme_classic() +
    scale_colour_hue(name = "Years") +
    labs(title = paste(index, "Index-Density of States seperated by Years", sep="-"),
         y = "Density",
         x = index) +
    coord_cartesian(xlim = xlim)
}

# Apply plot_density_aggregated to create density plot for Fraction
plot_density_aggr_fraction = plot_density_aggregated(dbys, "Fraction", c(0.1,0.6))
# Save the plot_density_aggr_fraction
# ggsave("plots/plot_aggregated_data_fraction.png", plot_aggregated_data_fraction)

# Function to test normality assumption of fraction or kaitz
shapiro_test = function(input, mode, list_years) {
  if(mode == "Fraction") {
    input$mode = input$Fraction
  } else if (mode == "Kaitz") {
    input$mode = input$Kaitz
  } else {
    print("Mode must be either Fraction or Kaitz!")
  }
  for(years in 1:length(list_years)) {
  test = shapiro.test(input$mode[input$Wave==list_years[years]])  ## for each year and table this loop
  print(list_years[years])
  print(test)
  }
}

# Apply shapiro_test-function to dbys using the years in list_years and fraction index
shapiro_test(dbys, "Fraction", list_years)
  
##Plot-Function for Kaitz or Fraction Indexes over time with aggregated data
plot_aggregated_data = function(input, index) {
  if(index == "Kaitz") {
    input$y = input$Kaitz
  } else if(index == "Fraction") {
    input$y = input$Fraction
  } else {
    print("Index must be either Kaitz or Fraction!")
  }
  ggplot(data = input, aes(x= Wave, y = y, color = State.of.Residence, group = State.of.Residence)) +
  geom_line() +
  theme(panel.background = element_rect(fill = "white")) +
  labs(title = "Index over Years",
       y = paste(index, "Index", sep="-"),
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
}

# Apply the plot_aggregated_data for Fraction index
plot_aggregated_data_fraction = plot_aggregated_data(dbys, "Fraction")
# Save the plot_aggregated_data_fraction
# ggsave("plots/plot_aggregated_data_fraction.png", plot_aggregated_data_fraction)

# Apply the plot_aggregated_data for Kaitz index
plot_aggregated_data_kaitz = plot_aggregated_data(dbys, "Kaitz")
# Save the plot_aggregated_data_kaitz
# ggsave("plots/plot_aggregated_data_kaitz.png", plot_aggregated_data_kaitz)

# Apply plot_density_aggregated to create density plot for Kaitz
plot_density_aggr_kaitz = plot_density_aggregated(dbys, "Kaitz", c(0.43,0.7))
# Save the plot_density_aggr_kaitz
# ggsave("plots/plot_density_aggr_kaitz.png", plot_density_aggr_kaitz)

# Apply Shapiro Test to dbys using shapiro_test and the list of years for kaitz index
shapiro_test(dbys, "Kaitz", list_years)

# Apply the plot_aggregated_data for Kaitz index
plot_aggregated_data_kaitz = plot_aggregated_data(dbys, "Kaitz")
# Save the plot_aggregated_data_kaitz
# ggsave("plots/plot_aggregated_data_kaitz.png", plot_aggregated_data_kaitz)
