# Load Packages used in Q2
library(stargazer)

### Descriptive Analysis of Data in 2013 ###
## Data Cleaning, Summary Statistics, Output

data_selector = function(input, wave) {
  select(filter(input, Wave == wave), c(Wave, never.Changing.Person.ID, State.of.Residence, Sex, Year.of.Birth,
                                                       Registered.Unemployed, Employment.Status,Labor.Force.Status,
                                                       Actual.Work.Time.Per.Week, Current.Gross.Labor.Income.in.Euro, 
                                                       School.Leaving.Degree, No.Vocational.Degree, Vocational.Degree.Received, College.Degree))
  }

# Create for 2013 by inserting the 2013 wave
sub2013 = data_selector(merged_all, 2013)

##Data Cleaning

## Adjust Labor Force Variable
##Sort out people not in working force anymore
check_labor_force = function(x) { 
  view = c()
  view = c(view, table(x$Labor.Force.Status))
  view = c(view, levels(x$Labor.Force.Status))
  return(view)
}

# Check Labor Force to find out the values
check_labor_force(sub2013)

set_labor_force = function(x, y, z) {
  x$LaborForce_num = NA
  x$LaborForce_num = as.numeric(x$Labor.Force.Status)
  summary(x$LaborForce_num)
  ## Just in case there would be missing values
  x$LaborForce_num[x$LaborForce_num <= y] = NA
  ##Too old -> 4720 NA
  x$LaborForce_num[x$LaborForce_num == z] = NA
  return(x)
}

# Apply Labor Force Status to the sub2013 Dataset
sub2013 = set_labor_force(sub2013, 6, 8)


### Age
age_calculator = function(x, year) {
  x$Age = year - x$Year.of.Birth
  # Implausible Value for Age
  x$Age[x$Age >= year] = NA
  return(x)
}

#Apply the age function to the subset
sub2013 = age_calculator(sub2013, 2013)

## Sex
check_gender = function(x) {
  view = c()
  view = c(view, table(x$Sex))
  view = c(view, levels(x$Sex))
  return(view)
}

# Apply Check Gender function on 2013
check_gender(sub2013)

# Function to correct Sex values
  gender_correction = function(x) {
      x$Sexnum = NA
    # Convert as numeric for analysis
      x$Sexnum = as.numeric(x$Sex) - 7
    # 0 = men, 1 = women 
    # Turn impausible values to NA
      x$Sexnum[x$Sexnum <= -1] = NA
      return(x)
}

# Apply Gender Correction function on sub2013
sub2013 = gender_correction(sub2013)

## Registered Unemployed
check_unemployment = function(x) {
  return(table(x$Registered.Unemployed))
}

# Apply Check Unemployment function on sub2013
check_unemployment(sub2013)

## Employment Status
check_employment_status = function(x) {
  view = c()
  view = c(view, "Employment Status Table: ")
  view = c(view, table(x$Employment.Status))
  view = c(view, "Employment Status Levels: ")
  view = c(view, levels(x$Employment.Status))
  return(view)
}

# Apply Check Employment Status function on sub2013
check_employment_status(sub2013)

## Create function to kick individuals not affected by Minimum Wage
set_na_not_affected = function(x) {
  x$Employment.Status_num = NA
  x$Employment.Status_num = as.numeric(x$Employment.Status)
  x$Employment.Status_num[x$Employment.Status_num == 9] = NA
  x$Employment.Status_num[x$Employment.Status_num == 12] = NA
  x$Employment.Status_num[x$Employment.Status_num == 13] = NA
  x$Employment.Status_num[x$Employment.Status_num == 14] = NA
  x$Employment.Status_num[x$Employment.Status_num == 16] = NA
  x$Employment.Status_num[x$Employment.Status_num == 17] = NA
  x$Employment.Status_num[x$Employment.Status_num == 18] = NA
  return(x)
}

## Apply the function to the sub2013 subset
sub2013 = set_na_not_affected(sub2013)

## Qualification
# High if college degree, middle if vocational degree, low if school degree, non if no school degree
# High = 3, Middle = 2, Low = 1, Non = 0
# Create a function for it

check_qualification = function(x) {
  view = c()
  view = c(view, "SCHOOL LEAVING DEGREE")
  view = c(view, "School.Leaving.Degree Table:")
  view = c(view, table(x$School.Leaving.Degree))
  view = c(view, "School.Leaving.Degree Levels:")
  view = c(view, levels(x$School.Leaving.Degree))
  
  view = c(view, "NO VOCATIONAL DEGREE")
  view = c(view, "No.Vocational.Degree Table:")
  view = c(view, table(x$No.Vocational.Degree))
  view = c(view, "No.Vocational.Degree Levels:")
  view = c(view, levels(x$No.Vocational.Degree))
  
  view = c(view, "VOCATIONAL DEGREE")
  view = c(view, "Vocational.Degree.Received Table:")
  view = c(view, table(x$Vocational.Degree.Received))
  view = c(view, "Vocational.Degree.Received Levels:")
  view = c(view, levels(x$Vocational.Degree.Received))
  
  view = c(view, "COLLEGE DEGREE")
  view = c(view, "College.Degree Table:")
  view = c(view, table(x$College.Degree))
  view = c(view, "College.Degree Table (as numeric):")
  view = c(view, table(as.numeric(x$College.Degree)))
  view = c(view, "College.Degree Levels:")
  view = c(view, levels(x$College.Degree))
  return(view)
}

## Apply the Check Qualification function to the sub2013 subset
check_qualification(sub2013)


rearrange_qualification = function(x) {
    x$qualification = NA
    # School degree
    x$qualification[as.numeric(x$School.Leaving.Degree) == 12] = 0
    x$qualification[as.numeric(x$School.Leaving.Degree) == 13] = 0
    # No Vocational Degree
    x$qualification[as.numeric(x$No.Vocational.Degree) >= 7] = 1
    # Vocational Degree
    x$qualification[as.numeric(x$Vocational.Degree.Received) >= 7] = 2
    # College degree
    x$qualification[as.numeric(x$College.Degree) >= 7] = 3
    return(x)
}

#Apply the Qualification Function to sub 2013
sub2013 = rearrange_qualification(sub2013)

##Income
# Set Values of -2 to 0 -> People that have no Monthly Income
# Create a function for it
set_income = function(x) {
  x$Current.Gross.Labor.Income.in.Euro[(x$Current.Gross.Labor.Income.in.Euro) == -2] = 0
  return(x)
}

#Apply the income function
sub2013 = set_income(sub2013)

##Working Time
#Set -3 to NA -> Implausible Answer
#Set -2 to 0 -> No working time
#Set -1 to NA -> Don't know working time
# Function to correct the working times
set_working_time = function(x) { 
  x$Actual.Work.Time.Per.Week[(x$Actual.Work.Time.Per.Week) == -3] = NA
  x$Actual.Work.Time.Per.Week[(x$Actual.Work.Time.Per.Week) == -2] = 0
  x$Actual.Work.Time.Per.Week[(x$Actual.Work.Time.Per.Week) == -1] = NA
  return(x)
}

#Apply the function to the 2013 Dataset
sub2013 = set_working_time(sub2013)

# Create a function to drop all observations having at least 1 "NA", so only keep complete cases
drop_sub_na = function(x) { x[complete.cases(x), ] }
# Apply this function to a dataset
sub2013noNA = drop_sub_na(sub2013)

### Summary Satistics of Important Variables
# Variables of Interest: Sex, Age, Registered Unemployed, Employment Status, Qualification, Income, Working Time
filter_complete_cases = function(x) { x %>% select(State.of.Residence, qualification, Employment.Status, Registered.Unemployed, Employment.Status_num, Labor.Force.Status, LaborForce_num, Sexnum, Age, Actual.Work.Time.Per.Week, Current.Gross.Labor.Income.in.Euro) }

# Apply Filter Complete Cases Function to a dataset (without NAs)
sumsub2013 = filter_complete_cases(sub2013noNA)

#Calculate Hourly Earnings
calc_hourly_earnings = function(x) {
  x$Hourly.earnings = NA
  x$Hourly.earnings[x$Actual.Work.Time.Per.Week > 0 ] = x$Current.Gross.Labor.Income.in.Euro[x$Actual.Work.Time.Per.Week > 0 ]/(4.3 * x$Actual.Work.Time.Per.Week[sumsub2013$Actual.Work.Time.Per.Week > 0 ])
  return(x)
}

# Apply Calc Hourly Earnings Function to a dataset
sumsub2013 = calc_hourly_earnings(sumsub2013)

## Dummy for Affected by Minimum Wage
# 1 if hourly earnings < 8.50
# Function to make it reusable
dummy_minimum_wage <- function(x) {
  x$Subject.to.minwage = NA
  x$Subject.to.minwage[x$Hourly.earnings < 8.5] = 1
  x$Subject.to.minwage[is.na(x$Subject.to.minwage)] = 0
  return(x)
}

# Apply the function to sumsub2013 and assign it to the same variable
sumsub2013 = dummy_minimum_wage(sumsub2013)

### Function for Means Calculation
calc_means = function(x) { 
  x %>%
  group_by(Employment.Status) %>%
  summarise(NumbOfObservations = n(),
            avg_Age = mean(Age, na.rm=TRUE), 
            avg_Sex = mean(Sexnum, na.rm=TRUE),
            avg_Qualification = mean(qualification, na.rm=TRUE),
            avg_Hourly.earnings = mean(Hourly.earnings),
            avg_monthly.earnings = mean(Current.Gross.Labor.Income.in.Euro, na.rm = TRUE),
            avg_subject.minwage = mean(Subject.to.minwage)
  )
  }

# Apply the Mean Function to the sumsub2013 dataset
Means2013 = calc_means(sumsub2013)

### Output
print_means = function(x) {
  stargazer(t(x), title="Descriptive statistics", type = "text", 
          dep.var.labels = c("Employment Status","Full Time", "Part Time", "Marginal", "Unemployed"),
          covariate.labels = c("n()", "mean age", "mean sex", "mean qualification", "mean hourly earning", "mean monthly earning"))
}

# Apply Print Means
print_means(Means2013)

## Show Kernel Density of the Variables in Means Output
xlabel, xlim1, xlim2, xlim3, xlim4
plot_density = function(input, mode) {
  if(mode == "Age") {
    x =   
  } else if(mode == "hourly earnings")
    
  }
  ggplot(data = input,aes(x = Age ,group = Employment.Status, color = Employment.Status)) +
  geom_line(stat = "density") +
  theme_classic() +
  labs(title = "density of the age seperated by employment status ",
       y = "Density",
       x = "Age") +
  scale_colour_hue(name = "Employment Status",
                   labels = c("Full time","Part Time", "Marginal", "Unemployed"))
  }

plot_density(sumsub2013, "Age")

# Densityplot for Age
ggplot(data = sumsub2013,aes(x = Age, group = Employment.Status, color = Employment.Status )) +
  geom_line(stat = "density") +
  theme_classic() +
  labs(title = "density of the age seperated by employment status ",
       y = "Density",
       x = "Age") +
  scale_colour_hue(name = "Employment Status",
                   labels = c("Full time","Part Time", "Marginal", "Unemployed"))


# Densityplot for Hourly Earnings
ggplot(data = sumsub2013,aes(x = Hourly.earnings, group = Employment.Status, color = Employment.Status )) +
  geom_line(stat = "density") +
  coord_cartesian(xlim = c(0,100),) +
  theme_classic() +
  labs(title = "density of the hourly earnings seperated by employment status ",
       y = "Density",
       x = "hourly earnings") +
  scale_colour_hue(name = "Employment Status",
                   labels = c("Full time","Part Time", "Marginal", "Unemployed"))


#Densityplot for monthly earning
ggplot(data = sumsub2013,aes(x = Current.Gross.Labor.Income.in.Euro ,group = Employment.Status, color = Employment.Status )) +
  geom_line(stat = "density") +
  coord_cartesian(xlim = c(190,4000),ylim = c(0,0.0035)) +
  theme_classic() +
  labs(title = "density of the monthly earnings seperated by employment status ",
       y = "Density",
       x = "monthly earnings") +
  scale_colour_hue(name = "Employment Status",
                   labels = c("Full time","Part Time", "Marginal", "Unemployed"))

#Densityplot for Actual Worktime (per week)
ggplot(data = sumsub2013,aes(x = Actual.Work.Time.Per.Week ,group = Employment.Status, color = Employment.Status )) +
  geom_line(stat = "density") +
  coord_cartesian(xlim = c(4.5,80),ylim = c(0,0.15)) 

# Plot for Gender of every emloyment status
ggplot(data = sumsub2013, aes(x = Employment.Status, fill = as.character(Sexnum))) + geom_bar(position = "fill") +
  theme_classic() +
  labs(title = "Gender for every emloyment status",
       y = "Count",
       x = "Employment Status", fill = "Sex") 
