### Descriptive Analysis of Data in 2013 ###
## Data Cleaning, Summary Statistics, Output

data_selector = function(merged_all, wave) {
  select(filter(merged_all, Wave == wave), c(Wave, never.Changing.Person.ID, State.of.Residence, Sex, Year.of.Birth,
                                                       Registered.Unemployed, Employment.Status,Labor.Force.Status,
                                                       Actual.Work.Time.Per.Week, Current.Gross.Labor.Income.in.Euro, 
                                                       School.Leaving.Degree, No.Vocational.Degree, Vocational.Degree.Received, College.Degree))
  }

# Create for 2013 by inserting the 2013 wave
sub2013 = data_selector(merged_all, 2013)

##Data Cleaning

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

# Apply Labor Force Status to the sub2013 Dataset
sub2013 = adj_labor_force(sub2013)


### Age
age_calculator = function(x, y) {
  x$Age = y - x$Year.of.Birth
  # summary(x$Age)  
  # Implausible Value for Age
  x$Age[x$Age > 2013] = NA
  return(x)
}

#Apply the age function to the subset
sub2013 = age_calculator(sub2013, 2013)

## Sex
table(sub2013$Sex)
levels(sub2013$Sex)
# Function to correct Sex values
  sex_correction = function(x) {
      x$Sexnum = NA
    # Convert as numeric for analysis
      x$Sexnum = as.numeric(x$Sex) - 7
    # 0 = men, 1 = women 
    # Turn impausible values to NA
      x$Sexnum[x$Sexnum <= -1] = NA
      summary(x$Sexnum)
      return(x)
}

# Apply Sex Correction Function to sub2013
sub2013 = sex_correction(sub2013)

##Registered Unemployed
table(sub2013$Registered.Unemployed)

##Employment Status
table(sub2013$Employment.Status)
##Kick individuals not affected by Minimum Wage
levels(sub2013$Employment.Status)
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

summary(sub2013$Employment.Status_num)

## Qualification
#High if college degree, middle if vocational degree, low if school degree, non if no school degree
# High = 3, Middle = 2, Low = 1, Non = 0
# Create a function for it
rearrange_qualification = function(x) {
    x$qualification = NA
    #School degree
    #levels(sub2013$School.Leaving.Degree)
    #table(sub2013$School.Leaving.Degree)
    x$qualification[as.numeric(x$School.Leaving.Degree) == 12] = 0
    x$qualification[as.numeric(x$School.Leaving.Degree) == 13] = 0
    #No Vocational Degree
    # levels(sub2013$No.Vocational.Degree)
    # table(sub2013$No.Vocational.Degree)
    x$qualification[as.numeric(x$No.Vocational.Degree) >= 7] = 1
    #Vocational Degree
    # levels(sub2013$Vocational.Degree.Received)
    # table(sub2013$Vocational.Degree.Received)
    x$qualification[as.numeric(x$Vocational.Degree.Received) >= 7] = 2
    
    #College degree
    # levels(sub2013$College.Degree)
    # table(as.numeric(sub2013$College.Degree))
    # table(sub2013$College.Degree)
    x$qualification[as.numeric(x$College.Degree) >= 7] = 3
  return(x)
}

#Apply the Qualification Function to sub 2013
sub2013 = rearrange_qualification(sub2013)

#Qualification
table(sub2013$qualification)
summary(sub2013$qualification)


##Income
summary(sub2013$Current.Gross.Labor.Income.in.Euro)
# Set Values of -2 to 0 -> People that have no Monthly Income
# Create a function for it
set_income = function(x) {
  x$Current.Gross.Labor.Income.in.Euro[(x$Current.Gross.Labor.Income.in.Euro) == -2] = 0
  return(x)
}

#Apply the income function
sub2013 = set_income(sub2013)

##Working Time
summary(sub2013$Actual.Work.Time.Per.Week)
#Set -3 to NA -> Implausible Answer
#Set -2 to 0 -> No working time
#Set -1 to NA -> Dont know working time
# Function to correct the working times
set_working_time = function(x) { 
  x$Actual.Work.Time.Per.Week[(x$Actual.Work.Time.Per.Week) == -3] = NA
  x$Actual.Work.Time.Per.Week[(x$Actual.Work.Time.Per.Week) == -2] = 0
  x$Actual.Work.Time.Per.Week[(x$Actual.Work.Time.Per.Week) == -1] = NA
  return(x)
}

#Apply the function to the 2013 Dataset
sub2013 = set_working_time(sub2013)

# Drop NAs
drop_sub_na = function(x) { x[complete.cases(x), ] }
sub2013noNA = drop_sub_na(sub2013)

###Summary Satistics of Important Variables
# Variables of Interest: Sex, Age, Registered Unemployed, Employment Status, Qualification, Income, Working Time
sumsub2013 = select(filter(sub2013noNA), c(State.of.Residence, qualification,
                                         Employment.Status, Registered.Unemployed, Employment.Status_num,
                                         Labor.Force.Status, LaborForce_num,
                                         Sexnum, Age, Actual.Work.Time.Per.Week, 
                                         Current.Gross.Labor.Income.in.Euro))

#Calculate Hourly Earnings
sumsub2013$Hourly.earnings = NA
sumsub2013$Hourly.earnings[sumsub2013$Actual.Work.Time.Per.Week > 0 ] = sumsub2013$Current.Gross.Labor.Income.in.Euro[sumsub2013$Actual.Work.Time.Per.Week > 0 ]/(4.3 * sumsub2013$Actual.Work.Time.Per.Week[sumsub2013$Actual.Work.Time.Per.Week > 0 ])


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
means_calculator = function(x) { 
  x %>%
  group_by(Employment.Status) %>%
  summarise(n(),
            avg_Age = mean(Age, na.rm=TRUE), 
            avg_Sex = mean(Sexnum, na.rm=TRUE),
            avg_Qualification = mean(qualification, na.rm=TRUE),
            avg_Hourly.earnings = mean(Hourly.earnings),
            avg_monthly.earnings = mean(Current.Gross.Labor.Income.in.Euro, na.rm = TRUE),
            avg_subject.minwage = mean(Subject.to.minwage)
  )
  }

# Apply the Mean Function to the sumsub2013 dataset
Means = means_calculator(sumsub2013)

###Output
install.packages("stargazer")
library(stargazer)
t(Means)
stargazer(t(Means), title="Descriptive statistics", type = "text", 
          dep.var.labels = c("Employment Status","Full Time", "Part Time", "Marginal", "Unemployed"),
          covariate.labels = c("n()", "mean age", "mean sex", "mean qualification", "mean hourly earning", "mean monthly earning"))

## Show Kernel Density of the Variables in Means Output
# Need to code these with the sumsub2013 dataset