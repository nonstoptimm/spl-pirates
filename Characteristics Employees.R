### Descriptive Analysis of Data in 2013 ###
## Data Cleaning, Summary Statistics, Output

sub2013 = select(filter(merged_all, Wave == 2013), c(Wave, never.Changing.Person.ID, State.of.Residence, Sex, Year.of.Birth,
                                                         Registered.Unemployed, Employment.Status,Labor.Force.Status,
                                                         Actual.Work.Time.Per.Week, Current.Gross.Labor.Income.in.Euro, 
                                                         School.Leaving.Degree, No.Vocational.Degree, Vocational.Degree.Received, College.Degree
                                                         ))

##Data Cleaning

## Adjust Labor Force Variable
##Sort out people not in working force anymore
table(sub2013$Labor.Force.Status)
levels(sub2013$Labor.Force.Status)
sub2013$LaborForce_num = NA
sub2013$LaborForce_num = as.numeric(sub2013$Labor.Force.Status)
summary(sub2013$LaborForce_num)

## Just in case there would be missing values
sub2013$LaborForce_num[sub2013$LaborForce_num <= 6] = NA
##To old -> 4720 NA
sub2013$LaborForce_num[sub2013$LaborForce_num == 8] = NA


#testunique = unique(sub2013$never.Changing.Person.ID)

### Age
sub2013$Age = 2013 - sub2013$Year.of.Birth
summary(sub2013$Age)              
sub2013$Age[sub2013$Age > 100] = NA

## Sex
table(sub2013$Sex)
levels(sub2013$Sex)
sub2013$Sexnum = NA
# Convert as numeric for analysis
sub2013$Sexnum = as.numeric(sub2013$Sex) - 7
# 0 = men, 1 = women 

# Turn impausible values to NA
sub2013$Sexnum[sub2013$Sexnum <= -1] = NA
summary(sub2013$Sexnum)


##Registered Unemployed
table(sub2013$Registered.Unemployed)



##Employment Status
table(sub2013$Employment.Status)
##Kick individuals not affected by Minimum Wage
levels(sub2013$Employment.Status)
sub2013$Employment.Status_num = NA
sub2013$Employment.Status_num = as.numeric(sub2013$Employment.Status)

sub2013$Employment.Status_num[sub2013$Employment.Status_num == 9] = NA
sub2013$Employment.Status_num[sub2013$Employment.Status_num == 12] = NA
sub2013$Employment.Status_num[sub2013$Employment.Status_num == 13] = NA
sub2013$Employment.Status_num[sub2013$Employment.Status_num == 14] = NA
sub2013$Employment.Status_num[sub2013$Employment.Status_num == 16] = NA
sub2013$Employment.Status_num[sub2013$Employment.Status_num == 17] = NA
sub2013$Employment.Status_num[sub2013$Employment.Status_num == 18] = NA

summary(sub2013$Employment.Status_num)


## Qualification
#High if college degree, middle if vocational degree, low if school degree, non if no school degree
# High = 3, Middle = 2, Low = 1, Non = 0
sub2013$qualification = NA

#School degree
levels(sub2013$School.Leaving.Degree)
table(sub2013$School.Leaving.Degree)
sub2013$qualification[as.numeric(sub2013$School.Leaving.Degree) == 12] = 0
sub2013$qualification[as.numeric(sub2013$School.Leaving.Degree) == 13] = 0

#No Vocational Degree
levels(sub2013$No.Vocational.Degree)
table(sub2013$No.Vocational.Degree)
sub2013$qualification[as.numeric(sub2013$No.Vocational.Degree) >= 7] = 1

#Vocational Degree
levels(sub2013$Vocational.Degree.Received)
table(sub2013$Vocational.Degree.Received)
sub2013$qualification[as.numeric(sub2013$Vocational.Degree.Received) >= 7] = 2

#College degree
levels(sub2013$College.Degree)
table(as.numeric(sub2013$College.Degree))
table(sub2013$College.Degree)
sub2013$qualification[as.numeric(sub2013$College.Degree) >= 7] = 3

#Qualification
table(sub2013$qualification)
summary(sub2013$qualification)

##Income
summary(sub2013$Current.Gross.Labor.Income.in.Euro)
# Set Values of -2 to 0 -> People that have no Monthly Income
sub2013$Current.Gross.Labor.Income.in.Euro[(sub2013$Current.Gross.Labor.Income.in.Euro) == -2] = 0


##Working Time
summary(sub2013$Actual.Work.Time.Per.Week)
#Set -3 to NA -> Implausible Answer
#Set -2 to 0 -> No working time
#Set -1 to NA -> Dont know working time
sub2013$Actual.Work.Time.Per.Week[(sub2013$Actual.Work.Time.Per.Week) == -3] = NA
sub2013$Actual.Work.Time.Per.Week[(sub2013$Actual.Work.Time.Per.Week) == -2] = 0
sub2013$Actual.Work.Time.Per.Week[(sub2013$Actual.Work.Time.Per.Week) == -1] = NA


# Drop NAs
sub2013noNA = sub2013[complete.cases(sub2013), ]



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


summary(sumsub2013$Hourly.earnings)

table(sumsub2013$Labor.Force.Status)
table(sumsub2013$Employment.Status)

summary(sumsub2013)
#Table for each variable
table(sumsub2013$Sexnum)
summary(sumsub2013$Age)

summary(sumsub2013$Sexnum)
## 53.48% Women

table(sumsub2013$Employment.Status)

Means = sumsub2013 %>%
  group_by(Employment.Status) %>%
  summarise(n(),
            avg_Age = mean(Age, na.rm=TRUE), 
            avg_Sex = mean(Sexnum, na.rm=TRUE),
            avg_Qualification = mean(qualification, na.rm=TRUE),
            avg_Hourly.earnings = mean(Hourly.earnings),
            avg_monthly.earnings = mean(Current.Gross.Labor.Income.in.Euro, na.rm = TRUE)
  )

summary()


###Output
