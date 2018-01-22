# Analysis of treatment & control group
if("merged2015" %in% current_year) {
# rewrite Sex as numeric and shows the gender variable of the Treatment group (1 for women)
table(merged2015$Sex)
# Sex for treatment group
merged2015$Sexnum15 = NA
merged2015$Sexnum15 = as.numeric(merged2015$Sex) - 7
merged2015$Sexnum15[merged2015$Sexnum15 <= -1] = NA
summary(merged2015$Sexnum15[merged2015$Treatment == 1])
table(merged2015$Sexnum15[merged2015$Treatment == 1])
# Sex for control group
summary(merged2015$Sexnum15[(merged2015$Control_1 == 1 | merged2015$Control_2 == 1 | merged2015$Control_3 == 1)])

# Overveiw amount of Education or Training in years
table(merged2015$`Amount of Education Or Training in Years`)
summary(merged2015$`Amount of Education Or Training in Years`)
# Overview of Educcation or Training of Treatment
merged2015$`Amount of Education Or Training in Years`[merged2015$`Amount of Education Or Training in Years` <= -1] = NA
table(merged2015$`Amount of Education Or Training in Years`[merged2015$Treatment == 1])
summary(merged2015$`Amount of Education Or Training in Years`[merged2015$Treatment == 1])
# Overview of Educcation or Training of control
summary(merged2015$`Amount of Education Or Training in Years`[(merged2015$Control_1 == 1 | merged2015$Control_2 == 1 | merged2015$Control_3 == 
    1)])

# Overview Nationality of Treatment
table(merged2015$Nationality)
# generate new dummy for german or not
merged2015$German = 0
merged2015$German[as.numeric(merged2015$Nationality) == 7] = 1
table(merged2015$German)
# (non)German people in treatment group
table(merged2015$German[merged2015$Treatment == 1])
summary(merged2015$German[merged2015$Treatment == 1])
# (non)German people in treatment group
table(merged2015$German[(merged2015$Control_1 == 1 | merged2015$Control_2 == 1 | merged2015$Control_3 == 1)])
summary(merged2015$German[(merged2015$Control_1 == 1 | merged2015$Control_2 == 1 | merged2015$Control_3 == 1)])

# Overview current gross labor income (class=integer) in Euro
summary(merged2015$`Current Gross Labor Income in Euro`)
# current gross labor income of treatment group
summary(merged2015$`Current Gross Labor Income in Euro`[merged2015$Treatment == 1])
# current gross labor income of control group
summary(merged2015$`Current Gross Labor Income in Euro`[(merged2015$Control_1 == 1 | merged2015$Control_2 == 1 | merged2015$Control_3 == 
    1)])

# Overview actual work time per week (class = numeric) in hours
class(merged2015$`Actual Work Time Per Week`)
summary(merged2015$`Actual Work Time Per Week`)
# actual work time per week of treatment group
summary(merged2015$`Actual Work Time Per Week`[merged2015$Treatment == 1])
# actual work time per week of control group
summary(merged2015$`Actual Work Time Per Week`[(merged2015$Control_1 == 1 | merged2015$Control_2 == 1 | merged2015$Control_3 == 
    1)])

# Overview working expirience (class = numeric) in years
class(merged2015$`Working Experience Full-Time Employment`)
merged2015$working_exp_with_NA = NA
merged2015$working_exp_with_NA[merged2015$`Working Experience Full-Time Employment` >= 0] = merged2015$`Working Experience Full-Time Employment`[merged2015$`Working Experience Full-Time Employment` >= 
    0]
summary(merged2015$working_exp_with_NA)
# working expirience of treatment group
summary(merged2015$working_exp_with_NA[merged2015$Treatment == 1])
# working expirience of control group
summary(merged2015$working_exp_with_NA[(merged2015$Control_1 == 1 | merged2015$Control_2 == 1 | merged2015$Control_3 == 1)])

# Overview of minimum wage in Euro (class =numeric)
class(merged2015$`Minimum Wage`)
summary(merged2015$`Minimum Wage`)
# generate Minimum wage without negative labeled answers (name: Minwagenona)
merged2015$Minwagenona = NA
merged2015$Minwagenona[merged2015$`Minimum Wage` >= 0] = merged2015$`Minimum Wage`[merged2015$`Minimum Wage` >= 0]
summary(merged2015$Minwagenona)
# Minimum wage without negative labeled of treatment group
summary(merged2015$Minwagenona[merged2015$Treatment == 1])
# Minimum wage without negative labeled of control group
summary(merged2015$Minwagenona[(merged2015$Control_1 == 1 | merged2015$Control_2 == 1 | merged2015$Control_3 == 1)])

# overview of Collective wage agreement min wage
class(merged2015$`Collective Wage Agreement Minimum Wage`)
summary(merged2015$`Collective Wage Agreement Minimum Wage`)
merged2015$getminwage = 0
merged2015$getminwage[as.numeric(merged2015$`Collective Wage Agreement Minimum Wage`) == 7] = 1
summary(merged2015$getminwage)
table(merged2015$getminwage)
# Collective wage agreement min wage of treatment group
table(merged2015$getminwage[merged2015$Treatment == 1])
summary(merged2015$getminwage[merged2015$Treatment == 1])
# Collective wage agreement min wage of control group
summary(merged2015$getminwage[(merged2015$Control_1 == 1 | merged2015$Control_2 == 1 | merged2015$Control_3 == 1)])

# overview year of birth
summary(merged2015$`Year of Birth`[merged2015$`Year of Birth` > 0])
# Year of birth of treatment group
summary(merged2015$`Year of Birth`[merged2015$`Year of Birth` > 0 & merged2015$Treatment == 1])
# Year of borth of control group
summary(merged2015$`Year of Birth`[merged2015$`Year of Birth` > 0 & (merged2015$Control_1 == 1 | merged2015$Control_2 == 1 | 
    merged2015$Control_3 == 1)])


# results: treatment group
summary(merged2015$Sexnum15[merged2015$Treatment == 1])
summary(merged2015$`Amount of Education Or Training in Years`[merged2015$Treatment == 1])
summary(merged2015$German[merged2015$Treatment == 1])
summary(merged2015$`Current Gross Labor Income in Euro`[merged2015$Treatment == 1])
summary(merged2015$`Actual Work Time Per Week`[merged2015$Treatment == 1])
summary(merged2015$working_exp_with_NA[merged2015$Treatment == 1])
summary(merged2015$Minwagenona[merged2015$Treatment == 1])
summary(merged2015$getminwage[merged2015$Treatment == 1])
summary(merged2015$`Year of Birth`[merged2015$`Year of Birth` > 0 & merged2015$Treatment == 1])

# charcateristics of the treatment group: Sex 58,93 % women Nationality 80,7 % German Education or Traning in years Median
# 11.50 years, Mean 11.65 years Current gross labor income in € Median 741.0 €, Mean 980.6 € Actual work time per week in
# hours Median 38.00 h, Mean 32.84 € Working expirience in years Median 4.000 years, Mean 9.215 years Minimum wage in €
# Median 8.500 €, Mean 8.508 € Collective wage agreement min wage 28.77 % Year of birth Median, Mean 1977

# control group
summary(merged2015$Sexnum15[(merged2015$Control_1 == 1 | merged2015$Control_2 == 1 | merged2015$Control_3 == 1)])
summary(merged2015$`Amount of Education Or Training in Years`[(merged2015$Control_1 == 1 | merged2015$Control_2 == 1 | merged2015$Control_3 == 
    1)])
summary(merged2015$German[(merged2015$Control_1 == 1 | merged2015$Control_2 == 1 | merged2015$Control_3 == 1)])
summary(merged2015$`Current Gross Labor Income in Euro`[(merged2015$Control_1 == 1 | merged2015$Control_2 == 1 | merged2015$Control_3 == 
    1)])
summary(merged2015$`Actual Work Time Per Week`[(merged2015$Control_1 == 1 | merged2015$Control_2 == 1 | merged2015$Control_3 == 
    1)])
summary(merged2015$working_exp_with_NA[(merged2015$Control_1 == 1 | merged2015$Control_2 == 1 | merged2015$Control_3 == 1)])
summary(merged2015$Minwagenona[(merged2015$Control_1 == 1 | merged2015$Control_2 == 1 | merged2015$Control_3 == 1)])
summary(merged2015$getminwage[(merged2015$Control_1 == 1 | merged2015$Control_2 == 1 | merged2015$Control_3 == 1)])
summary(merged2015$`Year of Birth`[merged2015$`Year of Birth` > 0 & (merged2015$Control_1 == 1 | merged2015$Control_2 == 1 | 
    merged2015$Control_3 == 1)])

# charcateristics of the control group: Sex 66,25% women Nationality 76.81% German Education or Traning in years Median
# 11.50 years, Mean 11.54 years Current gross labor income in € Median 1300 €, Mean 1177 € Actual work time per week in
# hours Median 35.00 h, Mean 32.06 h Working expirience in years Median 7.90 years, Mean 11.48 years Minimum wage in €
# Median 8.500 €, Mean 8.828 € Collective wage agreement min wage 20.34 % Year of birth Median 1972, Mean 1973

print("Executed!")

}