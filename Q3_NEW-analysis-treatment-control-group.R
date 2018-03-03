# Analysis of treatment & control group
y = 1
for (years in c(datalist)) {
  current_year = datalist[y]
  current_data = get(current_year)
  print(current_year)
  # rewrite Sex as numeric and shows the gender variable of the Treatment group (1 for women)
  table(current_data$Sex)
  # Sex for treatment group
  current_data$Sexnum = NA
  current_data$Sexnum = as.numeric(current_data$Sex) - 7
  current_data$Sexnum[current_data$Sexnum <= -1] = NA
  summary(current_data$Sexnum[current_data$Treatment == 1])
  table(current_data$Sexnum[current_data$Treatment == 1])
  # Sex for control group
  summary(current_data$Sexnum[(current_data$Control_3 == 1)])
  
  # overview year of birth
  summary(current_data$`Year of Birth`[current_data$`Year of Birth` > 0])
  # Year of birth of treatment group
  summary(current_data$`Year of Birth`[current_data$`Year of Birth` > 0 & current_data$Treatment == 1])
  # Year of borth of control group
  summary(current_data$`Year of Birth`[current_data$`Year of Birth` > 0 & current_data$Control_3 == 1])
  
  # Overveiw amount of Education or Training in years
  table(current_data$`Amount of Education Or Training in Years`)
  summary(current_data$`Amount of Education Or Training in Years`)
  # Overview of Educcation or Training of Treatment
  current_data$`Amount of Education Or Training in Years`[current_data$`Amount of Education Or Training in Years` <= -1] = NA
  table(current_data$`Amount of Education Or Training in Years`[current_data$Treatment == 1])
  summary(current_data$`Amount of Education Or Training in Years`[current_data$Treatment == 1])
  # Overview of Educcation or Training of control
  summary(current_data$`Amount of Education Or Training in Years`[(current_data$Control_3 == 1)])
  
  # Overview Nationality of Treatment
  table(current_data$Nationality)
  # generate new dummy for german or not
  current_data$German = 0
  current_data$German[as.numeric(current_data$Nationality) == 7] = 1
  table(current_data$German)
  # (non)German people in treatment group
  table(current_data$German[current_data$Treatment == 1])
  summary(current_data$German[current_data$Treatment == 1])
  # (non)German people in treatment group
  table(current_data$German[(current_data$Control_3 == 1)])
  summary(current_data$German[(current_data$Control_3 == 1)])
  
  # Overview current gross labor income (class=integer) in Euro
  summary(current_data$`Current Gross Labor Income in Euro`)
  # current gross labor income of treatment group
  summary(current_data$`Current Gross Labor Income in Euro`[current_data$Treatment == 1])
  # current gross labor income of control group
  summary(current_data$`Current Gross Labor Income in Euro`[(current_data$Control_3 == 1)])
  
  # Overview actual work time per week (class = numeric) in hours
  class(current_data$`Actual Work Time Per Week`)
  summary(current_data$`Actual Work Time Per Week`)
  # actual work time per week of treatment group
  summary(current_data$`Actual Work Time Per Week`[current_data$Treatment == 1])
  # actual work time per week of control group
  summary(current_data$`Actual Work Time Per Week`[(current_data$Control_3 == 1)])
  
  # Overview working expirience (class = numeric) in years
  class(current_data$`Working Experience Full-Time Employment`)
  current_data$working_exp_with_NA = NA
  current_data$working_exp_with_NA[current_data$`Working Experience Full-Time Employment` >= 0] = current_data$`Working Experience Full-Time Employment`[current_data$`Working Experience Full-Time Employment` >= 0]
  summary(current_data$working_exp_with_NA)
  # working expirience of treatment group
  summary(current_data$working_exp_with_NA[current_data$Treatment == 1])
  # working expirience of control group
  summary(current_data$working_exp_with_NA[(current_data$Control_3 == 1)])
  
  if("merged2015" == current_year) {
    # Overview of minimum wage in Euro (class =numeric)
    class(current_data$`Minimum Wage`)
    summary(current_data$`Minimum Wage`)
    # generate Minimum wage without negative labeled answers (name: Minwagenona)
    current_data$Minwagenona = NA
    current_data$Minwagenona[current_data$`Minimum Wage` >= 0] = current_data$`Minimum Wage`[current_data$`Minimum Wage` >= 0]
    summary(current_data$Minwagenona)
    # Minimum wage without negative labeled of treatment group
    summary(current_data$Minwagenona[current_data$Treatment == 1])
    # Minimum wage without negative labeled of control group
    summary(current_data$Minwagenona[(current_data$Control_3 == 1)])
    
    # overview of Collective wage agreement min wage
    class(current_data$`Collective Wage Agreement Minimum Wage`)
    summary(current_data$`Collective Wage Agreement Minimum Wage`)
    current_data$getminwage = 0
    current_data$getminwage[as.numeric(current_data$`Collective Wage Agreement Minimum Wage`) == 7] = 1
    summary(current_data$getminwage)
    table(current_data$getminwage)
    # Collective wage agreement min wage of treatment group
    table(current_data$getminwage[current_data$Treatment == 1])
    summary(current_data$getminwage[current_data$Treatment == 1])
    # Collective wage agreement min wage of control group
    summary(current_data$getminwage[(current_data$Control_3 == 1)])
  }
  
  # results: treatment group
  summary(current_data$Sexnum[current_data$Treatment == 1])
  summary(current_data$`Amount of Education Or Training in Years`[current_data$Treatment == 1])
  summary(current_data$German[current_data$Treatment == 1])
  summary(current_data$`Current Gross Labor Income in Euro`[current_data$Treatment == 1])
  summary(current_data$`Actual Work Time Per Week`[current_data$Treatment == 1])
  summary(current_data$working_exp_with_NA[current_data$Treatment == 1])
  summary(current_data$`Year of Birth`[current_data$`Year of Birth` > 0 & current_data$Treatment == 1])
  if("merged2015" == current_year) {
    summary(current_data$Minwagenona[current_data$Treatment == 1])
    summary(current_data$getminwage[current_data$Treatment == 1])
  }
  
  # charcateristics of the treatment group: Sex 58,93 % women Nationality 80,7 % German Education or Traning in years Median
  # 11.50 years, Mean 11.65 years Current gross labor income in € Median 741.0 €, Mean 980.6 € Actual work time per week in
  # hours Median 38.00 h, Mean 32.84 € Working expirience in years Median 4.000 years, Mean 9.215 years Minimum wage in €
  # Median 8.500 €, Mean 8.508 € Collective wage agreement min wage 28.77 % Year of birth Median, Mean 1977
  
  # control group
  summary(current_data$Sexnum[(current_data$Control_3 == 1)])
  summary(current_data$`Amount of Education Or Training in Years`[(current_data$Control_3 == 1)])
  summary(current_data$German[(current_data$Control_3 == 1)])
  summary(current_data$`Current Gross Labor Income in Euro`[(current_data$Control_3 == 1)])
  summary(current_data$`Actual Work Time Per Week`[(current_data$Control_3 == 1)])
  summary(current_data$working_exp_with_NA[(current_data$Control_3 == 1)])
  # doesnt exist: summary(current_data$Minwagenona[(current_data$Control_1 == 1 | current_data$Control_2 == 1 | current_data$Control_3 == 1)])
  # doesnt exist: summary(current_data$getminwage[(current_data$Control_1 == 1 | current_data$Control_2 == 1 | current_data$Control_3 == 1)])
  summary(current_data$`Year of Birth`[current_data$`Year of Birth` > 0 & (current_data$Control_3 == 1)])
  
  # charcateristics of the control group: Sex 66,25% women Nationality 76.81% German Education or Traning in years Median
  # 11.50 years, Mean 11.54 years Current gross labor income in € Median 1300 €, Mean 1177 € Actual work time per week in
  # hours Median 35.00 h, Mean 32.06 h Working expirience in years Median 7.90 years, Mean 11.48 years Minimum wage in €
  # Median 8.500 €, Mean 8.828 € Collective wage agreement min wage 20.34 % Year of birth Median 1972, Mean 1973
  
  # Assign it to the correct year
  assign(current_year, current_data)
  
  y = y + 1
}