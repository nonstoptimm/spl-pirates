## Want aggreagted summaries for each Bundesland per year

## Drop observations with missing values for aggregate
levels(merged_all$Employment.Status)
summary(merged_all$Employment.Status)

## Geschlecht
  table(merged_all$Sex)
  levels(merged_all$Sex)
  merged_all$Sexnum = NA
# Convert as numeric for analysis
  merged_all$Sexnum = as.numeric(merged_all$Sex) - 7
  # 0 = men, 1 = women 
  summary(merged_all$Sexnum)
# Turn impausible values to NA
  merged_all$Sexnum[merged_all$Sexnum <= -1] = NA
  summary(merged_all$Sexnum)
# Summary by year
  tapply(merged_all$Sexnum, merged_all$Wave, summary)
  
  length(merged_all$Sexnum[merged_all$Sexnum == 0])
  length(merged_all$Sexnum[merged_all$Sexnum == 1])
  
  `dby` = merged_all %>%
    group_by(Wave) %>%
    summarise(n(),
              Hourly_earnings = mean(Hourly.earnings, na.rm=TRUE), 
              AvgInc = mean(Current.Gross.Labor.Income.in.Euro, na.rm=TRUE),
              Avg.Weekly.Working.Time = mean(Actual.Work.Time.Per.Week, na.rm=TRUE),
              AvgSex = mean(Sexnum, na.rm = TRUE),
    )
  
  
  
  
  
  test = merged_all
  test1 = merged_all[complete.cases(merged_all$Sexnum)]
  test2 = merged_all[complete.cases(merged_all$Hourly.earnings)]
  test3 = merged_all[complete.cases(merged_all$Sexnum) & complete.cases(merged_all$Hourly.earnings)]
  
  `dby` = test3 %>%
    group_by(Wave) %>%
    summarise(n(),
              Hourly_earnings = mean(Hourly.earnings, na.rm=TRUE), 
              AvgInc = mean(Current.Gross.Labor.Income.in.Euro, na.rm=TRUE),
              Avg.Weekly.Working.Time = mean(Actual.Work.Time.Per.Week, na.rm=TRUE),
              AvgSex = mean(Sexnum, na.rm = TRUE),
    )

  
  `dbys2` = test3 %>%
    group_by(Wave, State.of.Residence) %>%
    summarise(n(),
              Hourly_earnings = mean(Hourly.earnings, na.rm=TRUE), 
              AvgInc = mean(Current.Gross.Labor.Income.in.Euro, na.rm=TRUE),
              Avg.Weekly.Working.Time = mean(Actual.Work.Time.Per.Week, na.rm=TRUE)
  
    )
  
  summary(merged_all$Sexnum)
  summary(merged_all$Hourly.earnings)
  summary(merged_all$Current.Gross.Labor.Income.in.Euro)
  
  
  
  
##For year 2013, which we use for binary Treatment


tapply(merged_all$Sex, merged_all$Wave, summary)

table(merged_all$Sex[merged_all$Wave == 2013])

levels(merged_all$Sex)






# Diff in Diff Gleichung
# log(y_it) = bite_i + D_t^MW * beta_l + Sum_t D_t^year * gamma_t + alpha + e_i,t

#Ansatz:
library(foreign)
mydata = read.dta("http://dss.princeton.edu/training/Panel101.dta")
mydata$time = ifelse(mydata$year >= 1994, 1, 0)
mydata$treated = ifelse(mydata$country == "E" |
                          mydata$country == "F" |
                          mydata$country == "G", 1, 0)
#Binary treatment
mydata$did = mydata$time * mydata$treated
didreg = lm(y ~ treated + time + did, data = mydata)
summary(didreg)

