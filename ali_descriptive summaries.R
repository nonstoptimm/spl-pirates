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





# Diff in Diff Gleichung
# log(y_it) = bite_i + D_t^MW * beta_l + Sum_t D_t^year * gamma_t + alpha + e_i,t
#Control for State: Average labor income in different periods

#Ansatz:
library(foreign)
mydata = read.dta("http://dss.princeton.edu/training/Panel101.dta")
mydata$time = ifelse(mydata$year >= 1994, 1, 0)
mydata$treated = ifelse(mydata$country == "E" |
                          mydata$country == "F" |
                          mydata$country == "G", 1, 0)
#Binary treatment
mydata$did = mydata$time * mydata$treated
didreg = lm(y ~ treated + time + time*treated, data = mydata)
summary(didreg)



