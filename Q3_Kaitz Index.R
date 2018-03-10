## Kaitz Index ##
# Kaitz Index is ration between the minimum legal wage and the average wage
# We want to compute that for every Bundesland and for every year of our dataset

#Compute Variable of hourly earnings
merged_all$Hourly.earnings = merged_all$Current.Gross.Labor.Income.in.Euro/(4 * merged_all$Actual.Work.Time.Per.Week)
summary(merged_all$Hourly.earnings)


#Generate a counter, which is helpful later on. Tells us how many of our observations were aggregated in the group by function
merged_all$Counter = 1
sum(merged_all$Counter)


###Collapse Dataset by year to dby (data by year)
`dby` = merged_all %>%
  group_by(Wave) %>%
  summarise(sum(merged_all$Counter),
            Hourly_earnings = mean(Hourly.earnings, na.rm=TRUE), 
            AvgInc = mean(Current.Gross.Labor.Income.in.Euro, na.rm=TRUE),
            Avg.Weekly.Working.Time = mean(Actual.Work.Time.Per.Week, na.rm=TRUE)
  )


#Generate Alternative Kaitz Index1:
`dby`$Kaitz_alt1 = 8.5/`dby`$Hourly_earnings



###Collapse Dataset by year and state to dbys (data by year and state)
`dbys` = merged_all %>%
  group_by(Wave, State.of.Residence) %>%
  summarise(sum(merged_all$Counter),
            Hourly_earnings = mean(Hourly.earnings, na.rm=TRUE), 
            AvgInc = mean(Current.Gross.Labor.Income.in.Euro, na.rm=TRUE),
            Avg.Weekly.Working.Time = mean(Actual.Work.Time.Per.Week, na.rm=TRUE)
  )


#Generate Alternative Kaitz Index1:
`dbys`$Kaitz_alt1 = 8.5/`dbys`$Hourly_earnings


### Process of Generating Treatment Variable
## Median of Kaitz Index from year 2013, to avoid anticipation effects
## Subset of dbys for 2013

dbys2013 = select(filter(dbys, Wave == 2013), c(Wave, State.of.Residence, Kaitz_alt1))
#abc = filter(dbys, Wave == 2013) = subset for all variables

#Generate Treatment Identificator
median(`dbys2013`$Kaitz_alt1)
dbys2013$treatment[dbys2013$Kaitz_alt1 > median(`dbys2013`$Kaitz_alt1)] = 1
dbys2013$treatment[is.na(dbys2013$treatment)] = 0


#library(gmodels)
#with(dbys2013, CrossTable(treatment, missing.include=TRUE))

###Generate Robust Treatment Identificator
## Use Kaitz Index above 60% Percentil for Treatment and below 40% percentil for Control
quantile(dbys2013$Kaitz_alt1, c(.40, .60)) 
dbys2013$treatment2[dbys2013$Kaitz_alt1 > quantile((`dbys2013`$Kaitz_alt1), c(.60))] = 1
dbys2013$treatment2[dbys2013$Kaitz_alt1 < quantile((`dbys2013`$Kaitz_alt1), c(.40))] = 0


