## Kaitz Index ##
# Kaitz Index is ration between the minimum legal wage and the average wage
# We want to compute that for every Bundesland and for every year of our dataset

#Compute Variable of hourly earnings
Reduced_merged$Hourly.earnings = Reduced_merged$Current.Gross.Labor.Income.in.Euro/(4.3 * Reduced_merged$Actual.Work.Time.Per.Week)
summary(Reduced_merged$Hourly.earnings)


###Collapse Dataset by year to dby (data by year)
`dby` = Reduced_merged %>%
  group_by(Wave) %>%
  summarise(n(),
            Hourly_earnings = mean(Hourly.earnings, na.rm=TRUE), 
            AvgInc = mean(Current.Gross.Labor.Income.in.Euro, na.rm=TRUE),
            Avg.Weekly.Working.Time = mean(Actual.Work.Time.Per.Week, na.rm=TRUE)
  )


#Generate Kaitz Index:
`dby`$Kaitz = 8.5/`dby`$Hourly_earnings



###Collapse Dataset by year and state to dbys (data by year and state)
`dbys` = Reduced_merged %>%
  group_by(Wave, State.of.Residence) %>%
  summarise(n(),
            Hourly_earnings = mean(Hourly.earnings, na.rm=TRUE), 
            AvgInc = mean(Current.Gross.Labor.Income.in.Euro, na.rm=TRUE),
            Avg.Weekly.Working.Time = mean(Actual.Work.Time.Per.Week, na.rm=TRUE)
  )


#Generate Kaitz Index:
`dbys`$Kaitz = 8.5/`dbys`$Hourly_earnings


### Process of Generating Binary Treatment Variable
## Median of Kaitz Index from year 2013, to avoid anticipation effects
## Subset of dbys for 2013

dbys2013 = select(filter(dbys, Wave == 2013), c(Wave, State.of.Residence, Kaitz))
#abc = filter(dbys, Wave == 2013) = subset for all variables

#Generate Binary Treatment Identificator1
median(`dbys2013`$Kaitz)
dbys2013$binary_treatment1[dbys2013$Kaitz > median(`dbys2013`$Kaitz)] = 1
dbys2013$binary_treatment1[is.na(dbys2013$binary_treatment1)] = 0


#library(gmodels)
#with(dbys2013, CrossTable(treatment, missing.include=TRUE))

###Generate Robust Binary Treatment Identificator2
## Use Kaitz Index above 60% Percentil for Treatment and below 40% percentil for Control
quantile(dbys2013$Kaitz, c(.40, .60)) 
dbys2013$binary_treatment2[dbys2013$Kaitz > quantile((`dbys2013`$Kaitz), c(.60))] = 1
dbys2013$binary_treatment2[dbys2013$Kaitz < quantile((`dbys2013`$Kaitz), c(.40))] = 0








