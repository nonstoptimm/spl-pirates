rm(list=ls(all=TRUE))


##### Identifying Treatment and Control Groups #####

##Information: We want a group identifier for our treatment group
##Treamtment group had a hourly wage less then 8.50€ before 2015 and are affected of minimum wage according to survey2015 and earning 8.50€.

##We have two variables in our dataset regarding the wage individuals earn if they are affected by the Collective Wage Agreement Minimum Wage
## 

#For merged2015 use variable 'Collective Wage Agreement Minimum Wage#
# Treatment <- merged2015$'Collective Wage Agreement Minimum Wage'

summary(merged2015$`Collective Wage Agreement Minimum Wage`)
summary(merged2015$`Labor Force Status`)

summary(merged2015$`Collective Wage Agreement Minimum Wage`)
summary(merged2015$`Amount of Education Or Training in Years`)

attributes(merged2015$"Minimum Wage Eur")

sapply(merged2015, class)
