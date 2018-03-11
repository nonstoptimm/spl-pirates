





# Variable Hourly  earnings

# generate hourly earings variable
merged_all$`Hourly earnings` = merged_all$`Current Gross Labor Income in Euro`/(4 * merged_all$`Actual Work Time Per Week`)
# clean variable for negatives
merged_all$`Hourly earnings`[merged_all$`Hourly earnings` <= 0] = NA
summary(merged_all$`Hourly earnings`)

#generate Kaitz Index for each Individual
merged_all$Kaitz = 8.5/merged_all$`Hourly earnings`[merged_all$`Hourly earnings` > 0 ]

#summed, but makes no sense, 
summary(merged_all$Kaitz)
summary(merged_all$Kaitz[merged_all$`Hourly earnings` > 1])

table(as.numeric(merged_all$`Employment Status`))
table(merged_all$`Employment Status`)

#summary of Kaitz index for full employment
summary(merged_all$Kaitz[as.numeric(merged_all$`Employment Status`) == 7])





# Kaitz for NOT unemployed and seperatet for all waves
summary(merged_all$Kaitz[as.numeric(merged_all$`Employment Status`) != 15 & merged_all$Wave == 2010 ])
summary(merged_all$Kaitz[as.numeric(merged_all$`Employment Status`) != 15 & merged_all$Wave == 2011 ])
summary(merged_all$Kaitz[as.numeric(merged_all$`Employment Status`) != 15 & merged_all$Wave == 2012 ])
summary(merged_all$Kaitz[as.numeric(merged_all$`Employment Status`) != 15 & merged_all$Wave == 2013 ])
summary(merged_all$Kaitz[as.numeric(merged_all$`Employment Status`) != 15 & merged_all$Wave == 2014 ])
summary(merged_all$Kaitz[as.numeric(merged_all$`Employment Status`) != 15 & merged_all$Wave == 2015 ])
summary(merged_all$Kaitz[as.numeric(merged_all$`Employment Status`) != 15 & merged_all$Wave == 2016 ])



# here comes the trash, little darling
Kaitz.df <- c(mean(merged_all$Kaitz[merged_all$Wave == 2010]
                   ,na.rm = TRUE))
  
mean(merged_all$Kaitz,na.rm = TRUE)

group_by(merged_all,`State of Residence`)

Kaitz.df  <-
summarize(group_by(merged_all,`State of Residence`), mean(Kaitz, na.rm = TRUE))

