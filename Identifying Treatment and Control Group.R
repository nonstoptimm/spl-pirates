#install.packages("gmodels")
#library(gmodels)
#rm(list=ls(all=TRUE))


##### Identifying Treatment and Control Groups #####

#Information: We want to create a dummy to indentify our treatment group as well as a dummy to identify our control group
  #Treatment individuals: Earned less than the minimum wage of 8.50€ before 2015 and 8.50€ starting in 2015
    #If individual belongs to treatment, they have the value of 1 in the treatment variable, otherwise 0

  #Control individuals: Earn slightly more than the treatment
    # We create various controll dummies with different wages, as we might fear that the sample size is too small for the control group.
    # 1. Control dummy earns between 8.50€ and 9€
    # 2. Control dummy earns between 8.50€ and 9.50€
    # 3. Control dummy earns between 8.50€ and 10€
      # If in given wage, dummy value is 1, otherwise 0

#Treatment dummy
  # 2015
    # Create a unique minimum wage earnings variable
      #Minimum wage earnings are split in 2 variables "Minimum Wage EUR" and "Minimum Wage Cent"
      #The values "-2" and "-1" refer to "Does not apply" and "No anwer" respectively
    
      #Looking for a nicer way to code this, kind of to ignore the -2 and -1 values and just label them to zero or so.
      merged2015$`Minimum Wage` <- merged2015$`Minimum Wage EUR` + merged2015$`Minimum Wage Cent`/100
      
    # Create the treatment dummy
      merged2015$`Treatment`[merged2015$`Minimum Wage` > 0 & merged2015$`Minimum Wage` <= 8.5] <- 1
      merged2015$Treatment[is.na(merged2015$Treatment)] <- 0
      table(merged2015$Treatment)
      #842 observations who are earning the minimum wage of 8.50 in 2015
 
  # 2010 - 2014
      # Use a loop function to identify the treatment group for these years
      # We need to generate a hourly earnings variable ourselves
        # Use the variables "Current Gross Labor Income in Euro" and "Contracted Working Hours" or "Hours per Week Actual" or "Actual Work Time Per Week"
          #summary(merged2014$`Contracted Working Hours`)
          #summary(merged2014$`Hours per Week Actual`)
            #Don't need the upper two
      summary(merged2014$`Actual Work Time Per Week`)
        # value "-3" are implausible answers, such as weekly working time over 80hours
        # eliminate the values "-3", "-2", "-1"
           # Also add Weihnachtsgeld and Stuff?
      summary(merged2014$`Current Gross Labor Income in Euro`)
        # eliminate the values "-2", "-1"
      
      #Create hourly wage:
        #for 2014
          # Rewrite the minus values as NA
          merged2014$`Actual Work Time Per Week`[merged2014$`Actual Work Time Per Week` == -1] <- NA
          merged2014$`Actual Work Time Per Week`[merged2014$`Actual Work Time Per Week` == -2] <- NA
          merged2014$`Actual Work Time Per Week`[merged2014$`Actual Work Time Per Week` == -3] <- NA
        
          merged2014$`Current Gross Labor Income in Euro`[merged2014$`Current Gross Labor Income in Euro` == -1] <- NA
          merged2014$`Current Gross Labor Income in Euro`[merged2014$`Current Gross Labor Income in Euro` == -2] <- NA
        # Variable Hourly earnings
        merged2014$`Hourly earnings` <-merged2014$`Current Gross Labor Income in Euro` / (4 * merged2014$`Actual Work Time Per Week`)
        
        merged2014$`Treatment`[merged2014$`Hourly earnings` < 8.5] <- 1
        merged2014$Treatment[is.na(merged2014$Treatment)] <- 0
        table(merged2014$Treatment)
        
        #Loop
          ## All of the above code for 2014 should be coded as a loop which just runs through for every year 2010 - 2014
        
# Control Dummies
  # Use a loop function to identify the treatment group for the years 2010 - 2015
  # We use the generated "hourly earnings" variable
  
  #for 2014
    # 1. Control Dummy
      merged2014$`Control_1`[merged2014$`Hourly earnings` >= 8.5 & merged2014$`Hourly earnings` < 9] <- 1
      merged2014$`Control_1`[is.na(merged2014$Control_1)] <- 0
        #table(merged2014$Control_1)
    # 2. Control Dummy
      merged2014$`Control_2`[merged2014$`Hourly earnings` >= 8.5 & merged2014$`Hourly earnings` < 9.5] <- 1
      merged2014$`Control_2`[is.na(merged2014$Control_2)] <- 0
        #table(merged2014$Control_2)
    # 3. Control Dummy
      merged2014$`Control_3`[merged2014$`Hourly earnings` >= 8.5 & merged2014$`Hourly earnings` < 10] <- 1
      merged2014$`Control_3`[is.na(merged2014$Control_3)] <- 0
        #table(merged2014$Control_3)     
  # Loop
     ## All of the above code for 2014 should be coded as a loop which just runs through for every year 2010 - 2015
    

