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
      # Minimum wage earnings are split in 2 variables "Minimum Wage EUR" and "Minimum Wage Cent"
      # The values "-2" and "-1" refer to "Does not apply" and "No anwer" respectively
        #Looking for a nicer way to code this, kind of to ignore the -2 and -1 values and just label them to zero or so.
          merged2015$`Minimum Wage` <- merged2015$`Minimum Wage EUR` + merged2015$`Minimum Wage Cent`/100

        # Create the treatment dummy
          merged2015$`Treatment`[merged2015$`Minimum Wage` > 0 & merged2015$`Minimum Wage` <= 8.5] <- 1
          merged2015$Treatment[is.na(merged2015$Treatment)] <- 0
        # table(get(current_year)$Treatment)
      #842 observations who are earning the minimum wage of 8.50 in 2015
 
  # 2010 - 2014
      # Use a loop function to identify the treatment group for these years
      # We need to generate a hourly earnings variable ourselves
        # Use the variables "Current Gross Labor Income in Euro" and "Contracted Working Hours" or "Hours per Week Actual" or "Actual Work Time Per Week"
          # SET THE CURRENT YEAR
          # We start with the first year of our dataset
          y <- 1  
            for(y in c(1:length(datalist))) {
              current_year <- datalist[y]
              current_data <- get(current_year)
              #deparse(current_year)
              print(current_year)
              print(summary(current_data$`Actual Work Time Per Week`))
              # summary(get(current_year)$`Actual Work Time Per Week`)
              # value "-3" are implausible answers, such as weekly working time over 80hours
              # eliminate the values "-3", "-2", "-1"
              # Also add Weihnachtsgeld and Stuff?
              print(summary(current_data$`Current Gross Labor Income in Euro`))
            }

      # eliminate the values "-2", "-1"
      #Create hourly wage:
          y <- 1 
          for (y in c(1:length(datalist))) {
            # Rewrite the minus values as NA
              current_year <- datalist[y]
              current_data <- get(current_year)
              current_data$`Actual Work Time Per Week`[current_data$`Actual Work Time Per Week` == -1] <- NA
              current_data$`Actual Work Time Per Week`[current_data$`Actual Work Time Per Week` == -2] <- NA
              current_data$`Actual Work Time Per Week`[current_data$`Actual Work Time Per Week` == -3] <- NA
              
              current_data$`Current Gross Labor Income in Euro`[current_data$`Current Gross Labor Income in Euro` == -1] <- NA
              current_data$`Current Gross Labor Income in Euro`[current_data$`Current Gross Labor Income in Euro` == -2] <- NA
            
            # Variable Hourly earnings
              current_data$`Hourly earnings` <- current_data$`Current Gross Labor Income in Euro` / (4 * current_data$`Actual Work Time Per Week`)
              current_data$`Treatment`[current_data$`Hourly earnings` < 8.5] <- 1
              current_data$Treatment[is.na(current_data$Treatment)] <- 0
              table(current_data$Treatment)
          }

#### DER CODE FUNKTIONIERT IRGENDWIE NUR MIT DEM 2014er DATENSATZ? GIBT ES HIER UNTERSCHIEDE? #####
# Control Dummies
  # Use a loop function to identify the treatment group for the years 2010 - 2015
  # We use the generated "hourly earnings" variable
        y <- 1
        for (y in c(1:length(datalist))) {
            current_year <- datalist[y]
            current_data <- get(current_year)
            print(current_year)
          # 1. Control Dummy
            current_data$`Control_1`[current_data$`Hourly earnings` >= 8.5 & current_data$`Hourly earnings` < 9] <- 1
            # merged2014$`Control_1`[merged2014$`Hourly earnings` >= 8.5 & merged2014$`Hourly earnings` < 9] <- 1
            current_data$`Control_1`[is.na(current_data$Control_1)] <- 0
            #table(merged2014$Control_1)
          # 2. Control Dummy
            current_data$`Control_2`[current_data$`Hourly earnings` >= 8.5 & current_data$`Hourly earnings` < 9.5] <- 1
            current_data$`Control_2`[is.na(current_data$Control_2)] <- 0
            #table(merged2014$Control_2)
          # 3. Control Dummy
            current_data$`Control_3`[current_data$`Hourly earnings` >= 8.5 & current_data$`Hourly earnings` < 10] <- 1
            current_data$`Control_3`[is.na(current_data$Control_3)] <- 0
            #table(merged2014$Control_3)
      }
    