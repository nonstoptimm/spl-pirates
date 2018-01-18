### IMPORT DATA SKRIPT ###
# Install Package "Foreign", preinstalled by default, so just skip
# install.packages("foreign")
# Load required libraries
  library(foreign)
  library(stringr)
install.packages("readxl")
    library(readxl)
#library(readstata13)
# Make Sure you check your Working Directory so that the import code works properly!

# Cleanup-Command - BE CAREFUL WITH IT!
# rm(list=ls(all=TRUE))

### IMPORT AND MERGE ALL DATA ###
# Definition of the function "dataprocess"
  dataprocess <- function(i) { 
    i <- 1
  # List all directories within the input data
    list_dirs <- list.dirs(path="input-data", recursive=FALSE)
  # Extract the year name of the directories
    list_years <- str_sub(list_dirs, -4)
  # Create Variable names for every merged year 
    list_varnames <- paste("merged", list_years, sep="")
  # Create a data list for the current year
    
  # Loop through all the years
    for (years in list_years) {
  # Define Current List
      list_files <- list.files(path=list_dirs[i], pattern = "", full.names=TRUE)
  # Import all the data from the current list
      list_import <- lapply(list_files, read.dta)
  # Merge it into one file
      data_merged <- Reduce(function(x, y) merge(x, y, all=FALSE,by.x="persnr",by.y="persnr",all.x =TRUE, all.y =TRUE),list_import,accumulate=F)
  # Assign data_merged to current filename
      assign(list_varnames[i], data_merged)
      i <- i + 1
  }
} 

# Execute Function with the initial value 1
# Therefore, the start year is variable (if you want to start in 2014, you have to type 4)
# If the number is higher than the amount of years stored in the directory, the function will abort automatically
dataprocess(1)
variable_selection("input-data/variable-selection/soep.xlsx")


