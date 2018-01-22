#### SKRIPT "IMPORT-DATA.R" ####
#### This script imports, merges and labels the required data for every year ####

### IMPORT DATA SKRIPT ###
# Load required libraries. They are preinstalled.
  library(foreign)
  library(stringr)
# Make Sure you check your Working Directory so that the code works flawless!
  getwd()
# Otherwise Set the Working Directory
# setwd("/Your/Path/to/Happiness")  

### IMPORT, MERGE AND CLEAN ALL DATA ###
# We need two control variables: 
# i is to step through the list of years, beginning with 1
# k is always one digit higher as it reads the second column of the feature selection list (the first column is the label)
  i <- 1
  k <- 2
# List all directories within the input data, non-recursive
  list_dirs <- list.dirs(path="input-data", recursive=FALSE)
# Extract the year name of the directories, so the last 4 digits
  list_years <- str_sub(list_dirs, -4)
# Create Variable names for every merged year based on the style merged[year]
  list_varnames <- paste("merged", list_years, sep="")
# Load the feature list we cleaned manually in Excel as CSV
  soep_selection <- read.table("variable-selection/soep-feature-selection.csv", header = TRUE, sep = ";", check.names=FALSE)
# Get all Labels, unfiltered
  labels <- soep_selection[,1]
# Create a vector to put object names of all years in it
  datalist <- c()

# Loop through all the years, import the data, merge, clean and label them
  for (years in list_years) {
    # Define Current List of import data based on the "i" value
      list_files <- list.files(path=list_dirs[i], pattern = "", full.names=TRUE)
    # Import all the data from the current list with the read.dta-Function for SPSS-Files
      list_import <- lapply(list_files, read.dta)
    # Merge it into one file
      data_merged <- Reduce(function(x, y) merge(x, y, by='persnr', all.x=TRUE), list_import)
    # Cut the .x and y. values from the merge process, so that we have clean column names
      colnames(data_merged) <- gsub("\\.x|\\.y", "", colnames(data_merged))
    # Get the feature list of the current year
      current_list <- sort(soep_selection[,k])
    # Delete all columns where no data exists (as the surveys differed every year) -> not needed as import function excludes missing values
      # shortlist <- na.omit(current_list)
  
    # ONLY take the data shortlisted for the current year
      cleaned <- data_merged[ ,which(names(data_merged) %in% current_list==TRUE)]
    
    # Select the Label Column and the Variable Column of the current Year
      soep_subcrit <- c(1, k)
    # Subset the Feature list so that only the label and the current year exist
      soep_selection_sub <- soep_selection[soep_subcrit]
    # Delete NA-Values from the list
      soep_selection_sub <- na.omit(soep_selection_sub)
    
    # Create a subset of the clean labels, where all codenames match, to make sure that the labels are correct
      clean_labels <- subset(soep_selection_sub, sort(soep_selection_sub[,2]) ==  sort(names(cleaned)))
    # Order Dataframe alphabetically
      clean_sorted <- cleaned[ , order(names(cleaned))]
    # Order Frame with the Labels based on the ID
      ordered_colnames <- clean_labels[order(clean_labels[2]), ]
    # Label the columns properly
      colnames(clean_sorted) <- ordered_colnames[,1]
    # Assign data_merged to current merge[year]
      assign(list_varnames[i], clean_sorted)
    # Add Year Variable to a list so that we can access all years by a loop  
      datalist <- c(datalist, list_varnames[i])
    # Update our variables for the next round
      i <- i + 1
      k <- k + 1
      
  }
  
# Delete the intermediate variables to clean up the workspace - all except merged[year]
  # rm(list = ls()[!ls() %in% list_varnames])
  
# Delete everything - Cleanup-Command - BE CAREFUL WITH IT!
  # rm(list=ls(all=TRUE))