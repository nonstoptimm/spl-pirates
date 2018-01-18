### IMPORT DATA SKRIPT ###
# Install Package "Foreign", preinstalled by default, so just skip
# install.packages("foreign")
# Load required libraries
library(foreign)
library(stringr)
# Make Sure you check your Working Directory so that the code works flawless!
getwd()
# Cleanup-Command - BE CAREFUL WITH IT!
# rm(list=ls(all=TRUE))

### IMPORT, MERGE AND CLEAN ALL DATA ###
# We need two variables: i is to step through the list of years, k is always one digit higher as it reads the second column of the feature selection list (the first column is the label)
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

# Loop through all the years, import the data, merge, clean and label them
for (years in list_years) {
  # Define Current List of import data based on the "i" value
  list_files <- list.files(path=list_dirs[i], pattern = "", full.names=TRUE)
  # Import all the data from the current list with the read.dta-Function for SPSS-Files
  list_import <- lapply(list_files, read.dta)
  # Merge it into one file
  data_merged <- Reduce(function(x, y) merge(x, y, all=FALSE,by.x="persnr",by.y="persnr",all.x =TRUE, all.y =TRUE, check.names=FALSE),list_import,accumulate=F)
  # Label the data with proper names, based on our feature list
  colnames(data_merged) <- soep_selection[,1]
  # This command would cut the x and y values for duplicates but we prefer to use labels
  # colnames(data_merged) <- gsub(".x|.y", "", colnames(data_merged))
  # Get the feature list of the current year
  current_list <- soep_selection[,k]
  # Delete all columns where no data exists (as the surveys differed every year)
  shortlist <- current_list[!is.na(current_list)]
  cleaned <- subset(data_merged, select = shortlist)
  # Assign data_merged to current merge[year]
  assign(list_varnames[i], cleaned)
  # Update our variables for the next round
  i <- i + 1
  k <- k + 1
}