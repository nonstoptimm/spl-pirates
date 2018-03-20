# Quantlet 1 - IMPORT DATA SKRIPT
# Load Packages used in Q1
library(foreign)
library(stringr)
library(data.table)
# Make Sure you check your Working Directory so that the code works flawless!
getwd()
# Otherwise Set the Working Directory -> setwd('/Your/Path/to/Happiness')

### IMPORT, MERGE AND CLEAN ALL DATA ### We need two iterators: i is to step through the list of years, beginning with
### k is always one digit higher than i as it reads the second column of the feature selection list (the first column is the label)
i = 1 # iterator to step through the list of years
k = 2 # iterator to step through the columns in variable list
# List all directories within the input data, non-recursive
list_dirs = list.dirs(path = "input-data", recursive = FALSE)
# Extract the year name of the directories, so the last 4 digits and make it numeric
list_years = str_sub(list_dirs, -4)
# Create Variable names for every merged year based on the style merged[year]
list_varnames = paste("merged", list_years, sep = "")
# Load the variable list we cleaned manually in Excel as CSV
soep_selection = read.table("variable-selection/soep-var-selection.csv", header = TRUE, sep = ";", check.names = FALSE)
# Get all Labels, unfiltered
labels = soep_selection[, 1]
# Create a vector to put object names of all years in it
datalist = c()

# Loop through all the years, import the data, merge, clean and label them
for (years in list_years) {
  # Define Current List of import data based on the 'i' value
  list_files = list.files(path = list_dirs[i], pattern = "", full.names = TRUE)
  # Import all the data from the current list with the read.dta-Function (part of foreign package) for SPSS-Files
  list_import = lapply(list_files, read.dta)
  # Merge it into one file
  data_merged = Reduce(function(x, y) merge(x, y, by = "persnr", all.x = TRUE), list_import)
  # Cut the .x and .y values from the merge process, so that we have clean column names
  colnames(data_merged) = gsub("\\.x|\\.y", "", colnames(data_merged))
  # Get the variable list of the current year
  current_list = sort(soep_selection[, k])
  # ONLY take the data shortlisted for the current year
  cleaned = data_merged[, which(names(data_merged) %in% current_list == TRUE)]
  # Select the Label Column and the Variable Column of the current Year
  soep_subcrit = c(1, k)
  # Subset the Variable list so that only the label and the current year exist
  soep_selection_sub = soep_selection[soep_subcrit]
  # Delete NA-Values from the list
  soep_selection_sub = na.omit(soep_selection_sub)
  # Create a subset of the clean labels, where all codenames match, to make sure that the labels are correct
  clean_labels = subset(soep_selection_sub, sort(soep_selection_sub[, 2]) == sort(names(cleaned)))
  # Order Dataframe alphabetically
  clean_sorted = cleaned[, order(names(cleaned))]
  # Order Frame with the Labels based on the ID
  ordered_colnames = clean_labels[order(clean_labels[2]), ]
  # Label the columns properly
  colnames(clean_sorted) = ordered_colnames[, 1]
  # Assign data_merged to current merge[year]
  assign(list_varnames[i], clean_sorted)
  # Add Year Variable to a list so that we can access all years by a loop
  datalist = c(datalist, list_varnames[i])
  # Update our variables for the next round
  i = i + 1
  k = k + 1
}

# Merge all data into one dataframe and add a column with the respective year, called "Wave"
# Create a new dataframe
merged_all <- data.frame(matrix(ncol = nrow(soep_selection), nrow = 0))
# Name the dataframe using the first column of the csv
colnames(merged_all) <- soep_selection[,1]
# Add "Wave" column to the dataframe
merged_all$Wave = numeric(nrow(merged_all))
# Iterator to step through the years
z = 1
# For loop adding data of every year to the data frame
for (years in c(datalist)) {
  # Get current year for the Wave column
  current_year = list_years[z]
  # Get dataset of the current year
  current_data = get(datalist[z])
  # Repeat the current year to fill the column "Wave" of the respective year
  Wave = rep(current_year,nrow(current_data))
  # Add year-value to the "Wave" column
  current_data = cbind(Wave, current_data)
  # Add the data to the merge dataframe
  merged_all = rbindlist(list(merged_all, current_data), fill = TRUE)
  # Iterator one up
  z = z + 1
} # END OF FOR-LOOP

# Removes Spaces in Variable Names and substitues with a . - Necessary for the dplyr package, which is handy for later analysis of our data
valid_column_names = make.names(names=names(merged_all), unique=TRUE, allow_ = TRUE)
names(merged_all) = valid_column_names

# Delete the intermediate variables to clean up the workspace
rm(list=datalist)
rm(list = c('clean_labels','clean_sorted', 'cleaned', 'current_data', 'data_merged', 'list_import', 'ordered_colnames', 'soep_selection', 'soep_selection_sub', 'current_list', 'current_year', 'i', 'k', 'labels', 'list_dirs', 'list_files', 'soep_subcrit', 'Wave', 'years', 'z'))

# REMOVE ALL OBJECTS
# rm(list=ls())
