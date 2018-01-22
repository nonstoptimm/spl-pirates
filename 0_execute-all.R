### EXECUTE ENTIRE R CODE ###
# Install FormatR Package
install.packages("formatR")

# Tidy up the source code
#  tidy_source(file = "output.R")

# All Quantlet Files begin with a Q, so we list all files starting with a "Q"
  execute_files <- list.files(pattern = "^Q", all.files=TRUE, recursive=FALSE)
  
  for(file in execute_files) {
    print("Loaded")
    print(file)
    current_load = file
    #current_load = get(current_load)
    source("Q1_import-data.R")
    close()
  }
  
