### EXECUTE ENTIRE R CODE ###
# Install FormatR Package
install.packages("formatR")

# Tidy up the source code to make it in Quantlet-Style
#  tidy_source(file = "output.R")

# All Quantlet Files begin with a Q, so we list all files starting with a "Q"
  execute_files <- list.files(pattern = "^Q", all.files=TRUE, recursive=FALSE)
  
  for(file in execute_files) {
    if(success == TRUE) {
      print("Loaded")
      print(file)
      current_load = file
      #current_load = get(current_load)
      source(current_load)
      #close()
    } else {
      "The former file could not be executed successfully so we make a break here!"
    }
  }
  
  
  
  # Delete everything - Cleanup-Command - BE CAREFUL WITH IT!
  # rm(list=ls(all=TRUE))
  
  
