### EXECUTE ENTIRE R CODE ###

# All Quantlet Files begin with a Q, so we list all files starting with a "Q"
  execute_files <- list.files(pattern = "^Q", all.files=TRUE, recursive=FALSE)
  
  for(file in execute_files) {
    print("Loaded")
    print(file)
    current_load = execute_files[file]
    current_load = get(current_load)
    source(current_load)
  }
  
  
  tidy_source(file = "output.R")