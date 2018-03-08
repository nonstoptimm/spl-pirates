### INSTALL ALL NECESSARY, ADDITIONAL PACKAGES ###
# Vector with names of all packages
list_packages = c("foreign", "stringr", "data.table")
# lapply on the package vector
lapply(list_packages, install.packages)
# Delete the package vector again
rm(list_packages)