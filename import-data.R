### IMPORT DATA SKRIPT ###
# Install Package "Foreign", preinstalled by default, so just skip
# install.packages("foreign")

# Make Sure you check your Working Directory so that the import code works properly!

# Load Package "Foreign"
library(foreign)
jp <- read.dta("input-data/jp.dta")