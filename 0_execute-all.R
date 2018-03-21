### EXECUTE ENTIRE R CODE ###
# Install FormatR Package
# install.packages("formatR")
library(formatR)

# Tidy up the source code to make it in Quantlet-Style
tidy_source("Q1_Import-Data.R", file = "SOEPQ1/ImportData.R")
tidy_source("Q2_Characteristics-Employees.R", file = "SOEPQ2/CharacteristicsEmployees.R")
tidy_source("Q3_Simulation-of-Minimum-Wage.R", file = "SOEPQ3/SimulationMinimumWage.R")
tidy_source("Q4_Employment-Analysis-and-Graphs.R", file = "SOEPQ4/EmploymentAnalysisGraphs.R")
tidy_source("Q5_Kaitz-Index.R", file = "SOEPQ5/KaitzIndex.R")
tidy_source("Q6_Treatment-Graphs.R", file = "SOEPQ6/TreatmentGraphs.R")
tidy_source("Q7_DiD_Estimation.R", file = "SOEPQ7/DiDEstimation.R")
tidy_source("Q8_Statistical_Tests.R", file = "SOEPQ8/StatisticalTests.R")

# Delete everything - Cleanup-Command - BE CAREFUL WITH IT!
# rm(list=ls(all=TRUE))
  
  
