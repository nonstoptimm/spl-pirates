### QUANTLET PREPARATION ###
# Install FormatR Package
# install.packages("formatR")
library(formatR)

# Tidy up the source code to make it in Quantlet-Style
tidy_source("Q1_Import-Data.R", file = "SOEPQ1_ImportPrepareData/ImportPrepareData.R")
tidy_source("Q2_Characteristics-Employees.R", file = "SOEPQ2_DescriptiveAnalysis/DescriptiveAnalysis.R")
tidy_source("Q3_Simulation-of-Minimum-Wage.R", file = "SOEPQ3_SimulationMinimumWageEffect/SimulationMinimumWageEffect.R")
tidy_source("Q4_Employment-Analysis-and-Graphs.R", file = "SOEPQ4_EmploymentAnalysis/EmploymentAnalysis.R")
tidy_source("Q5_Kaitz-Index.R", file = "SOEPQ5_IdentificationAffectedRegions/IdentificationAffectedRegions.R")
tidy_source("Q6_Treatment-Graphs.R", file = "SOEPQ6_GraphicalAnalysisMinWage/GraphicalAnalysisMinWage.R")
tidy_source("Q7_DiD_Estimation.R", file = "SOEPQ7_DiffDiffEstimation/DiffDiffEstimation.R")
tidy_source("Q8_Statistical_Tests.R", file = "SOEPQ8_StatisticalTests/StatisticalTests.R")

# needed only for package installation or update
library(devtools)
devtools::install_github("lborke/yamldebugger")

# load the package every time you want to use 'yamldebugger'
# library(yamldebugger)
# 
# allKeywords
# "plot" %in% allKeywords

# workdir = "/Users/Timm/Documents/Humboldt/5.Semester/SPL/Repository/spl-pirates/SOEPQ1"
# 
# d_init = yaml.debugger.init(workdir, show_keywords = TRUE)
# 
# qnames = yaml.debugger.get.qnames(d_init$RootPath)
# 
# d_results = yaml.debugger.run(qnames, d_init)
# 
# OverView = yaml.debugger.summary(qnames, d_results, summaryType = "mini")

# Delete everything - Cleanup-Command - BE CAREFUL WITH IT!
# rm(list=ls(all=TRUE))
  
  
