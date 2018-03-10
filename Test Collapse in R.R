library(doBy)
hsb2 <- read.table("https://stats.idre.ucla.edu/wp-content/uploads/2016/02/hsb2-1.csv", header=T, sep=",")
attach(hsb2)


a <- summaryBy(socst ~ prog, data=hsb2)
