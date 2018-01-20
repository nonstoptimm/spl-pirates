rm(list=ls(all=TRUE))

test <- read.dta("C:/Users/AlbertThieme/Dropbox/Albert Privat/HU Berlin/Master/Semester 1/Statistical Programming Languages/github/spl-pirates/input-data/2015/2015_bfp.dta")

summary(test$bfp10401)

test$b <- test$bfp10401 

as.numeric(test$b)
summary(test$b)

test$a[test$bfp10401 != "[1] Ja"] <- 0
test$a[test$b == "[1] Ja"] <- 1

summary(test$a)

print(attributes(test$a))

