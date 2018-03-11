install.packages("sp")
library(sp)
map <- readRDS("/Users/Meret/Desktop/HU/5. Semester/SPL/git/spl-pirates/DEU_adm1.rds")

dev.off()
spplot(fknnew, "NAME_1", col.regions = "Kaitz", 
       col = "black")  

n <- length(map$NAME_1)
plot(map,col=rainbow(n),   main= "Kaitz Index")

n <- length(map$"NAME_1")
plot(map2, col=rainbow(n)



###########
print(map$NAME_1)
print(dbys2013$stateger)
newdbys2013$stateger <- c("Baden-Württemberg", "Bayern", "Berlin", "Brandenburg", "Bremen",  "Hamburg", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland", "Sachsen-Anhalt", "Sachsen", "Schleswig-Holstein", "Thüringen")  
#the merge
fknnew <- merge(map@data, newdbys2013 , by.x = "NAME_1", by.y = "stateger")
print(fknnew)


map2 <- merge(fknnew, map)
#checks for error, but I still didnt check it 
length(map@data$NAME_1)
length(newdbys2013$stateger)
is.character((map@data$NAME_1))

install.packages("tmaptools")
library(tmaptools)

is.character(newdbys2013$stateger)

######

map@data$KAITZ <- 
