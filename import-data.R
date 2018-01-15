### IMPORT DATA SKRIPT ###
# Install Package "Foreign", preinstalled by default, so just skip
# install.packages("foreign")

# Make Sure you check your Working Directory so that the import code works properly!

# Load Package "Foreign"
install.packages("reshape")
library(reshape)
install.packages("plyr")
library(reshape)
# Load all Data from "Input-Data"
bap <- read.dta("input-data/2010_bap.dta") #2010 START
bapage17 <- read.dta("input-data/2010_bapage17.dta")
bapbrutto <- read.dta("input-data/2010_bapbrutto.dta")
bapkal <- read.dta("input-data/2010_bapkal.dta")
bapluecke <- read.dta("input-data/2010_bapluecke.dta") #2010 END
bbp <- read.dta("input-data/2011_bbp.dta") #2011 START
bbpage17 <- read.dta("input-data/2011_bbpage17.dta")
bbpbrutto <- read.dta("input-data/2011_bbpbrutto.dta")
bbpkal <- read.dta("input-data/2011_bbpkal.dta")  #2011 END
bcp <- read.dta("input-data/2012_bcp.dta") #2012 START
bcpage17 <- read.dta("input-data/2012_bcpage17.dta")
bcpbrutto <- read.dta("input-data/2012_bcpbrutto.dta")
bcpka <- read.dta("input-data/2012_bcpkal.dta")
bcpluecke <- read.dta("input-data/2012_bcpluecke.dta") #2012 END
bdp <- read.dta("input-data/2013_bdp.dta") #2013 START
bdpage17 <- read.dta("input-data/2013_bdpage17.dta")
bdpbrutto <- read.dta("input-data/2013_bdpbrutto.dta")
bdpkal <- read.dta("input-data/2013_bdpkal.dta")
bdpluecke <- read.dta("input-data/2013_bdpluecke.dta") #2013 END
bep <- read.dta("input-data/2014_bep.dta") #2014 START
bepage17 <- read.dta("input-data/2014_bepage17.dta")
bepbrutto <- read.dta("input-data/2014_bepbrutto.dta")
bepkal <- read.dta("input-data/2014_bepkal.dta")
bepluecke <- read.dta("input-data/2014_bepluecke.dta") #2014 END
bfp <- read.dta("input-data/2015_bfp.dta") #2015 START
bfpage17 <- read.dta("input-data/2015_bfpage17.dta")
bfpbrutto <- read.dta("input-data/2015_bfpbrutto.dta") 
bfpkal <- read.dta("input-data/2015_bfpkal.dta") #2015 END

### Merge

filenames <- list.files(path="input-data/2015", full.names=TRUE)
import.list <- lapply(filenames, read.dta)

All2015 <- Reduce(function(x, y) merge(x, y, all=FALSE,by.x="persnr",by.y="persnr",all.x =TRUE, all.y =TRUE),import.list,accumulate=F)