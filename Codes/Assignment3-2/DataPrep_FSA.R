
install.packages("RCurl")

library(sf)
library(tidyverse)

library(RCurl)




# TAKES TIME TO DOWNLOAD! so you might as well save and comment this to skip this  once saved. 
# SOurce of FSA - statistics canada, 2011 File. NOte that there are 2021 data too, so this example use old data. 
#"https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2011-eng.cfm"
URL <- "https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gfsa000b11a_e.zip"
download.file(url=URL,destfile='CanadaFSA.zip')


# Create a filder for FSA shapefile 
dir.create("dataDir", showWarnings = FALSE)
unzip('CanadaFSA.zip', exdir = "dataDir")
fsaCan <- read_sf("dataDir/gfsa000a11a_e.shp")


# Takes time to display! 
# Display Canada 
plot(st_transform(fsaCan %>% select(CFSAUID)), main = "FSAs in Canada, default CRS")
plot(st_transform(fsaCan, crs = 3978) %>% select(CFSAUID), main = "Use of SRID 3978")

# Display Quebec, with and without projection 
# I added filter(PRUID == 24) to select QC (Province ID = 24) 
plot(st_transform(fsaCan %>%  filter(PRUID == 24) %>% select(CFSAUID), crs = 3978), main = "QC, projected nationally")
plot(st_transform(fsaCan %>%  filter(PRUID == 24), crs = 32198) %>% select(CFSAUID), main = "QC, projected to QC")

# Display Census Metro Area of Montreal  
fsaQc <- fsaCan[grepl("^H", fsaCan$CFSAUID), ]
plot(fsaQc)

