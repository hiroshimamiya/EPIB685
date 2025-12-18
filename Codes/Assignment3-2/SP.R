
### Setting up R environment ################################
# Refresh the environment 
rm(list= ls()) 

# Install and Load the necessary packages 
library("spdep")
library("sp")
library("sf")
library("classInt") # For mapping 
library("RColorBrewer") # For mapping
library("CARBayes") # Library to run conditional autoregressive model for areal data 







### Load area data 

#https://ised-isde.canada.ca/site/office-superintendent-bankruptcy/en/statistics-and-research/forward-sortation-area-fsa-and-north-american-industry-classification-naics-reports/forward-sortation-area-definition

# TAKES TIME TO DOWNLOAD! so you might as well save and comment this to skip this  once saved. 
# SOurce of FSA - statistics canada, 2011 File. NOte that there are 2021 data too, so this example use old data. 
#"https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2011-eng.cfm"
URL <- "https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gfsa000b11a_e.zip"
download.file(url=URL,destfile='CanadaFSA.zip')


# Create a filder for FSA shapefile 
dir.create("fsaFolder", showWarnings = FALSE)
unzip('CanadaFSA.zip', exdir = "fsaFolder")
fsaCan <- read_sf("fsaFolder/gfsa000b11a_e.shp")

                             
# pre-downloaded data - water cropped 
fsaCan_water <- read_sf("data/fsaCan_waterversion/gfsa000b11a_e.shp")

# Takes time to display! 
# Display Canada 
plot(st_transform(fsaCan %>% select(CFSAUID)), main = "FSAs in Canada, default CRS")
plot(st_transform(fsaCan, crs = 3978) %>% select(CFSAUID), main = "Use of SRID 3978")

# Display Quebec, with and without projection 
# I added filter(PRUID == 24) to select QC (Province ID = 24) 
plot(st_transform(fsaCan %>%  filter(PRUID == 24) %>% select(CFSAUID), crs = 3978), main = "QC, projected nationally")
plot(st_transform(fsaCan %>%  filter(PRUID == 24), crs = 32198) %>% select(CFSAUID), main = "QC, projected to QC")

# Extract postabl codes and display Census Metro Area of Montreal  
fsa2001 <- read_sf("data/FSA_Montreal_2001/shapefile_FSA_Montreal_2001.shp")
targetFSA_island <- unique(fsa2001$FSA)

# Extract the island of Montreal only
fsaMtl <- fsaCan[fsaCan$CFSAUID %in% targetFSA_island, ]
fsaMtl_water   <- fsaCan_water[fsaCan_water$CFSAUID %in% targetFSA_island, ]

# Plot minimal info, two ways for Montreal
plot(fsaMtl['PRUID'], main = "FSA in Mtl, ignoring water boundary")
ggplot() + geom_sf(data = fsaMtl , fill = "white", col = "red") + theme_minimal() + ggtitle("FSA in Mtl, ignoring water boundary")

plot(fsaMtl_water['PRUID'], main = "FSA in Mtl, respecting the water boundary")









### 1. Load and prepare data  ######################
# load GI count data by fsa, sex, and age. 
giData <- read.csv("data/giMontreal2006_quarter.csv", header = TRUE, stringsAsFactors = FALSE)
str(giData) #inspect 
head(giData) #inspect

# load 2011 census data containinig socio-economic and demographic data at FSA level 
fsaCensus2011 <- read.csv("./data/fsaCensusMontreal2011_imputed.csv", header = TRUE, stringsAsFactors = FALSE)
str(fsaCensus2011) #inspect
head(fsaCensus2011) #inspect


# Load shapefile (point of hospitals). The dsn argument indicates the path to
# the file, and the layer argument indicates the name of shapefile, usually
# without file extension.
hospPoint <- read_sf("data/Hospitals_Montreal/shapefile_Hospitals_Mtl.shp")
plot(hospPoint['ramq_id'], pch = 16, col = "black", main = "Location of hospitals in Mtl")



# FSA shape is prepared for you, but for your future study or fun activity can
# be downloaded from here too (2011 file below)
# https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2011-eng.cfm











### Example showing re-projection of spatial data ######################

# Are they in the same coordinate reference system (CRS)? If not, one needs to be
# re-projected to the same shape of the earth.


# If false, they cannot be overlaid (can't be plot on the same map), and
# distance/area-based analysis (e.g. distance between points) can be incorrect

plot(st_geometry(fsaMtl))
plot(st_geometry(hospPoint), pch = 16,  add = TRUE)


# Answer 
st_crs(fsaMtl)
st_crs(hospPoint)
hospPoint2 <- st_transform(hospPoint, 2959)
fsaMtl2 <-  st_transform(fsaMtl, st_crs(hospPoint2))
plot(st_geometry(fsaMtl2))
plot(st_geometry(hospPoint2), pch = 16,  add = TRUE)


ggplot() + 
  geom_sf(data = fsaMtl, color = "black", fill = "white") + 
  geom_sf(data = hospPoint, color = "red") + 
  theme_minimal()


identical(st_crs(fsaMtl2), st_crs(hospPoint2))
# coordinate reference system I used for Montreal is found here:
# http://spatialreference.org/ref/epsg/nad83csrs-utm-zone-18n/



## Question 1. Could the assessment of relative risk in section 3 of the 
##             module be affected by using an incorrect projection? Explain.


### How to access attributes in S4-class spatial data used by sp library?######################
# If interested in gaining advanced understainding in R and S4 class, see this:
# "Software for Data Analysis - Programming with R" by John Chambers

class(fsaMtl) 
summary(fsaMtl) #show summary of spatial and attribute data 
class(hospPoint)
summary(hospPoint)










### Attribute join i.e. Adding census attributes to FSA shape data ########################

# Analysis will be based on FSA, so first aggregate the observed 
# and expected GI count into FSA-level across age and sex groups in the GI outcome table
giDataFsa <- giData %>% 
  group_by(fsa) %>% 
  summarise(giCount = sum(giCount), denomCount = sum(denomCount), giExpectedCount = sum(giExpectedCount))

head(giDataFsa)
hist(giDataFsa$giCount)

# Additionally, we will calculte FSA-level rate of GI
# Answer 
giDataFsa$giFsaRate <- giDataFsa$giCount / giDataFsa$denomCount


# NOW perform attriute join 
# Merge FSA shapefle and GI count data.
fsaMtl <- fsaMtl %>% 
  left_join(giDataFsa, by = c("CFSAUID" = "fsa"))
# Alternatively, do this, but never refer data frame using @data if using "merge" function
# fsaMtl <- merge(fsaMtl, giDataFsa, by.x= "FSA", by.y = "fsa")

# Likewise, merge FSA shapefile with 2011 census data i.e. attach census attributes to the shapefle 
fsaMtl <- fsaMtl %>% 
  left_join(fsaCensus2011, by = c("CFSAUID" = "fsa"))


glimpse(fsaMtl)
plot(fsaMtl['giFsaRate'], main = "rate")
plot(fsaMtl['giExpectedCount'], main = "expected count, age-sex adjusted")
plot(fsaMtl['prop_ps'], main = "proportion of post-secondary graduates")
plot(fsaMtl['med_income'], main = "Median household income")





### 2. Plotting maps #############################################################

## 2.1 Plot Points
# Plot FSA shape first, then try to overlay with hospital points - Remember if
# using markdown, the two plot commands need to be executed together (not line
# by line)
plot(fsaMtl)#plot shape of Montreal partitioned by FSA
plot(hospPoint, add=TRUE, col= "blue") # I am trying to overlay point (hospital) data to fsa polygon 

# Subset hopsital data by island
hospPointIsland <- hospPoint[fsaMtl, ]
plot(fsaMtl, main="FSA and hospitals"); plot(hospPointIsland, col = "blue", add=TRUE)

# if you like, display FSA label 
text(coordinates(fsaMtl),as.character(fsaMtl@data$FSA),cex=0.6)



## 2.2 Plot Risk Factor as Choropleth
# Now use census attributes that you joined (merged) to the FSA spatial polygon
# data, and make a map of one of these attributes, median family income, by FSA.

# define number of color bins to group continuous measure of median family income 
nclr = 5

# define color palette -- see http://colorbrewer2.org 
plotclr = brewer.pal(nclr,"Greens")

# define the classes or breaks for the data
class = classIntervals(fsaMtl@data$med_income, nclr, style="quantile", dataPrecision=0)

# create a vector of colors for each region
colcode = findColours(class,plotclr)

# plot the region boundaries - again, run all the three lines as a chunk if using R markdown
plot(fsaMtl, col=colcode)
title(sub="Median Family Income", main='Classed Choropleth')
legend('topleft', legend=names(attr(colcode, "table")), fill=attr(colcode,"palette"), cex=0.9, bty='n')


## 2.3 Plot median family income as a proportional symbol or 'bubble map'
max.symbol.size=5
min.symbol.size=1
plotvar = fsaMtl@data$med_income
# create symbols for each FSA with size scaled to income in FSA
symbol.size = ((plotvar-min(plotvar))/(max(plotvar)-min(plotvar))*(max.symbol.size-min.symbol.size) + min.symbol.size)
# plot FSA boundaries
plot(fsaMtl)
# get coordinates for centroids of FSA
mtl.cntr = coordinates(fsaMtl)
# plot circles of graduate size and color at centroids
points(mtl.cntr, pch=16, col=colcode, cex=symbol.size)
# outline the circles
points(mtl.cntr, cex=symbol.size)
title(sub="Median Family Income", main="Bubble Plot")
legend('topleft', legend=names(attr(colcode, "table")), fill=attr(colcode,"palette"), cex=0.9, bty='n')



## 2.4 Plot disease outcomes as choropleth

# Create a map showing crude GI visits by FSA as rate (Number of GI visits over
# number of residents in each FSA) 

#color bin to categorize the intensity of GI visits
nclr = 5

# Plot map with four different levels of grouping of the outcome
groups = c(3,5,7,9)
par(mfrow=c(2,2))

for (group in groups) {
  plotclr = brewer.pal(nclr, 'Reds')
  class.crude = classIntervals((fsaMtl@data$giFsaRate*1000), group, style='quantile', dataPrecision=0)
  colcode.crude = findColours(class.crude, plotclr)
  
  plot(fsaMtl)
  plot(fsaMtl, col=colcode.crude, add=T)
  title(sub="Crude Rates of GI Visits by FSA 
        (Annual Visits per 1,000)")
  legend('topleft', legend=names(attr(colcode.crude, "table")), fill=attr(colcode.crude,"palette"), cex=0.9, bty='n')
} # for - levels



# @David, there are 5 areas with no gi case, implication of this to crude/smooth SMR  and 
# potential need to impute with a small number could be discussed, as anlayst would commonly face this uncommon 
# for most diseases that are rare
# Display areas without observed count
par(mfrow=c(1,1))
fsaMtl@data[fsaMtl$giCount == 0, c("denom_totalpop", "giCount", "giExpectedCount")]
plot(fsaMtl, border = "grey", col=grey(c(1,0))[as.factor(fsaMtl@data$giFsaRate == 0)], 
     main = "Areas with no observed gi count in Jan - March of 2006
     Note that there are three very small 
     business districts with a small # of inhabitants", cex.main = 0.8)
# For nuw, imputed with 1 
fsaMtl@data[fsaMtl$giCount == 0, "giCount"] <- 1


## Question 2. Comment on the spatial patterns you do or do not observe at each level of 
##             grouping. Is there any relationship between the number of groups and the concept 
##             of smoothing? Explain. 




### 3. Analysis of areal disease count - Disease risk mapping ###############################################

# Here we will calculate and map the relative risk of GI cases by FSA using SMR
# (Standaridized Morbidity ratio), none-spatial model, and spatial model.


### 3.1 Crude SMR (Please review standardized morbidity ratio from the first year)

# SMR of GI visits for area i is: SMR_i = Observed_count_i/Expected_count_i
# Where age-sex standardized expected count of GI in area i can be calculated
# by indirect standardization. Expected count of area i is already provided to you and stored in
# the column `fsaMtl@data$giExpectedCount`. This is a sum of expected count,
# where stratum-specific population (age and sex in this excercise) in each FSA
# is multiplied by the stratum-specific GI rate across the entire study region
# of Montreal.






# Note that the variance of SMR is; 
# VAR(SMR) = SMR/Expected_Count
# Thus, the smaller the expected count, larger the variance. 

# We will map SMR, a maximum likelihood estimator of area-level relative risk  

# Using the expected and oversed GI count, caulcate SMR and its variance 
fsaMtl@data$SMR <- fsaMtl@data$giCount / fsaMtl@data$giExpectedCount
fsaMtl@data$varSMR <- fsaMtl@data$SMR / fsaMtl@data$giExpectedCount


## Question 3 - Plot SMR, its variance, population counts, and disease counts 
##              in a 2x2 gridded plot as above. Describe the spatial trends in 
##              the crude SMR and variance.




par(mfrow=c(2,2))
#Map 3.1 - SMR
plotclr = brewer.pal(nclr, 'BuPu')
class.crude = classIntervals((fsaMtl@data$SMR), 4, style='fixed', fixedBreaks=c(0,0.5,1,2,3), dataPrecision = 2)
colcode.crude = findColours(class.crude, plotclr)

plot(fsaMtl)
plot(fsaMtl, col = colcode.crude, add=T)
title(sub="Standardized Morbidity Ratio") 
legend('topleft', legend=names(attr(colcode.crude, "table")), fill=attr(colcode.crude,"palette"), cex=0.6, bty='n')

#Map 3.2 - Variance 
plotclr = brewer.pal(nclr, 'BuPu')
class = classIntervals((fsaMtl@data$varSMR), 2, style='fixed', fixedBreaks = c(0, 0.001, 0.05, 0.092), dataprecision = 3)
colcode.crude = findColours(class.crude, plotclr)

plot(fsaMtl)
plot(fsaMtl, col=colcode.crude, add=T)
title(sub="SMR Variance") 
legend('topleft', legend=names(attr(colcode.crude, "table")), fill=attr(colcode.crude,"palette"), cex=0.6, bty='n')

#Map 3.3 - Population Counts!
plotclr = brewer.pal(nclr, 'BuPu')
class.crude = classIntervals((fsaMtl@data$denom_totalpop), 6, style='quantile', dataPrecision = 0)
colcode.crude= findColours(class.crude, plotclr)

plot(fsaMtl)
plot(fsaMtl, col=colcode.crude, add=T)
title(sub="Population Counts") 
legend('topleft', legend=names(attr(colcode.crude, "table")), fill=attr(colcode.crude,"palette"), cex=0.6, bty='n')

#Map 3.4 - Disease Counts!
plotclr = brewer.pal(nclr, 'BuPu')
class.crude <- classIntervals((fsaMtl@data$giCount), 5, style='quantile', dataPrecision=0)
colcode.crude <- findColours(class.crude, plotclr)

plot(fsaMtl)
plot(fsaMtl, col=colcode.crude, add=T)
title(sub="Disease Counts")
legend('topleft', legend=names(attr(colcode.crude, "table")), fill=attr(colcode.crude,"palette"), cex=0.6, bty='n') 

par(mfrow=c(1,1))






















### 3.2 Smoothed SMR using hierarchical Bayesian spatial model (Besag York Mollie
### conditional autoregressive model). 


# Create an R object (list) showing which FSA is adjacent to which
fsaNb = poly2nb(fsaMtl, queen=FALSE) # this function creates a neighorhood object called nb 


# Inspect the list for the neighbourhood structure.  
head(fsaNb) # list of areas showing neighbor ID
summary(fsaNb) # Note that one region has no link (neighbour) -- is this realistic? 
is.symmetric.nb(fsaNb) # Neighborhood needs to be symmetric (i.e. area i is neighbour of j, and vice versa)

# Plot and see how connection is defined 
plot(fsaMtl, border='darkgrey', las=1, main='Connectivity by Shared Borders', sub = "not quite correct definition of neighorhood")
plot(fsaNb, coordinates(fsaMtl), add=TRUE)


# The spatial structure above is not valid due to the presence of an isolated
# area, so need to create new one. Already done for you and ready to load. Read
# another neighbour list where the connection was manually created to represent
# bridges so that there is no isolated neighbour
fsaNbBridge <- read.gal('data/fsaNbBridge')
summary(fsaNbBridge) #now empty neighbour shoud be gone

plot(fsaMtl, border='lightgrey',  las=1, main='Neighborhood definition by Shared Borders')
plot(fsaNbBridge, coordinates(fsaMtl), col="red", add=TRUE, cex = 0.3) #new spatial relationship
plot(fsaNb, coordinates(fsaMtl), pch = 0.1, cex = 0.1, points=FALSE, add=TRUE, col="black") #old (and invalid) spatial relationship

# Finally generate a spatial weight matrix containing spatial structure 
# Matrix whose elements are coded 1 if two areas are adjacent(column and row are
# contiguous), otherwise zero
W <- nb2mat(fsaNbBridge, style = "B") 


# Center and scale covariates - helps convergence of MCMC 
fsaMtl$income_center <- scale(fsaMtl@data$med_income, center = TRUE, scale = TRUE)[,1]
fsaMtl$immig_center <- scale((fsaMtl@data$prop_immig), center = TRUE, scale = TRUE)[,1]
fsaMtl$young_center <- scale((fsaMtl@data$prop_age_under18), center = TRUE, scale = TRUE)[,1]
fsaMtl$old_center <- scale((fsaMtl@data$prop_age_over65), center = TRUE, scale = TRUE)[,1]
fsaMtl$education_center <- scale((fsaMtl@data$prop_ps), center = TRUE, scale = TRUE)[,1]
fsaMtl$popdensity_center <- scale((fsaMtl@data$popdensity_km), center = TRUE, scale = TRUE)[,1]






#http://people.bath.ac.uk/masgs/MedStatsIC_Files_2017/Prac3.pdf

# Before running the model, set a random seed so that you could get the identical results when repated, at least for the same computer. 
set.seed(123456) # pick any number you like 

# Run Besag-York-Mollie Conditional Autoregressive (BYM-CAR) model for areal data
# This could take a long time 
giFitCar <- S.CARbym(giCount~income_center + immig_center + young_center + education_center + popdensity_center + offset(log(giExpectedCount)), 
              family="poisson", 
              data=fsaMtl@data, 
              W = W, # Add the spatial structure here 
              # prior probability for precision of spatial and non-spatial random effect 
              prior.tau2 = c(1,0.01), # inverse gamma prior parametrized by shape and scale 
              prior.sigma2 = c(1,0.01), # inverse gamma prior parametrized by shape and scale

              burnin = 100000, # number of sample to be discarded as value before sationality (sign of convergence) of MCMC does not constitute posteriro distribution 
              
              n.sample =1000000, 
              thin = 40) # thinning wil often reduce autocorrelation of MCMC chain


# Because MCMC can take a long time to run, you may not have the time to repeat again when the software or computer crashes. So I would recommend saving the file  
# this saves the model fit object to your current directory. You can load this object later by read.RDS command or double click in R studio file window
saveRDS(giFitCar, file = "data/myBYM_CAR_modelFit.rds") # about 34 megabytes

# Now a quick diagnosis of MCMC and generate summary
summary(giFitCar)
colnames(giFitCar$samples$beta) <- colnames(giFitCar$X)

# plottting could take a few min depending on computer - do they look erratic or stationary?
plot(giFitCar$samples$beta) #Important to check convergence before seeing results  
plot(giFitCar$samples$psi[, sample(1:ncol(giFitCar$samples$psi), 6)]) #Check convergence of some spatial random effects

# If MCMC chains look good, display posterior summary (Inference on regression coefficients)
# NOte that the parameter "tau2" and "sigma2" indicates variance of random effect due to spatial and non-spatial effect, respectively. 
# However, 2 two variance components are not on the same scale and so how much of latent effect is attributable to spatial/non-spatial 
# component is not really obvious for this model.
print(giFitCar) 



## Question 4 - Calculate the relative risk (RR) for each area (remember 
##              that RR = disease count/Expected count), where model-estimated 
##              smoothed disease count is now in 
##             in fitted.values in the model output.  Map the RR, and compare
##              the estimated risk surface with that of the crude (non-model based) 
##              SMR. (Hint: after you caulcte the RR, you must join the results to
##              the fsaMtl spatial data frame for mapping as demonstrated above).














### end ####








# @David note, around 6 ourliers shurunk towards neighorhood mean due to smoothing, however, 3 areas are too small to see  #downtown and old montreal) 



### @David, codes below are optional. Compute and display uncertainty of the posterir RR using 
# 95% credible interval generated from posterior simulations in the model fit object. 

library("graphics")
library("grid") # For mapping
library("maptools")
library("sf")

# get RR and plot - here using spplot rather than generic plot function 
fsaMtl@data$RR.smooth <- giFitCar$fitted.values / fsaMtl@data$giExpectedCount
spplot(fsaMtl, c("RR.smooth", "SMR"))

### 95% CI and risk map 
# To compute credible interval (of fitted values) in Bayesian estimation, I first need posterior sample of fitted values from MCMC
postSampleFitted <- data.frame(giFitCar$samples[["fitted"]])
# Resulting data frame above has 102 colums recording posteior sample for each area, 
# and the command below get 95% tile of posteiror sample (credible interval) for each columns (areas)
ci <- sapply(postSampleFitted, function(x)quantile(x, probs = c(0.025, 0.975)))


# Resulting ci dataframe contains upper and lower 2.5percentile
fsaMtl@data$RR.smooth_lower <- ci["2.5%", ] / fsaMtl@data$giExpectedCount
fsaMtl@data$RR.smooth_upper <- ci["97.5%", ] / fsaMtl@data$giExpectedCount

# Create an indicator for high, low and indeterminate risk area 
fsaMtl@data$RR.smooth_classify <- 0
fsaMtl@data$RR.smooth_classify[fsaMtl@data$RR.smooth_lower > 1] <- 1
fsaMtl@data$RR.smooth_classify[fsaMtl@data$RR.smooth_upper < 1] <- -1

fsaMtl@data$RR.smooth_classify <- factor(fsaMtl@data$RR.smooth_classify, 
                                           levels = c(-1,1,0), 
                                           labels =c("Low-risk", "High-risk", "Indeterminate"))

# And plot 
spplot(fsaMtl, "RR.smooth_classify", col.regions = c( "#000000", "white", "lightgrey"), 
       col = "darkgrey", lwd = 1, colorkey = FALSE, 
       main = "Classification of areas by high, low and indeterminate risk of GI visits", 
       sub = "Areas are classifed by inclusion of null value (RR=1) into posterior 95% credible interval
       Black: High-risk area\n White: low-risk area \n Grey: Inconclusive area"
)

# As well, compare distribution of relative risk (y-axis) and population count in each area
par(mfrow = c(1,2))
plot(fsaMtl$totalpop, fsaMtl$RR.smooth, 
     ylim = c(min(fsaMtl$SMR), max(fsaMtl$SMR)),
     xlab = "Population at risk", ylab = "Smoothed RR", 
     main = "Smoothed relative risk by BYM-CAR model")
plot(fsaMtl$totalpop, fsaMtl$SMR, 
     ylim = c(min(fsaMtl$SMR), max(fsaMtl$SMR)), 
     xlab = "Population at risk", ylab = "Crude SMR", 
     main = "Crude relative risk")
dev.off()





