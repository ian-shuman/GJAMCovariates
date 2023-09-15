## Extracting Environmental Variables for Section Corners 
## Author: I Shuman and L Darling
## Date modified: 28 September 2022

rm(list=ls(all=TRUE)) 

#Load libraries -----------

library(tidyverse)
library(tidylog)
library(magrittr)
library(sf)
library(raster)
library(spatialEco)
library(exactextractr)
library(terra)

setwd("D:/Management Areas")

files <- list.files("Indiana Points/")

setwd("D:/Shuman Covariates 2022/Topographic Covariates")
slope <- raster('SlopeProjected.tif') 

setwd("D:/Shuman Covariates 2022/Topographic Covariates")
aspect <- raster('AspectProjected.tif')

setwd("D:/Shuman Covariates 2022/Soil Covariates/Lindsay's Compiled Soils Rasters/Kelly's Python Rasters/Python Rerun Output")
#I suggest projecting the rasters in ArcMap, as it is much faster
CACraster <- raster('IanMosaicCACreal.tif') 

CECraster <- raster('IanMosaicCECreal.tif') 

CLAraster <- raster('IanMosaicCLAreal.tif') 

KSAraster <- raster('IanMosaicKSATreal.tif') 

SANraster <- raster('IanMosaicSANreal.tif') 

SILraster <- raster('IanMosaicSILreal.tif') 

WATraster <- raster('IanMosaicWATreal.tif') 

## Main loop for computing SWI
for(i in files) {
setwd("D:/Management Areas")
#Section corners ----------
filename = paste0(i)
corner <- st_read(filename) %>%  #TODO: change for all section corners
  dplyr::select(x, y) %>% #Too many fields, grabbing a couple for simplicity
  st_transform(., crs = 3175) %>% #change to Albers Great Lake
  st_buffer(., 0.0001) #buffer to point level (buffer size is an extremely small polygon to approximate point data) 

#TODO: I am doing this by creating a small buffer around each point. We can change that buffer size
#if it is preferable. However, the zonal.stats function that I'm using to extract values requires a
#polyogon, not a point. If we want to extract point data we need to find a new function or make
#an extremely small polygon by changing the buffer size to 0.0001 or something.


#Slope --------------------

cornerSlope <- raster::extract(slope, corner, method = simple, buffer = 0.0001, fun = mean) %>% 
  cbind(corner, .)

#Aspect -------------------

cornerSlopeAspect <- raster::extract(aspect, corner, method = simple, buffer = 0.0001, fun = mean) %>%
  cbind(cornerSlope, .)

colnames(cornerSlopeAspect) <- c('x', "y", "Slope", "Aspect", "geometry")

#Soils --------------------

#This script reads in the merged .tif files produced after going through Kelly's Python process.

setwd("D:/Shuman Covariates 2022/Soil Covariates/Lindsay's Compiled Soils Rasters/Kelly's Python Rasters")

#Load soil rasters and calculate stats
CAC <- raster::extract(CACraster, corner, method = simple, buffer = 0.0001, fun = mean)

CEC <- raster::extract(CECraster, corner, method = simple, buffer = 0.0001, fun = mean)

CLA <- raster::extract(CLAraster, corner, method = simple, buffer = 0.0001, fun = mean)

KSA <- raster::extract(KSAraster, corner, method = simple, buffer = 0.0001, fun = mean)

SAN <- raster::extract(SANraster, corner, method = simple, buffer = 0.0001, fun = mean)

SIL <- raster::extract(SILraster, corner, method = simple, buffer = 0.0001, fun = mean)

WAT <- raster::extract(WATraster, corner, method = simple, buffer = 0.0001, fun = mean)

#Join soil data back to main df

cornerSlopeAspectSoil <- cbind(cornerSlopeAspect, CAC, CEC, CLA, KSA, SAN, SIL, WAT)
setwd("D:/Shuman Covariates 2022")
cornerSlopeAspectSoil$geometry <- NULL
name = substring(filename, 1, (nchar(filename)-11)) # Substring DEM to match with shp file name
finished_filename = paste0("D:/Shuman Covariates 2022/10m Slope Aspect Soil/Ian Mosaic", name,'_Ian10mSlopeAspectSoil.csv') # specify shp file path
write.csv(cornerSlopeAspectSoil, finished_filename, row.names=FALSE, quote=FALSE)
setwd("D:/Management Areas")
}


#I didn't want to take the time to make this a loop, as I only needed to do this process once, but you could/should loop this over all files in the 10m and Old Soils folders of D:/Shuman Covariates 2022/10m Slope Aspect Soil/

#Read in old driver data used in previous GJAM runs and also new 10m fine resolution soils data
setwd("D:/Shuman Covariates 2022/10m Slope Aspect Soil/Old Soils/")
OLDsoils <- read.csv("IN_NE_X_DataStructure_RandomEffects_Fixed.csv")
setwd("D:/Shuman Covariates 2022/10m Slope Aspect Soil/10m/")
NEWsoils <- read.csv("Ian MosaicIN_NE_Ian10mSlopeAspectSoil.csv")

#Sort both the old X dataset and new 10m soils dataset by X, then Y so all columns are matched
OLDsoils2 <- OLDsoils[order(OLDsoils$x, OLDsoils$y, decreasing = TRUE),]
NEWsoils2 <- NEWsoils[order(NEWsoils$x, NEWsoils$y, decreasing = TRUE),]
NEWsoils2 <- subset(NEWsoils2, select = -c(KSA))

#Put 1km resolution old soils values into any new soils value with NA
for(i in 1:nrow(NEWsoils2)){
  if(is.na(NEWsoils2$CAC[i]) == T){
    NEWsoils2$CAC[i] <- OLDsoils2$mean.CAC[i]
  }
  if(is.na(NEWsoils2$CEC[i]) == T){
    NEWsoils2$CEC[i] <- OLDsoils2$mean.CEC[i]
  }
  if(is.na(NEWsoils2$CLA[i]) == T){
    NEWsoils2$CLA[i] <- OLDsoils2$mean.CLA[i]
  }
  if(is.na(NEWsoils2$SAN[i]) == T){
    NEWsoils2$SAN[i] <- OLDsoils2$mean.SAN[i]
  }
  if(is.na(NEWsoils2$SIL[i]) == T){
    NEWsoils2$SIL[i] <- OLDsoils2$mean.SIL[i]
  }
  if(is.na(NEWsoils2$WAT[i]) == T){
    NEWsoils2$WAT[i] <- OLDsoils2$mean.WAT[i]
  }
}

#Re-use the other covariates from previous GJAM runs, but put in new 10m soils covariates
Combinedsoils <- cbind(OLDsoils2, NEWsoils2)
Combinedsoils <- subset(Combinedsoils, select = -c(mean.SlopeProjected, mean.AspectProjected, mean.CAC, mean.CEC, mean.CLA, mean.SAN, mean.SIL, mean.WAT, direction))
Combinedsoils <- subset(Combinedsoils, select = -c(x.1, y.1))
Combinedsoils <- Combinedsoils[,c(1:5, 11:18, 6:10)]
setwd("D:/Shuman Covariates 2022/10m Slope Aspect Soil/No NA/New Soils")
write.csv(Combinedsoils, "IN_NE_X_Ian10mDataStructure_RandomEffects_NotFixed.csv", row.names=FALSE, quote=FALSE)
setwd("D:/Shuman Covariates 2022/10m Slope Aspect Soil/Fixed Aspect/No Aspect/")
Combinedsoils_NoAspect <- subset(Combinedsoils, select = -c(Aspect))
write.csv(Combinedsoils_NoAspect, "IN_NE_X_Ian10mDataStructure_RandomEffects_NoAspect.csv", row.names=FALSE, quote=FALSE)


#Code for adding "direction" column of simplified Aspect with NS = No Slope (instead of NA)

setwd("D:/Shuman Covariates 2022/10m Slope Aspect Soil/")

files <- list.files("No NA/New Soils")

## Main loop for computing simplified Aspect
for(t in files) {
  setwd("D:/Shuman Covariates 2022/10m Slope Aspect Soil/No NA/New Soils/")
  origname = paste0(t)
  object <- read.csv(paste0("D:/Shuman Covariates 2022/10m Slope Aspect Soil/No NA/New Soils/", origname))
  xdata_new <- object
  xdata_new <- cbind(xdata_new, direction=NA)
  
  for(i in 1:nrow(xdata_new)){
    if(is.na(xdata_new$Aspect[i]) == T){
      xdata_new$direction[i] <- "NS"
    }}
  
  for(i in 1:nrow(xdata_new)){
    if(xdata_new$Aspect[i] >= 0 && xdata_new$Aspect[i] < 45 && is.na(xdata_new$Aspect[i]) == F){
      xdata_new$direction[i] <- "N"
    }}
  
  for(i in 1:nrow(xdata_new)){
    if(xdata_new$Aspect[i] >= 315 && xdata_new$Aspect[i] <= 360 && is.na(xdata_new$Aspect[i]) == F){
      xdata_new$direction[i] <- "N"
    }}
  
  for(i in 1:nrow(xdata_new)){
    if(xdata_new$Aspect[i] >= 45 && xdata_new$Aspect[i] < 135 && is.na(xdata_new$Aspect[i]) == F){
      xdata_new$direction[i] <- "E"
    }}
  
  for(i in 1:nrow(xdata_new)){
    if(xdata_new$Aspect[i] >= 135 && xdata_new$Aspect[i] < 225 && is.na(xdata_new$Aspect[i]) == F){
      xdata_new$direction[i] <- "S"
    }}
  
  for(i in 1:nrow(xdata_new)){
    if(xdata_new$Aspect[i] >= 225 && xdata_new$Aspect[i] < 315 && is.na(xdata_new$Aspect[i]) == F){
      xdata_new$direction[i] <- "W"
    }}

  name = substring(t, 1, (nchar(t)-13))
  finished_filename = paste0("D:/Shuman Covariates 2022/10m Slope Aspect Soil/Fixed Aspect/New Soils/", name,'_Fixed.csv') # specify file path
  write.csv(xdata_new, finished_filename, row.names=FALSE, quote=FALSE)
}








