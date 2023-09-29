## Creating a Final CSV For Each Management Area's Corners which aligns with the covariates extracted

## Author: I Shuman
## Date: December 19 2022

## Prepare workspace
rm(list = ls())
library(raster)
library(sf)
library(tidyverse)
library(RSAGA)
library(spatialEco)
setwd("D:/Data for Chris/")

IN_PLS <- read.csv("ndilpls_v2.1.csv")
NE_final_corners <- read.csv("Empty Headers.csv")

setwd("D:/Data for Chris/Illinois Covariates/")

covariate_file <- read.csv("IL_Prairie3_Covariate_Data.csv")

#Extract the PLS corners which match up with the desired covariate xy locations
#Aligning the old covariate extraction with the new PLS DATA
for(i in IN_PLS$UniqueID){
  for(n in covariate_file$UniqueID){
  if(IN_PLS$x[i] == covariate_file$x[n] && IN_PLS$y[i] == covariate_file$y[n]){
    NE_final_corners <- rbind(NE_final_corners, IN_PLS[i,])
  } 
  }
}

if(nrow(covariate_file == nrow(NE_final_corners))){
  setwd("D:/Data for Chris/Illinois Y")
  write.csv(NE_final_corners, file = "IL_Prairie3_RawY.csv")
  
}

####CODE FOR IF nrow(corner_file != nrow(Small3_final_covariates) #####################
#If there are covariate xy locations which are no longer on the PLS Wiki Dataset


corner_file <- corner_file[order(corner_file$x, corner_file$y),]
Small3_final_covariates <- Small3_final_covariates[order(Small3_final_covariates$x, Small3_final_covariates$y),]


setwd("D:/Data for Chris/")
write.csv(corner_file, file = "IL_Small3_CovaraiatesTEST.csv")
write.csv(Small3_final_covariates, file = "IL_Small3_CornersTEST.csv")

corner_file <- read.csv("Illinois X/IL_Prairie33_CovariatesFinal.csv")

unique(corner_file$y == Small3_final_covariates$y)
