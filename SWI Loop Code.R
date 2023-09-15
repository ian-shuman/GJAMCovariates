## Computing SWI from DEM
## Author: AM Willson and I Shuman
## Date modified: 16 September 2022

## Prepare working environment
rm(list = ls())
library(raster)
library(sf)
library(tidyverse)
library(RSAGA)
library(spatialEco)
setwd("D:/Shuman Covariates 2022/Topographic Covariates")

## List files to loop over
files <- list.files("IN SWI DEMs/tifs/")

## Main loop for computing SWI
for(i in files) {
  ## Reformat DEM input
  filename = paste0('IN SWI DEMs/tifs/',i) # Write file name pointing to the subdirectory
  DEM <- raster(filename) # Load in DEM for area of interest
  print(paste('Success loading DEM', i))
  projectRaster(DEM, crs='+init=epsg:3175') # Reproject in Great Lakes St. Lawrence Albers
  print(paste('Reprojected raster for DEM', i))
  writeRaster(DEM, 'DEM.sgrd', overwrite = T) # Saves DEM in proper format
  
  ## Calculate SWI
  print(paste('Calculating SWI using DEM', i))
  env <- rsaga.env(path = 'C:/Users/paleolab/Desktop/saga-8.3.0_x64/saga-8.3.0_x64/') # specify RSAGA path
  rsaga.wetness.index(in.dem = 'DEM.sgrd', out.wetness.index = 'SWI.tif', env = env) # Calculate SWI
  print(paste('Success calculating SWI', i))
  
  ## Reload SWI as GTIFF
  SWI <- raster('SWI.tif')
  print(paste('Success loading SWI.tif', i))
  
  ## Load and Extract SWI at Point Level
  print(paste('Extracting SWI at point level for area', i))
  #setwd("D:/Management Areas/IN SWI Points/")
  name = substring(filename, 18, (nchar(filename)-7)) # Substring DEM to match with shp file name
  shp_filename = paste0("D:/Management Areas/IN SWI Points/IN_", name,'_Points.shp') # specify shp file path
  corner <- st_read(shp_filename) %>%  #TODO: change for all section corners
    dplyr::select(x, y) %>% #Too many fields, grabbing a couple for simplicity
    st_transform(., crs = 3175) %>% #change to Albers Great Lake
    st_buffer(., 0.0001) #buffer to 0.0001m (point level) for extraction 
  cornerSWI <- spatialEco::zonal.stats(corner, SWI, stats = 'mean') %>% 
    cbind(corner,.)
  cornerSWI$geometry <- NULL
  
  ##Save Extracted SWI
  print(paste('Saving SWI for area', i))
  #setwd("D:/Shuman Covariates 2022/Topographic Covariates/SWIs/")
  csv_filename = paste0("IN SWIs/IN_", name,'_SWI.csv') # specify corresponding SWI csv
  write.csv(cornerSWI, csv_filename, row.names=FALSE, quote=FALSE) # write csv
  print(paste('Finished with file', i))
}
