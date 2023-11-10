## Calculating Number of Townships for Materials and Methods

## Author: I Shuman
## Date: December 16 2022

## Prepare workspace
rm(list = ls())


rm(list = ls())
library(raster)
library(sf)
library(tidyverse)
library(RSAGA)
library(spatialEco)
setwd("D:/Data for Chris/Indiana Data")

file <- read.csv("IN_Prairie1_Corner_Data.csv")

length(unique(file$TRP))

