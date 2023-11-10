## Calculating Distance from Corners to Trees

## Author: I Shuman
## Date: May 24 2023

## Prepare workspace
rm(list = ls())


library("FactoMineR")
library("factoextra")
library(tidyverse)
library(corrplot)

setwd("D:/Data for Chris")

########## Data Prep based on 1Process.R in gjam repository #####################

# Read in file names that we need to loop over
xfiles <- list.files('GJAM DATA/X/')
yfiles <- list.files('GJAM DATA/Y/')


# Storage
xdata_list <- list()
yedata_list <- list()

# Read in files
for(i in 1:length(xfiles)){
  filename <- xfiles[i]
  pathname <- paste0('GJAM DATA/X/',filename)
  xdata_list[[i]] <- read.csv(pathname)
  
  filename <- yfiles[i]
  pathname <- paste0('GJAM DATA/Y/',filename)
  yedata_list[[i]] <- read.csv(pathname)
}

# Initialize unlist
xdata <- xdata_list[[1]]
xdata$filename <- rep(xfiles[1], times = nrow(xdata))

# Unlist
for(i in 2:length(xfiles)){
  dat <- xdata_list[[i]]
  dat$filename <- rep(xfiles[i], times = nrow(dat))
  xdata <- rbind(xdata, dat)
}

ydata_list <- list()
edata_list <- list()

# Separate the ydata and the edata
for(i in 1:length(yfiles)){
  tempdata <- yedata_list[[i]]
  
  ydata_list[[i]] <- tempdata %>%
    select((colnames(tempdata)[!grepl('dist', colnames(tempdata), fixed = T)]))
  
  edata_list[[i]] <- tempdata %>%
    select(colnames(tempdata)[grepl('dist', colnames(tempdata), fixed = T)])
}

# Storage
columns <- c()

# Find names of all columns
for(i in 1:length(yfiles)){
  tempdata <- ydata_list[[i]]
  columns <- c(columns, colnames(tempdata))
}
columns <- unique(columns)

# Make function for "not in"
`%nin%` <- Negate(`%in%`)

# Add columns that don't exist in a given matrix
for(i in 1:length(yfiles)){
  tempdata <- ydata_list[[i]]
  cols <- colnames(tempdata)
  save <- which(columns %nin% cols)
  newcols <- (ncol(tempdata)+1):(ncol(tempdata)+length(save))
  tempdata[,newcols] <- 0
  tempdata <- as.data.frame(tempdata)
  colnames(tempdata)[newcols] <- columns[save]
  ydata_list[[i]] <- tempdata
}

# Do the same for the edata
columns <- c()
for(i in 1:length(yfiles)){
  tempdata <- edata_list[[i]]
  columns <- c(columns, colnames(tempdata))
}
columns <- unique(columns)

for(i in 1:length(yfiles)){
  tempdata <- edata_list[[i]]
  cols <- colnames(tempdata)
  save <- which(columns %nin% cols)
  newcols <- (ncol(tempdata)+1):(ncol(tempdata)+length(save))
  tempdata[,newcols] <- 0
  tempdata <- as.data.frame(tempdata)
  colnames(tempdata)[newcols] <- columns[save]
  edata_list[[i]] <- tempdata
}

# Initialize unlist
ydata <- ydata_list[[1]]
ydata$filename <- rep(yfiles[1], times = nrow(ydata))

# Unlist
for(i in 2:length(yfiles)){
  dat <- ydata_list[[i]]
  dat$filename <- rep(yfiles[i], times = nrow(dat))
  ydata <- rbind(ydata, dat)
}

# Same for edata
edata <- edata_list[[1]]
edata$filename <- rep(yfiles[1], times = nrow(edata))

for(i in 2:length(yfiles)){
  dat <- edata_list[[i]]
  dat$filename <- rep(yfiles[i], times = nrow(dat))
  edata <- rbind(edata, dat)
}

# Remove big unnecessary objects
rm(edata_list, xdata_list, ydata_list, yedata_list)

# Format management area columns
xdata <- xdata %>%
  mutate(marea = sub('_X.*', '', filename)) %>%
  select(-filename) %>%
  mutate(uniqueID = paste0(marea,'_',uniqueID))

ydata <- ydata %>%
  mutate(marea = sub('_Y.*', '', filename)) %>%
  select(-filename) %>%
  mutate(uniqueID = paste0(marea,'_',uniqueID))

edata <- edata %>%
  mutate(marea = sub('_Y.*', '', filename)) %>%
  select(-filename) %>%
  mutate(uniqueID = ydata$uniqueID)

#############Find the max and upper 90% distance from corner point to tree location

chainstree <- as.vector(c(ydata$chainstree, ydata$chainstree2, ydata$chainstree3, ydata$chainstree4))
chainstree <- na.omit(chainstree)
chainstree <- as.numeric(grep('^-?[0-9.]+$', chainstree, val = T))
max(chainstree)
#4780
quantile(chainstree, probs = c(.5, .9, .99))
#20, 88, 438
#This tells us that 90% of trees are within 88 chains of the corner

#Calculate how many m or km 88 chains is
(88*804)/40
#1768.8 m or 1.7688 km

CHAINStree  <- chainstree / 100
  
kilometerstree <-  (CHAINStree * 804)/(40*1000)

hist(kilometerstree, breaks = 100)

sort <- sort(kilometerstree, decreasing = F)
tail(sort)

quantile(kilometerstree, probs = c(.1, .2, .3, .4, .5, .6, .7, .8, .9))

###Without Prairie or Savanna

ydata_new <- subset(ydata, !No.tree == 1)
ydata_new <- subset(ydata_new, !Oak == 1)
ydata_new <- subset(ydata_new, !Hickory == 1)
view(ydata_new)

chainstree_new <- as.vector(c(ydata_new$chainstree, ydata_new$chainstree2, ydata_new$chainstree3, ydata_new$chainstree4))
chainstree_new <- na.omit(chainstree_new)
chainstree_new <- as.numeric(grep('^-?[0-9.]+$', chainstree_new, val = T))

quantile(chainstree_new, probs = c(.5, .9, .99))
chainstree_new  <- (chainstree_new * 804)/(40*1000)

hist(chainstree_new, breaks = 100)
sort_new <- sort(chainstree_new, decreasing = T)
view(sort_new)


quantile(chainstree_new, probs = c(.1, .2, .3, .4, .5, .6, .7, .8, .9))




