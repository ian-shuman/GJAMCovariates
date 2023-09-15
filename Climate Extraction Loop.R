rm(list=ls(all=TRUE)) 


library(plyr)
library(raster)
library(data.table)
library(rgdal)
library(reshape2)
library(ggplot2)

setwd("D:/Shuman Covariates 2022/Climate Covariates")

## List files to loop over
files <- list.files("Indiana Points/")
for(i in files) {

pointname = paste0(i)
basename = substring(pointname, 1, (nchar(pointname)-11))
csvname= paste0(basename, "_Points.csv")

##Code for calculating monthly temperature means and extracting the to the point coordinates for the historical PRISM data from 1895-1925##
setwd("D:/Shuman Covariates 2022/Climate Covariates")
IL_StudyArea_Points <- read.csv(csvname)
coordinates(IL_StudyArea_Points) <- ~x + y
proj4string(IL_StudyArea_Points) <- '+init=epsg:3175'
TransPoints<- spTransform(IL_StudyArea_Points, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 '))

TransPoints<- as.data.frame(TransPoints)

years <- 1895:1925
yrs <- "1895-1925"

setwd("D:/Shuman Covariates 2022/Climate Covariates/Mean Temperature 1895 - 1925")

filenames <- list.files(pattern=paste(".*_",".*\\.bil$", sep = ""))

# use substring to index filenames that match the years designated:

filenames <- filenames [substring(filenames, first = 26, last = 29) %in% years]
filenames
s <- stack(filenames) 
t <- crop(s, extent(c(-97.24357, -82.40131 , 37.1442 , 49.38583))) #crop to the extent of indiana & illinois 
s <- projectRaster(t, crs= '+init=epsg:3175') # project in great lakes albers
y <- data.frame(rasterToPoints(s)) #covert to dataframe
years <- rep(years, each = 12)
mo <- rep(c('Jan', 'Feb', 'Mar', "Apr", "May", 
            'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec"), 10)  


test <- y
monthly <- test
yearly <- test

melted.mo <- melt(monthly, id.var = c('x', 'y'))
melted.mo$yrs <- substring(melted.mo$variable, first = 26, last = 29)
melted.mo$mos <- substring(melted.mo$variable, first = 30, last = 31)
full<- dcast(melted.mo, x + y ~ mos, mean , value.var='value', na.rm = TRUE)
full$Mean <- rowMeans(full[,3:14])
colnames(full) <- c('x','y','Jan', 'Feb', 'Mar', "Apr", "May", 
                    'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec",'Mean')
test <- full

#calculate the CV temperature seasonality:
#TSI = sd(m1....m12)/Tavgannual *100
test$cv <- (apply(test[,3:14],1, sd, na.rm = TRUE)/test[,15])*100
full <- test
TransPoints <- data.frame(TransPoints)

# for monthly dataset
# convert to rasterstack
coordinates(full) <- ~x + y
gridded(full) <- TRUE
avgs <- stack(full) 

plot(avgs) #plots averages


setwd("D:/Shuman Covariates 2022/Climate Covariates/Mean Temperature 1895 - 1925")
IL_StudyArea_Points<- as.data.frame(IL_StudyArea_Points)
avgs.df <- data.frame(raster::extract(avgs, IL_StudyArea_Points[,c("x","y")])) #Extracting based on IL_Prairie1_Points_Practice so that output has xy coordinates in meters
avgs.df$x <- IL_StudyArea_Points$x
avgs.df$y <- IL_StudyArea_Points$y
avgs.df$long <- TransPoints$x #need to add lat and long for use in calculating PET
avgs.df$lat <- TransPoints$y
avgs.df
workingdir <- "/Users/paleolab/ShumanCovariates 2022/Climate Covariates/"
setwd("D:/Shuman Covariates 2022/Climate Covariates/Indiana Outputs/")
csvfilename1 = paste0(basename,'_t1895-1925.csv')
write.csv(avgs.df, csvfilename1, row.names=FALSE, quote=FALSE) 



#saveRDS(avgs.df, csvfilename1)


####################################################################################################################################################################################
##Code for calculating monthly precipitation means and extracting the to the point coordinates for the historical PRISM data from 1895-1925#########################################
####################################################################################################################################################################################

setwd("D:/Shuman Covariates 2022/Climate Covariates")
IL_StudyArea_Points <- read.csv(pointname)
coordinates(IL_StudyArea_Points) <- ~x + y
proj4string(IL_StudyArea_Points) <- '+init=epsg:3175'
TransPoints<- spTransform(IL_StudyArea_Points, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 '))



TransPoints<- as.data.frame(TransPoints)


#designate the years we want to extract/ average over
years <- 1895:1925
yrs <- "1895-1925"

setwd("D:/Shuman Covariates 2022/Climate Covariates/Mean Precipitation 1895 - 1925")

# this chunk of code reads in the filenames within the PRISM data folder
filenames <- list.files(pattern=paste(".*_",".*\\.bil$", sep = ""))

# use substring to index filenames that match the years designated:

filenames <- filenames [substring(filenames, first = 24, last = 27) %in% years]
filenames

s <- stack(filenames) #make all into a raster
t <- crop(s, extent(c(-97.24357, -82.40131 , 37.1442 , 49.38583))) 
s <- projectRaster(t, crs='+init=epsg:3175') # project in great lakes albers
y <- data.frame(rasterToPoints(s)) #covert to dataframe
years <- rep(years, each = 12)
mo <- rep(c('Jan', 'Feb', 'Mar', "Apr", "May", 
            'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec"), 10)  

monthly <- y
yearly <- y

# melt so that we can calculate the mean total precipitation by month  
melted.mo <- melt(monthly, id.var = c('x', 'y'))
melted.mo$yrs <- substring(melted.mo$variable, first = 24, last = 27)
melted.mo$mos <- substring(melted.mo$variable, first = 28, last = 29)

#calculate means for months
full<- dcast(melted.mo, x + y ~ mos, mean , value.var='value', na.rm = TRUE)
full<- data.frame(full)
full$total <- rowSums(full[,3:14])
colnames(full) <- c('x','y','Jan', 'Feb', 'Mar', "Apr", "May", 
                    'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec",'total')


# calculate a seasonality index from the monthly precipitatoin
full$SI <- rowSums(abs(full[,3:14]-(full[,15]/12)))/full[,15]
#melted.yr <- melt(yearly, id.var = c('x', 'y'))
write.csv(full, "temporary_meltedppt_1895_1925.csv", row.names=FALSE, quote=FALSE) 
full

coordinates(full) <- ~x + y
gridded(full) <- TRUE
avgs <- stack(full)





plot(avgs) #plots the raster averages
#writeRaster(avgs, "raster_avgs")

IL_StudyArea_Points<- as.data.frame(IL_StudyArea_Points)
avgs.df <- data.frame(raster::extract(avgs, IL_StudyArea_Points[,c("x","y")]))
avgs.df$x <- IL_StudyArea_Points$x
avgs.df$y <- IL_StudyArea_Points$y
avgs.df

setwd("D:/Shuman Covariates 2022/Climate Covariates/Indiana Outputs/")
csvfilename1a <- paste0('avgs_', basename,'_ppt1895-1925.csv')
write.csv(avgs.df, csvfilename1a)
setwd("D:/Shuman Covariates 2022/Climate Covariates/Mean Precipitation 1895 - 1925")

# kh temporary:
melt.temp <- melted.mo[!melted.mo$mos %in% "02",] # rn we are missing feb in the PET:

saveRDS(melt.temp, "melt_StudyArea_ppt1895-1925.RDS")

melt.temp$year <- as.numeric(substring(melt.temp$variable, first = 24, last = 27))
melt.temp.byxy <- dcast(melt.temp, x + y ~  year + mos, mean, value.var = 'value', na.rm = TRUE)
coordinates(melt.temp.byxy) <- ~x + y
gridded(melt.temp.byxy) <- TRUE
avgs <- stack(melt.temp.byxy) 

IL_StudyArea_Points<- as.data.frame(IL_StudyArea_Points)

plot(avgs) #plots the raster averages

avgs.df <- data.frame(raster::extract(avgs, IL_StudyArea_Points[,c("x","y")]))
avgs.df$x <- IL_StudyArea_Points$x
avgs.df$y <- IL_StudyArea_Points$y

setwd("D:/Shuman Covariates 2022/Climate Covariates/Indiana Outputs/")
csvfilename2 = paste0(basename,'_ppt1895-1925.csv')

write.csv(avgs.df, csvfilename2, row.names=FALSE, quote=FALSE)

######################################################################################
#PET CODE USING THORNTHWAITE()#
######################################################################################


setwd("D:/Shuman Covariates 2022/Climate Covariates/Indiana Outputs/")
temp_data = read.csv(csvfilename1)
setwd("D:/Shuman Covariates 2022/Climate Covariates/Mean Temperature 1895 - 1925")

library(SPEI)
library(tidyverse)


data2 = temp_data[,1:12]
data2$site = seq(1, nrow(data2))

data_new = data2 %>%
  pivot_longer(cols = Jan:Dec) #pivot data so that it is compatible with thornthwaite() function

colnames(data_new) = c('Site', 'Month', 'Temp')

th = matrix(NA, nrow = max(unique(data_new$Site)), ncol = 12) #make empty matrix to fill with data from the loop

for(i in 1:max(unique(data_new$Site))){
  th[i,] = thornthwaite(data_new$Temp[data_new$Site == i], temp_data$lat[i])
}

th <- as.data.frame(th)
colnames(th) = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
th$Mean <-apply(th,1,mean)
th$x <- temp_data$x
th$y <- temp_data$y
th$long <- temp_data$long 
th$lat <- temp_data$lat

setwd("D:/Shuman Covariates 2022/Climate Covariates/Indiana Outputs/")

csvfilename3 = paste0(basename,'_PET.csv')

write.csv(th, csvfilename3, row.names=FALSE, quote=FALSE)

##########################################################################
#Calculating P-PET
##########################################################################

setwd("D:/Shuman Covariates 2022/Climate Covariates/Indiana Outputs/")
PET <- read.csv(csvfilename3)
PETjja_unsorted <- PET[,c("Mar","Apr", "May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "x", "y")]
colnames(PETjja_unsorted) <- c("Mar_pet","Apr_pet", "May_pet","Jun_pet", "Jul_pet", "Aug_pet", "Sep_pet", "Oct_pet", "Nov_pet", "x", "y" )
PETjja <- arrange(PETjja_unsorted, x, y)



Precip <- read.csv(csvfilename1a)
setwd("D:/Shuman Covariates 2022/Climate Covariates/Mean Precipitation 1895 - 1925")
PrJJA_unsorted <- Precip[,c("Mar","Apr", "May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "x", "y")]
colnames(PrJJA_unsorted) <- c("Mar_pr","Apr_pr", "May_pr","Jun_pr", "Jul_pr", "Aug_pr", "Sep_pr", "Oct_pr", "Nov_pr", "x", "y")
PrJJA <- arrange(PrJJA_unsorted, x, y)

P.PET <- cbind(PrJJA, PETjja)

# now lets calculaate P - PET for Mar - Nov
P.PET$Mar_ppet <- P.PET$Mar_pr - P.PET$Mar_pet
P.PET$Apr_ppet <- P.PET$Apr_pr - P.PET$Apr_pet
P.PET$May_ppet <- P.PET$May_pr - P.PET$May_pet
P.PET$Jun_ppet <- P.PET$Jun_pr - P.PET$Jun_pet
P.PET$Jul_ppet <- P.PET$Jul_pr - P.PET$Jul_pet
P.PET$Aug_ppet <- P.PET$Aug_pr - P.PET$Aug_pet
P.PET$Sep_ppet <- P.PET$Sep_pr - P.PET$Sep_pet
P.PET$Oct_ppet <- P.PET$Oct_pr - P.PET$Oct_pet
P.PET$Nov_ppet <- P.PET$Nov_pr - P.PET$Nov_pet

#Calculate total P-PET for the entire growing season
P.PET$GS_ppet <- rowSums(P.PET[,3:11], na.rm=TRUE) - rowSums(P.PET[,12:20], na.rm=TRUE)

#ggplot(P.PET, aes(x,y, fill = Jun_ppet))+geom_raster()
#ggplot(P.PET, aes(x,y, fill = Jul_ppet))+geom_raster()
#ggplot(P.PET, aes(x,y, fill = Aug_ppet))+geom_raster()
#ggplot(P.PET, aes(x,y, fill = GS_ppet))+geom_raster()


setwd("D:/Shuman Covariates 2022/Climate Covariates/Indiana Outputs/")

csvfilename4 = paste0(basename,'_P.PET.csv')

write.csv(P.PET, csvfilename4)

}
######################################################################################
######################################################################################
#OLD CODE FROM KELLY TO CALCULATE PET WITHOUT thornthwaite()
######################################################################################
######################################################################################






#####################################PET CODE ###########################################################################
#Ian pasted this from 03c_Extract_PET_crc.R and removed the original code, may need to put the original code from 03_Extract_Prism_historical.R back in there too##


library(plyr)
library(StudyAreater)
library(data.table)
library(rgdal)
library(reshape2)

version <- "1.7-5" # pls version

# set the working dir (where the prism data folder is)
setwd("D:/Shuman Covariates 2022/Climate Covariates")


# again read in the 8km grid for extracting
IL_Prairie1_Points_Practice <- read.csv("IL_Prairie1_Points_Practice.csv")
coordinates(IL_Prairie1_Points_Practice) <- ~x + y
TransPoints.11 <- proj4string(IL_Prairie1_Points_Practice) <- '+init=epsg:3175'
TransPoints.11 <- spTransform(IL_Prairie1_Points_Practice, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 '))



TransPoints.11<- as.data.frame(TransPoints.11)
#spec.table.ll <- read.csv(paste0(workingdir, "spec.lat.long.csv"))


######################################################################################
# calculate PET from the temperature data:
######################################################################################
# may need to do this in CRC:

library(SPEI)
#setwd to data directory
setwd("D:/Shuman Covariates 2022/Climate Covariates/Mean Temperature 1895 - 1925")

#read in the grid again
#TransPoints <- read.csv("IL_Prairie1_Points_Practice.csv")
#coordinates(spec.table) <- ~x + y

# project the grid to lat long
#proj4string(spec.table) <- '+init=epsg:3175'
#spec.lat <- spTransform(spec.table, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0' ))
TransPoints <- TransPoints.11


years <- 1895:1925
yrs <- "1895-1925"

# read in the filenames, stack as StudyAreaters, extract raster to points
filenames <- list.files(pattern=paste(".*_",".*\\.bil$", sep = ""))

# use substring to index filenames that match the years designated:

filenames <- filenames [substring(filenames, first = 24, last = 27) %in% years]

s <- stack(filenames) #make all into a raster
t <- crop(s, extent(c(-97.24357, -82.40131 , 37.1442 , 49.38583))) #crop to the extent of the region 
s <- projectRaster(t, crs='+init=epsg:3175') # project in great lakes albers
y <- data.frame(rasterToPoints(t)) #covert to dataframe


test <- y
# this does not work in pulling out the data from one grid cell
y$CellID <- seq(1:nrow(y))

my.list <- list()


# this for loop calculates thornthwaite PET for each month in each grid cell
#for(i in 1:length(y$y)){
source("D:/Shuman Covariates 2022/Climate Covariates/Thornthwaite_PET.R")
system.time(for(i in 1:length(y$y)){
  
  ynew <- t(y[i,3:135])
  lat <- y[i,]$y
  long <- y[i,]$x
  cellID <- y[i,]$CellID
  # get month an year from row.names
  year <-  data.frame(year = substring(row.names(ynew), first = 24, last = 27))
  month <- data.frame(month = substring(row.names(ynew), first = 28, last = 29))
  ynew2 <- data.frame(Tave = ynew[,1], 
                      year <- year, 
                      month <- month)
  ynew2 <- ynew2[ynew2$month %in% c("01","03","04","05","06", "07", "08", "09", "10", "11", "12"),]
  # use the thorthwaite equation to attach the PET data to the 
  
  ynew2$PET_tho <- as.numeric(thornthwaite_PET(ynew2$Tave, lat))
  ynew2$lat <- lat
  ynew2$long <- long
  ynew2$CellID <- cellID
  
  my.list[[i]] <- ynew2
  
})
# if PET.df already exists, then add to the df, if not, then create "PET.df"
#   if(exists  ("PET.df")){
PET.df <- do.call(rbind, my.list)


# rename the object:
full.PET <- PET.df
#remove PET.df
#rm(PET.df)


PET.means <- dcast(full.PET, lat + long  ~ month , mean , value.var='PET_tho', na.rm = TRUE)
colnames(PET.means) <- c("lat", "long",
                         "apr","may","jun", "jul", "aug", "sep", "oct")
#ggplot(PET.means, aes(lat, long, fill = jul))+geom_raster()
# get the precipitation data in the same format:
saveRDS(full.PET, "full.PET_full_reg.rds")
saveRDS(PET.means, "PET.means_full_reg.rds")

#full.PET <- readRDS("data/full.PET.rds")
full.PET <- full.PET[,c( "month","PET_tho", "lat","long")]
full <- dcast(full.PET, lat + long ~ month, mean, value.var = 'PET_tho', na.rm = TRUE)

full$Mean <- rowMeans(full[,4:length(full)], na.rm=TRUE)



full2 <- full

#ggplot(full2, aes(long, lat, fill = Aug))+geom_raster()
# for monthly dataset
# convert to rasterstack
coordinates(full) <- ~long + lat
gridded(full) <- TRUE
avgs <- stack(full) 

plot(avgs) #plots averages
#proj4string(avgs) <- '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0' 
#avgs.alb <- projectRaster(avgs, crs='+init=epsg:3175')

# spec.table is a spatial points df, convert to regular df:
#spec.table <- as.data.frame(spec.table)
IL_Prairie1_Points_Practice<- as.data.frame(IL_Prairie1_Points_Practice)
avgs.df <- data.frame(raster::extract(avgs, IL_Prairie1_Points_Practice[,c("x","y")]))
avgs.df$x <- IL_Prairie1_Points_Practice$x
avgs.df$y <- IL_Prairie1_Points_Practice$y

write.csv(avgs.df, "PET_pls_extracted.csv", row.names=FALSE, quote=FALSE)

saveRDS(avgs.df, "PET_pls_extracted.RDS")



######################################################################
# Extracting P- E from the Extract_PET_crc.R output
#


#full.PET <- readRDS('data/full.PET.rds')
#full.PET <- readRDS("data/PET_full/full.PET.rds")
full.PET <- full.PET[,c( "month","PET_tho", "lat","long")]
full <- dcast(full.PET, lat + long ~ month, mean, value.var = 'PET_tho', na.rm = TRUE)
colnames(full) <- c("lat", "long", "Jan", 'Mar',"Apr", "May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


coordinates(full) <- ~long + lat
gridded(full) <- TRUE
avgs <- stack(full) 

plot(avgs) #plots averages

proj4string(avgs) <- '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0' 
avgs.alb <- projectRaster(avgs, crs='+init=epsg:3175')

# spec.table is a spatial points df, convert to regular df:
#spec.table <- read.csv(paste0(workingdir,'midwest_pls_full_density_alb',version,'.csv'))
TransPoints <- read.csv("IL_Prairie1_Points_Practice.csv")
coordinates(TransPoints) <- ~x + y

# project the grid to lat long
proj4string(TransPoints) <- '+init=epsg:3175'
TransPoints <- as.data.frame(TransPoints)

#Use IL_Prairie1_Points_Practice for x and y in meters
setwd("D:/Shuman Covariates 2022/Climate Covariates")
IL_Prairie1_Points_Practice <- read.csv("IL_Prairie1_Points_Practice.csv")
coordinates(IL_Prairie1_Points_Practice) <- ~x + y
proj4string(IL_Prairie1_Points_Practice) <- '+init=epsg:3175'

plot(avgs.alb)

IL_Prairie1_Points_Practice <- as.data.frame(IL_Prairie1_Points_Practice)
avgs.df <- data.frame(raster::extract(avgs.alb, IL_Prairie1_Points_Practice[,c("x","y")]))
avgs.df$x <- IL_Prairie1_Points_Practice$x
avgs.df$y <- IL_Prairie1_Points_Practice$y

write.csv(avgs.df, "PETJJA_1895_1925_pls_extracted_mar_nov.csv")
#ggplot(avgs.df, aes(x,y, fill = Aug))+geom_raster()



#PETjja <- read.csv("data/PETJJA_1895_1925_pls_extracted.csv")
setwd("D:/Shuman Covariates 2022/Climate Covariates/Mean Precipitation 1895 - 1925")
Precip <- read.csv("avgs_Prairie1_ppt1895-1925.csv")
PETjja <- avgs.df
PrJJA <- Precip[,c("x", "y","Mar","Apr", "May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov")]
colnames(PrJJA) <- c("x", "y", "Mar_pr","Apr_pr", "May_pr","Jun_pr", "Jul_pr", "Aug_pr", "Sep_pr", "Oct_pr", "Nov_pr")

colnames(PETjja) <- c("Jan_pet", "Mar_pet","Apr_pet", "May_pet","Jun_pet", "Jul_pet", "Aug_pet", "Sep_pet", "Oct_pet", "Nov_pet","Dec_pet","x","y")
PETjja <- PETjja[,!names(PETjja) %in% c("Jan_pet", "Dec_pet")] # remove january

#ggplot(PrJJA, aes(x,y, fill = Jun_pr))+geom_raster()
#ggplot(PETjja, aes(x,y, fill = Jun_pet))+geom_raster()

P.PET <- merge(PrJJA, PETjja, by = c("x", "y"))



# now lets calculaate P - PET for Jun - Aug
P.PET$Mar_ppet <- P.PET$Mar_pr - P.PET$Mar_pet
P.PET$Apr_ppet <- P.PET$Apr_pr - P.PET$Apr_pet
P.PET$May_ppet <- P.PET$May_pr - P.PET$May_pet
P.PET$Jun_ppet <- P.PET$Jun_pr - P.PET$Jun_pet
P.PET$Jul_ppet <- P.PET$Jul_pr - P.PET$Jul_pet
P.PET$Aug_ppet <- P.PET$Aug_pr - P.PET$Aug_pet
P.PET$Sep_ppet <- P.PET$Sep_pr - P.PET$Sep_pet
P.PET$Oct_ppet <- P.PET$Oct_pr - P.PET$Oct_pet
P.PET$Nov_ppet <- P.PET$Nov_pr - P.PET$Nov_pet

P.PET$GS_ppet <- rowSums(P.PET[,3:11], na.rm=TRUE) - rowSums(P.PET[,12:20], na.rm=TRUE)

ggplot(P.PET, aes(x,y, fill = Jun_ppet))+geom_raster()
ggplot(P.PET, aes(x,y, fill = Jul_ppet))+geom_raster()
ggplot(P.PET, aes(x,y, fill = Aug_ppet))+geom_raster()
ggplot(P.PET, aes(x,y, fill = GS_ppet))+geom_raster()

setwd("D:/Shuman Covariates 2022/Climate Covariates")
write.csv(P.PET, "P.PET_prism_1895_1925_Mar_Nov.csv")



