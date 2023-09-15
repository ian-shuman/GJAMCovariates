
#Code for calculating cardinal direction from the aspect value given by gSSURGO


##########Illinois X


setwd("D:/Data for Chris/GJAM Formatting/Illinois Output/Random Effects/")

files <- list.files("X/")

## Main loop for computing SWI
for(t in files) {
  setwd("D:/Data for Chris/GJAM Formatting/Illinois Output/Random Effects/X/")
  origname = paste0(t)
  object <- read.csv(paste0("D:/Data for Chris/GJAM Formatting/Illinois Output/Random Effects/X/", origname))
  xdata_new <- subset(object, select = -c(mean.KSA, GS_ppet))
  xdata_new <- cbind(xdata_new, direction=NA)
  
  for(i in 1:nrow(xdata_new)){
    if(is.na(xdata_new$mean.AspectProjected[i]) == T){
      xdata_new$direction[i] <- "NS"
    }}
  
  for(i in 1:nrow(xdata_new)){
    if(xdata_new$mean.AspectProjected[i] >= 0 && xdata_new$mean.AspectProjected[i] < 22.5 && is.na(xdata_new$mean.AspectProjected[i]) == F){
      xdata_new$direction[i] <- "N"
    }}
  
  for(i in 1:nrow(xdata_new)){
    if(xdata_new$mean.AspectProjected[i] >= 337.5 && xdata_new$mean.AspectProjected[i] <= 360 && is.na(xdata_new$mean.AspectProjected[i]) == F){
      xdata_new$direction[i] <- "N"
    }}
  
  for(i in 1:nrow(xdata_new)){
    if(xdata_new$mean.AspectProjected[i] >= 22.5 && xdata_new$mean.AspectProjected[i] < 67.5 && is.na(xdata_new$mean.AspectProjected[i]) == F){
      xdata_new$direction[i] <- "NE"
    }}
  
  for(i in 1:nrow(xdata_new)){
    if(xdata_new$mean.AspectProjected[i] >= 67.5 && xdata_new$mean.AspectProjected[i] < 112.5 && is.na(xdata_new$mean.AspectProjected[i]) == F){
      xdata_new$direction[i] <- "E"
    }}
  
  for(i in 1:nrow(xdata_new)){
    if(xdata_new$mean.AspectProjected[i] >= 112.5 && xdata_new$mean.AspectProjected[i] < 157.5 && is.na(xdata_new$mean.AspectProjected[i]) == F){
      xdata_new$direction[i] <- "SE"
    }}
  
  for(i in 1:nrow(xdata_new)){
    if(xdata_new$mean.AspectProjected[i] >= 157.5 && xdata_new$mean.AspectProjected[i] < 202.5 && is.na(xdata_new$mean.AspectProjected[i]) == F){
      xdata_new$direction[i] <- "S"
    }}
  
  for(i in 1:nrow(xdata_new)){
    if(xdata_new$mean.AspectProjected[i] >= 202.5 && xdata_new$mean.AspectProjected[i] < 247.5 && is.na(xdata_new$mean.AspectProjected[i]) == F){
      xdata_new$direction[i] <- "SW"
    }}
  
  for(i in 1:nrow(xdata_new)){
    if(xdata_new$mean.AspectProjected[i] >= 247.5 && xdata_new$mean.AspectProjected[i] < 292.5 && is.na(xdata_new$mean.AspectProjected[i]) == F){
      xdata_new$direction[i] <- "W"
    }}
  
  for(i in 1:nrow(xdata_new)){
    if(xdata_new$mean.AspectProjected[i] >= 292.5 && xdata_new$mean.AspectProjected[i] < 337.5 && is.na(xdata_new$mean.AspectProjected[i]) == F){
      xdata_new$direction[i] <- "NW"
    }}
  name = substring(t, 1, (nchar(t)-4))
  finished_filename = paste0("D:/Data for Chris/GJAM Formatting/Illinois Output/Random Effects/Fixed/", name,'_Fixed.csv') # specify file path
  write.csv(xdata_new, finished_filename, row.names=FALSE, quote=FALSE)
}


##########Indiana X


setwd("D:/Data for Chris/GJAM Formatting/Indiana Output/Random Effects/")

files <- list.files("X/")


for(t in files) {
  setwd("D:/Data for Chris/GJAM Formatting/Indiana Output/Random Effects/X/")
  origname = paste0(t)
  object <- read.csv(paste0("D:/Data for Chris/GJAM Formatting/Indiana Output/Random Effects/X/", origname))
  xdata_new <- subset(object, select = -c(mean.KSA, GS_ppet))
  xdata_new <- cbind(xdata_new, direction=NA)
  
  for(i in 1:nrow(xdata_new)){
    if(is.na(xdata_new$mean.AspectProjected[i]) == T){
      xdata_new$direction[i] <- "NS"
    }}
  
  for(i in 1:nrow(xdata_new)){
    if(xdata_new$mean.AspectProjected[i] >= 0 && xdata_new$mean.AspectProjected[i] < 22.5 && is.na(xdata_new$mean.AspectProjected[i]) == F){
      xdata_new$direction[i] <- "N"
    }}
  
  for(i in 1:nrow(xdata_new)){
    if(xdata_new$mean.AspectProjected[i] >= 337.5 && xdata_new$mean.AspectProjected[i] <= 360 && is.na(xdata_new$mean.AspectProjected[i]) == F){
      xdata_new$direction[i] <- "N"
    }}
  
  for(i in 1:nrow(xdata_new)){
    if(xdata_new$mean.AspectProjected[i] >= 22.5 && xdata_new$mean.AspectProjected[i] < 67.5 && is.na(xdata_new$mean.AspectProjected[i]) == F){
      xdata_new$direction[i] <- "NE"
    }}
  
  for(i in 1:nrow(xdata_new)){
    if(xdata_new$mean.AspectProjected[i] >= 67.5 && xdata_new$mean.AspectProjected[i] < 112.5 && is.na(xdata_new$mean.AspectProjected[i]) == F){
      xdata_new$direction[i] <- "E"
    }}
  
  for(i in 1:nrow(xdata_new)){
    if(xdata_new$mean.AspectProjected[i] >= 112.5 && xdata_new$mean.AspectProjected[i] < 157.5 && is.na(xdata_new$mean.AspectProjected[i]) == F){
      xdata_new$direction[i] <- "SE"
    }}
  
  for(i in 1:nrow(xdata_new)){
    if(xdata_new$mean.AspectProjected[i] >= 157.5 && xdata_new$mean.AspectProjected[i] < 202.5 && is.na(xdata_new$mean.AspectProjected[i]) == F){
      xdata_new$direction[i] <- "S"
    }}
  
  for(i in 1:nrow(xdata_new)){
    if(xdata_new$mean.AspectProjected[i] >= 202.5 && xdata_new$mean.AspectProjected[i] < 247.5 && is.na(xdata_new$mean.AspectProjected[i]) == F){
      xdata_new$direction[i] <- "SW"
    }}
  
  for(i in 1:nrow(xdata_new)){
    if(xdata_new$mean.AspectProjected[i] >= 247.5 && xdata_new$mean.AspectProjected[i] < 292.5 && is.na(xdata_new$mean.AspectProjected[i]) == F){
      xdata_new$direction[i] <- "W"
    }}
  
  for(i in 1:nrow(xdata_new)){
    if(xdata_new$mean.AspectProjected[i] >= 292.5 && xdata_new$mean.AspectProjected[i] < 337.5 && is.na(xdata_new$mean.AspectProjected[i]) == F){
      xdata_new$direction[i] <- "NW"
    }}
  name = substring(t, 1, (nchar(t)-4))
  finished_filename = paste0("D:/Data for Chris/GJAM Formatting/Indiana Output/Random Effects/Fixed/", name,'_Fixed.csv') # specify file path
  write.csv(xdata_new, finished_filename, row.names=FALSE, quote=FALSE)
}


####Illinois Y
#Code to change "other" to "Misc" in Y data columns- not used in final analyses


#setwd("D:/Data for Chris/GJAM Formatting/Illinois Output/Random Effects/")
#files <- list.files("Y/")
#for(t in files) {
  setwd("D:/Data for Chris/GJAM Formatting/Illinois Output/Random Effects/Y/")
  origname = paste0(t)
  object <- read.csv(paste0("D:/Data for Chris/GJAM Formatting/Illinois Output/Random Effects/Y/", origname))
  ydata <- object
  ydata$Misc.conifer <- ydata$Other.conifer
  ydata$Misc.hardwood <- ydata$Other.hardwood
  ydata_new <- subset(ydata, select = -c(Other.hardwood, Other.conifer))
  name = substring(t, 1, (nchar(t)-4))
  finished_filename = paste0("D:/Data for Chris/GJAM Formatting/Illinois Output/Random Effects/Fixed/", name,'_Fixed.csv') # specify file path
  write.csv(ydata_new, finished_filename, row.names=FALSE, quote=FALSE)
}
