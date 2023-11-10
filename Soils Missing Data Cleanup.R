

setwd("D:/Shuman Covariates 2022/Soil Covariates/Lindsay's Compiled Soils Rasters/Kelly's Python Rasters")
CACraster <- raster('ILINCAC_projected.tif') 
crs(CACraster) <- CRS('+init=EPSG:3175')

CECraster <- raster('ILINCEC_projected.tif') 

CLAraster <- raster('ILINCLA_projected.tif') 

KSAraster <- raster('ILINKSA_projected.tif') 

SANraster <- raster('ILINSAN_projected.tif') 

SILraster <- raster('INILSIL_projected.tif') 

WATraster <- raster('INILWAT_projected.tif') 

soilsstack <- stack(CACraster, CECraster, CLAraster, KSAraster, SANraster, SILraster, WATraster)

setwd("D:/Shuman Covariates 2022/10m Slope Aspect Soil/")
object <- read.csv("IN_Dunes_10mSlopeAspectSoil.csv")
Narows <- object[is.na(object$KSA),]
coordinates(Narows) <- ~x+y
crs(Narows) <- CRS('+init=EPSG:3175')
check.coords <- rSDM::points2nearestcell(Narows, CACraster)


PointsMatrix = rasterToPoints(soilsstack)

save.image("RasterXY.RData")

CACpoints <- rasterToPoints(CACraster, spatial = T)
  proj4string(CACpoints)


  
  
