# GJAMCovariates
Extraction of PLS data and environmental covariates for GJAM analyses

PLS Vegetation Data was obtained from the PalEON Wiki at https://paleon.geography.wisc.edu/doku.php/start
Under "Data and Products" > "Raw Settlement Vegetation Data" > "Point/Corner Level Surveyor Data By State/Area" > "Illinois" and "Indiana"
Data will be made publicly available closer to publication of the manuscript

Workflow:

1. Climate Extraction Loop
   - This code extracts values for the climate covariates at each PLS corner
   - Input: x and y coordinates of the PLS corners to be used for analysis; PRISM temperature and precipitation data
   - Output: Mean temperature and Mean precipitation data (as well as deprecated variables) for each PLS corner
2. Topographic and Soils Loop
   - This code extracts values for the topographic and soils covariates at each PLS corner
   - Input: x and y coordinates of the PLS corners used for analysis; slope and aspect rasters from the U.S. Geological Survey’s 3DEP ⅓ arc-second Digital Elevation Model at 10-m resolution, and soil covariate rasters from the U.S. Department of Agriculture and National Cooperative Soil Survey’s gridded Soil Survey Geographic (gSSURGO) database
   - Output: Soils and topography values for each PLS corner
3. SWI Loop
   - This code calculates the SWI covariate using the slope and aspect rasters used in step 2
   - Input: x and y coordinates of the PLS corners used for analysis; DEM rasters for the PLS management areas, 
   - Output: SWI values for each PLS corner
4. Aspect Fixing
   - This code takes the numerical values for Aspect calculated in 2. Topographic and Soils Loop and converts it to the cardinal directions, as gjam does not do well with NA values
   - Input: Dataset of X covariates (from steps 2-4)
   - Output: Same files, with an additional column for Aspect direction
5. Alignment Code
   - This code creates a uniqueID for the PLS corner data and the extracted covariate data to keep the X and Y data "aligned," , this code was used when there were updates to the PLS corner data after extraction had already been completed
   - Input: PLS corner data and X covariate datq
   - Output: Same files with matching uniqueIDs
6. Validation Code
   - This code is simply the random number generator used to select which PLS management areas were witheld for out of sample prediction, and then the verification that all vegetation taxa are represented in the data used for model training
   - Input: Vegetation data for the PLS corners, sorted into individual files for each management area
   - Output: None
