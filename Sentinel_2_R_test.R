# Documentation
# https://sentinels.copernicus.eu/web/sentinel/missions/sentinel-1/observation-scenario
# https://en.wikipedia.org/wiki/BBCH-scale
# https://en.wikipedia.org/wiki/Tasseled_cap_transformation
# https://valentinitnelav.github.io/satellite-image-classification-r/
# https://www.datacareer.de/blog/random-forest-in-r-an-example/
#
################################################################################

library(raster)
library(cluster)
library(rgdal)
library(RStoolbox)
library(tidyverse)
library(caret)

################################################################################

# Set the folder location
setwd('/home/diego/GITHUP_REPO/Innovation_laboratory_EAGLE')

# Read Shapefile with the roi
roi <- readOGR(dsn=file.path("./ROI/Carmen_Rosa_Field.shp"))
plot(roi)

# Subset the shapefile by Number
roi_1 <- subset(roi, Number == "1")
plot(roi_1)

# Reproject to UTM zone 19S
roi_utm <- spTransform(roi, CRS("+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs"))
#roi_utm

################################################################################
# Sentinel 1 image display dated 2017-04-29

# Raster firsts images
test_S1 <- stack('Sentinel_1/S1A_IW_GRDH_1SDV_20170429T232729_20170429T232754_016365_01B172_C485.tif')

# Rescale images Sentine-1
#test2_S1 <- setValues(test_S1, scales::rescale(values(test), to = c(0, 255)))  

# Print our ranges for each band 
#cellStats(test2_S1, min)
#cellStats(test2_S1, max)

# Plotting RGB and Shapefile together
plot(test_S1[[1]],  stretch = "lin", axes = TRUE,
        main = 'Equipo de riego Carmen Rosa Parrón Español / 2017-04-29 / Sentinel-1')
plot(roi_utm, border = 'red', add = TRUE)

################################################################################
# Sentinel 2 image display dated 2016-09-01

# Raster firsts images
test_S2 <- stack('Sentinel_2/20160901T143752_20160901T144926_T19HCC.tif')

# Rescale images Sentine-2
#test2_S2 <- setValues(test_S2, scales::rescale(values(test), to = c(0, 255)))  

# Print our ranges for each band 
#cellStats(test2_S2, min)
#cellStats(test2_S2, max)

# Stack RGB images
test3_S2 <- stack(test_S2[[4]], test_S2[[3]], test_S2[[2]])

# Plotting RGB and Shapefile together
plotRGB(test3_S2,  stretch = "lin", axes = TRUE,
        main = 'Equipo de riego Carmen Rosa Parrón Español / 2016-09-01 / Sentinel-2')
plot(roi_utm, border = 'red', add = TRUE)

################################################################################

# Sentinel-1 images orbit Ascending
Sentinel_1_path <- file.path("./Sentinel_1")

# Load all the images in one list.
Sentinel_1_images <- list.files(Sentinel_1_path,
                                full.names = TRUE,
                                pattern = ".tif$")

# Analysis if there are some duplicated
duplicated(Sentinel_1_images)

# Quantify the duplicates
Sentinel_1_images[duplicated(Sentinel_1_images)]

# Remove duplicates
Sentinel_1_images <- Sentinel_1_images[!duplicated(Sentinel_1_images)]

# Create a List of the crop list
crop_list <- list()

# Progress bar creation using the number of images as total length
pb = txtProgressBar(min = 0, max = length(Sentinel_1_images), initial = 0, char = " ^-^ ") 

# For loop to normalize the name for a time serie
for (i in 1:length(Sentinel_1_images)){
  # Progress bar
  setTxtProgressBar(pb,i)
  
  # Call each image using the stack function
  Sentinel_1_stack <- stack(Sentinel_1_images[i])
  
  # Crop the raster to the extend of the roi
  crop_list <- crop(Sentinel_1_stack,roi_utm)
  
  # Extraction of Characters to create the names for each image
  # Extract day of the data
  day <- substr(Sentinel_1_images[i], start=37, stop=38)
  
  # Extract month of the data
  mth <- substr(Sentinel_1_images[i], start=35, stop=36)
  
  # Extract year of the data
  yr <- substr(Sentinel_1_images[i], start=31, stop=34)
  
  # Rescale images
  #Rescale <- setValues(crop_list, scales::rescale(values(crop_list), to = c(0, 255)))  
  
  # Save the Raster with a specific name
  #writeRaster(Rescale, filename=file.path("./Sentinel_1_time_serie",paste0(yr,"_",mth,"_", day,"_S1")), format='GTiff', overwrite=T)
  writeRaster(crop_list, 
              filename=file.path("./Sentinel_1_time_serie",paste0(yr,"_",mth,"_", day,"_S1")), 
              format='GTiff', overwrite=T)
  
  # Remove lists
  #rm(Rescale)
  rm(Sentinel_1_stack)
  rm(crop_list)
  rm(day)
  rm(mth)
  rm(yr)
}

################################################################################

# Sentinel-2 images
Sentinel_2_path <- file.path("./Sentinel_2")

# Load all the images in one list.
Sentinel_2_images <- list.files(Sentinel_2_path,
                                full.names = TRUE,
                                pattern = ".tif$")

# Analysis if there are some duplicated
duplicated(Sentinel_2_images)

# Quantify the duplicates
Sentinel_2_images[duplicated(Sentinel_2_images)]

# Remove duplicates
Sentinel_2_images <- Sentinel_2_images[!duplicated(Sentinel_2_images)]

# Create a List of the crop list
crop_list <- list()

# Progress bar creation using the number of images as total length
pb2 = txtProgressBar(min = 0, max = length(Sentinel_2_images), initial = 0, char = " ^-^ ") 

# For loop to normalize the name for a time serie
for (i in 1:length(Sentinel_2_images)){
  # Progress bar
  setTxtProgressBar(pb2,i)
  
  # Call each image using the stack function
  Sentinel_2_stack <- stack(Sentinel_2_images[i])
  
  # Crop the raster to the extend of the roi
  crop_list <- crop(Sentinel_2_stack,roi_utm)
  
  # Extraction of Characters to create the names for each image
  # Extract day of the data
  day <- substr(Sentinel_2_images[i], start=20, stop=21)
  
  # Extract month of the data
  mth <- substr(Sentinel_2_images[i], start=18, stop=19)
  
  # Extract year of the data
  yr <- substr(Sentinel_2_images[i], start=14, stop=17)
  
  # Rescale images
  #Rescale <- setValues(crop_list, scales::rescale(values(crop_list), to = c(0, 255)))  
  
  # Save the Raster with a specific name
  #writeRaster(Rescale, filename=file.path("./Sentinel_2_time_serie",paste0(yr,"_",mth,"_", day,"_S2")), format='GTiff', overwrite=T)
  writeRaster(crop_list, 
              filename=file.path("./Sentinel_2_time_serie",paste0(yr,"_",mth,"_", day,"_S2")), 
              format='GTiff', overwrite=T)
  
  # Remove lists
  #rm(Rescale)
  rm(Sentinel_2_stack)
  rm(crop_list)
  rm(day)
  rm(mth)
  rm(yr)
}
