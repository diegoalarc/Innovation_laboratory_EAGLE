library(raster)
library(cluster)
library(rgdal)
library(RStoolbox)
library(tidyverse)

################################################################################

# Set the folder location
setwd('/home/diego/GITHUP_REPO/Innovation_laboratory_EAGLE')

# Raster firsts images
test <- raster('Sentinel_2/20160901T143752_20160901T144926_T19HCC.tif')

# Rescale images Sentine-2
test2 <- setValues(test, scales::rescale(values(test), to = c(0, 255)))  

# Print our ranges for each band 
cellStats(test2, min)
cellStats(test2, max)

# Stack RGB images
test3 <- stack(test2[[4]], test2[[3]], test2[[2]])

# Plotting RGB image
plotRGB(test3)

# Read Shapefile with the roi
roi <- readOGR(dsn=file.path("./ROI/EQUIPO_DE_RIEGO_CARMEN_ROSA_Parron.shp"))
#plot(roi, axes = TRUE)

# Reproject to UTM zone 19S
roi_utm <- spTransform(roi, CRS("+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs"))
#roi_utm

# Plotting RGB and Shapefile together
plotRGB(test3,  stretch = 'hist', axes = TRUE,
        main = 'Equipo de riego Carmen Rosa Parrón Español / 2016-09-01')
plot(roi_utm, border = 'red', add = TRUE)

################################################################################

# Sentinel-1 images
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

# Progress bar creation using the number of images as total length
pb = txtProgressBar(min = 0, max = length(Sentinel_1_images), initial = 0, char = " ^-^ ") 

# For loop to normalize the name for a time serie
for (i in 1:length(Sentinel_1_images)){
  # Progress bar
  setTxtProgressBar(pb,i)
  
  # Call each image using the stack function
  Sentinel_1_stack <- stack(Sentinel_1_images[i])
  
  # Extraction of Characters to create the names for each image
  # Extract day of the data
  day <- substr(Sentinel_1_images[i], start=37, stop=38)
  
  # Extract month of the data
  mth <- substr(Sentinel_1_images[i], start=35, stop=36)
  
  # Extract year of the data
  yr <- substr(Sentinel_1_images[i], start=31, stop=34)
  
  # Save the Raster with a specific name
  writeRaster(Sentinel_1_stack, filename=file.path("./Sentinel_1_time_serie",paste0(yr,"_",mth,"_", day)), format='GTiff', overwrite=T)
  
  # Remove lists
  rm(Sentinel_1_stack)
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

# Progress bar creation using the number of images as total length
pb2 = txtProgressBar(min = 0, max = length(Sentinel_2_images), initial = 0, char = " ^-^ ") 

# For loop to normalize the name for a time serie
for (i in 1:length(Sentinel_2_images)){
  # Progress bar
  setTxtProgressBar(pb2,i)
  
  # Call each image using the stack function
  Sentinel_2_stack <- stack(Sentinel_2_images[i])
  
  # Extraction of Characters to create the names for each image
  # Extract day of the data
  day <- substr(Sentinel_2_images[i], start=20, stop=21)
  
  # Extract month of the data
  mth <- substr(Sentinel_2_images[i], start=18, stop=19)
  
  # Extract year of the data
  yr <- substr(Sentinel_2_images[i], start=14, stop=17)
  
  # Save the Raster with a specific name
  writeRaster(Sentinel_2_stack, filename=file.path("./Sentinel_2_time_serie",paste0(yr,"_",mth,"_", day)), format='GTiff', overwrite=T)
  
  # Remove lists
  rm(Sentinel_2_stack)
}