library(raster)
library(cluster)
library(rgdal)
library(RStoolbox)

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
plot(roi, axes = TRUE)

roi_utm <- spTransform(roi, CRS("+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs"))
roi_utm

# Plotting RGB and Shapefile together
plotRGB(test3,  stretch = 'hist', axes = TRUE,
        main = 'Equipo de riego Carmen Rosa Parrón Español')
plot(roi_utm, border = 'red', add = TRUE)

################################################################################

# Sentinel-2 images
Sentinel_2_path <- file.path("./Sentinel_2")

# Load all the images in one list.
Sentinel_2_images <- list.files(Sentinel_2_path,
                              full.names = TRUE,
                              pattern = ".tif$")

# Create a stack of Raster Files with all the *.tiff Sentinel_2_images
Sentinel_2_brick <- brick(Sentinel_2_images)
