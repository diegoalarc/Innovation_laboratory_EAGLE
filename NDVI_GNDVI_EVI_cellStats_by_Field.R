################################################################################

library(raster)
library(cluster)
library(rgdal)
library(RStoolbox)
library(tidyverse)
library(caret)

################################################################################

# Set the folder location
#setwd('~/Dropbox/RS Project/RSSTARTUP_repo/Product_Development/Yield_forcasting_R/Innovation_laboratory_EAGLE')
setwd('/home/diego/GITHUP_REPO/Innovation_laboratory_EAGLE')

# Read Shapefile with the roi
roi <- readOGR(dsn=file.path('./ROI/Carmen_Rosa_Field.shp'))
#plot(roi)

# Subset the shapefile by Number
roi_1 <- subset(roi, Number == '1')
#plot(roi_1)
roi_1$Name

# Change path to folder containing rasters
rasdir <- file.path('./Images')

################################################################################

year <- '2017'

# List all GeoTIFF files in folder, change extension in pattern if different format
fllst <- list.files(path=rasdir,
                    full.names = TRUE,
                    pattern = paste0(year,'.tif$'))

fllst_stack <- stack(fllst)
fllst_brick <- brick(fllst)

# Create a List of the crop list
crop_list <- list()

# Activation of the cores in the device and focus these in the following process
beginCluster()

# For-loop to crop the raster in order to obtain the study area
for (i in 1:nlayers(fllst_stack)){
  crop_list[[i]] <- mask(fllst_stack[[i]],roi_1)
}

# Create the vector with the name file
names_file <- vector(mode='character')

names_file <- names(fllst_brick)

# For-loop to obtain the name file for all the raster in one vector file
# which will be used when the rasters file will be saved
for (i in 14:length(fllst_stack)){
  names_file[[i]] <- names(fllst_stack[[i]])
}

# Create a stack of Raster Files with all the *.tiff cropped
roi_1_crop <- stack(crop_list)

# Save the names in a vector
names(roi_1_crop) <- names_file

################################################################################

year2 <- '2018'

# List all GeoTIFF files in folder, change extension in pattern if different format
fllst2 <- list.files(path=rasdir,
                    full.names = TRUE,
                    pattern = paste0(year2,'.tif$'))

fllst_stack2 <- stack(fllst2)
fllst_brick2 <- brick(fllst2)

# Create a List of the crop list
crop_list2 <- list()

# For-loop to crop the raster in order to obtain the study area
for (i in 1:nlayers(fllst_stack2)){
  crop_list2[[i]] <- mask(fllst_stack2[[i]],roi_1)
}

# Create the vector with the name file
names_file2 <- vector(mode='character')

names_file2 <- names(fllst_brick2)

# For-loop to obtain the name file for all the raster in one vector file
# which will be used when the rasters file will be saved
for (i in 14:length(fllst_stack2)){
  names_file2[[i]] <- names(fllst_stack2[[i]])
}

# Create a stack of Raster Files with all the *.tiff cropped
roi_1_crop2 <- stack(crop_list2)

# Save the names in a vector
names(roi_1_crop2) <- names_file2

################################################################################

year3 <- '2019'

# List all GeoTIFF files in folder, change extension in pattern if different format
fllst3 <- list.files(path=rasdir,
                     full.names = TRUE,
                     pattern = paste0(year3,'.tif$'))

fllst_stack3 <- stack(fllst3)
fllst_brick3 <- brick(fllst3)

# Create a List of the crop list
crop_list3 <- list()

# For-loop to crop the raster in order to obtain the study area
for (i in 1:nlayers(fllst_stack3)){
  crop_list3[[i]] <- mask(fllst_stack3[[i]],roi_1)
}

# Create the vector with the name file
names_file3 <- vector(mode='character')

names_file3 <- names(fllst_brick3)

# For-loop to obtain the name file for all the raster in one vector file
# which will be used when the rasters file will be saved
for (i in 14:length(fllst_stack3)){
  names_file3[[i]] <- names(fllst_stack3[[i]])
}

# Create a stack of Raster Files with all the *.tiff cropped
roi_1_crop3 <- stack(crop_list3)

# Save the names in a vector
names(roi_1_crop3) <- names_file3

################################################################################

year4 <- '2020'

# List all GeoTIFF files in folder, change extension in pattern if different format
fllst4 <- list.files(path=rasdir,
                     full.names = TRUE,
                     pattern = paste0(year4,'.tif$'))

fllst_stack4 <- stack(fllst4)
fllst_brick4 <- brick(fllst4)

# Create a List of the crop list
crop_list4 <- list()

# For-loop to crop the raster in order to obtain the study area
for (i in 1:nlayers(fllst_stack4)){
  crop_list4[[i]] <- mask(fllst_stack4[[i]],roi_1)
}

# Create the vector with the name file
names_file4 <- vector(mode='character')

names_file4 <- names(fllst_brick4)

# For-loop to obtain the name file for all the raster in one vector file
# which will be used when the rasters file will be saved
for (i in 14:length(fllst_stack4)){
  names_file4[[i]] <- names(fllst_stack4[[i]])
}

# Create a stack of Raster Files with all the *.tiff cropped
roi_1_crop4 <- stack(crop_list4)

# Save the names in a vector
names(roi_1_crop4) <- names_file4

# Disabling the cores on the device when the process ends
endCluster()

################################################################################
# Plot Area to observe scenes

# Select band
#band <- 15

# Plotting RGB and Shapefile together
#plot(roi_1_crop[[1]],  stretch = 'lin', axes = TRUE, ext=roi_1,
#     main = paste(roi_1$Name,'/ Band:',names(roi_1_crop[[band]]),'/ Year:',year,'/ Sentinel-2'))
#plot(roi_1, border = 'red', add = TRUE)

################################################################################

# Create the data frame with the data by Band
my_df <- data.frame(names_file,stringsAsFactors=FALSE)

for (i in 1:nlayers(roi_1_crop)){
  band_mean <- cellStats(roi_1_crop[[i]], 'mean', progress = 'text')
  my_df[i,2] <- as.numeric(band_mean)
  
  band_mean2 <- cellStats(roi_1_crop2[[i]], 'mean', progress = 'text')
  my_df[i,3] <- as.numeric(band_mean2)
  
  band_mean3 <- cellStats(roi_1_crop3[[i]], 'mean', progress = 'text')
  my_df[i,4] <- as.numeric(band_mean3)
  
  band_mean4 <- cellStats(roi_1_crop4[[i]], 'mean', progress = 'text')
  my_df[i,5] <- as.numeric(band_mean4)
}
names(my_df) <- c('band', paste0('mean_',year), paste0('mean_',year2), paste0('mean_',year3),paste0('mean_',year4))

################################################################################

library(data.table)

my_df_or <- transpose(my_df)
colnames(my_df_or) <- rownames(my_df)
rownames(my_df_or) <- colnames(my_df)
my_df_or <- my_df_or[-1,]
my_df_or[,17] <- c(year,year2,year3,year4)
names(my_df_or) <- c('B1_mean', 'B2_mean', 'B3_mean', 'B4_mean', 'B5_mean', 'B6_mean',
                     'B9_mean', 'B8_mean', 'B8A_mean', 'B9_mean', 'B10_mean', 'B11_mean',
                     'B12_mean', 'EVI', 'GNDVI', 'NDVI', 'Year')

# Observe the Structure of my main dataframe.
str(my_df)

# prepare the data
EVI <- as.numeric(my_df_or$EVI)
GNDVI <- as.numeric(my_df_or$GNDVI)
NDVI <- as.numeric(my_df_or$NDVI)
# gererate normal distribution with same mean and sd
EVI_norm <- rnorm(200,mean=mean(EVI, na.rm=TRUE), sd=sd(EVI, na.rm=TRUE))
GNDVI_norm <- rnorm(200,mean=mean(GNDVI, na.rm=TRUE), sd=sd(GNDVI, na.rm=TRUE))
NDVI_norm <- rnorm(200,mean=mean(NDVI, na.rm=TRUE), sd=sd(NDVI, na.rm=TRUE))

# Boxplot by Field using the data from 2017-2020 generated by GEE 
boxplot(EVI, EVI_norm, GNDVI, GNDVI_norm, NDVI, NDVI_norm,
        main = paste0('Multiple boxplots for comparision 2017-2020 for the Field ', roi_1$Name),
        at = c(1,2,4,5,7,8),
        names = c('EVI', 'normal distribution', 
                  'GNDVI', 'normal distribution', 
                  'NDVI', 'normal distribution'),
        las = 1,
        col = c('orange','red'),
        border = 'brown',
        horizontal = F,
        notch = F,
        ylim = c(0, 1))

write.csv(my_df_or,paste0('./Original_data/',roi_1$Name,'.csv'),row.names = T)
