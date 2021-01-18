################################################################################

library(raster)
library(cluster)
library(rgdal)
library(RStoolbox)
library(tidyverse)
library(caret)
library(data.table)

################################################################################

# Set the folder location
#setwd('~/Dropbox/RS Project/RSSTARTUP_repo/Product_Development/Yield_forcasting_R/Innovation_laboratory_EAGLE')
setwd('/home/diego/GITHUP_REPO/Innovation_laboratory_EAGLE')

# Read Shapefile with the roi
roi <- readOGR(dsn=file.path('./ROI/Carmen_Rosa_Field.shp'))
#plot(roi)

# Change path to folder containing rasters
rasdir <- file.path('./Images')

# Activation of the cores in the device and focus these in the following process
#beginCluster()

# Progress bar creation using the number of images as total length
pb = txtProgressBar(min = 0, max = 12, initial = 0, char = " ^-^ ") 

year <- '2017'
year2 <- '2018'
year3 <- '2019'
year4 <- '2020'
year5 <- '2021'

# List all GeoTIFF files in folder, change extension in pattern if different format
fllst <- list.files(path=rasdir,
                    full.names = TRUE,
                    pattern = paste0(year,'.tif$'))

fllst_stack <- stack(fllst)

fllst2 <- list.files(path=rasdir,
                     full.names = TRUE,
                     pattern = paste0(year2,'.tif$'))

fllst_stack2 <- stack(fllst2)

fllst3 <- list.files(path=rasdir,
                     full.names = TRUE,
                     pattern = paste0(year3,'.tif$'))

fllst_stack3 <- stack(fllst3)

fllst4 <- list.files(path=rasdir,
                     full.names = TRUE,
                     pattern = paste0(year4,'.tif$'))

fllst_stack4 <- stack(fllst4)

fllst5 <- list.files(path=rasdir,
                     full.names = TRUE,
                     pattern = paste0(year5,'.tif$'))

fllst_stack5 <- stack(fllst5)

##############################################################################

# Create a list
df_band <- list()

for (x in 1:length(roi)){
  # Progress bar
  setTxtProgressBar(pb,x)
  
  print(x)
  
  # Subset the shapefile by Number
  roi_1 <- subset(roi, Number == x)
  #plot(roi_1)
  print(roi_1$Name)
  
  # Create a List of the crop list
  crop_list <- list()
  
  # For-loop to crop the raster in order to obtain the study area
  for (i in 1:nlayers(fllst_stack)){
    crop_list[[i]] <- mask(fllst_stack[[i]],roi_1)
  }
  
  # Create a stack of Raster Files with all the *.tiff cropped
  roi_1_crop <- stack(crop_list)
  
  names_file <- c('B1_mean', 'B2_mean', 'B3_mean', 'B4_mean', 'B5_mean', 'B6_mean',
                  'B7_mean', 'B8_mean', 'B8A_mean', 'B9_mean', 'B10_mean', 'B11_mean',
                  'B12_mean', paste0('EVI_Carmen_Rosa_Field_',year), 
                  paste0('GNDVI_Carmen_Rosa_Field_',year),
                  paste0('NDVI_Carmen_Rosa_Field_',year))
  
  # Save the names in a vector
  names(roi_1_crop) <- names_file
  
  ##############################################################################
  
  # Create a List of the crop list
  crop_list2 <- list()
  
  # For-loop to crop the raster in order to obtain the study area
  for (i in 1:nlayers(fllst_stack2)){
    crop_list2[[i]] <- mask(fllst_stack2[[i]],roi_1)
  }
  
  # Create a stack of Raster Files with all the *.tiff cropped
  roi_1_crop2 <- stack(crop_list2)
  
  # Create the vector with the name file
  names_file2 <- vector(mode='character')
  
  names_file2 <- c('B1_mean', 'B2_mean', 'B3_mean', 'B4_mean', 'B5_mean', 'B6_mean',
                   'B7_mean', 'B8_mean', 'B8A_mean', 'B9_mean', 'B10_mean', 'B11_mean',
                   'B12_mean', paste0('EVI_Carmen_Rosa_Field_',year2), 
                   paste0('GNDVI_Carmen_Rosa_Field_',year2),
                   paste0('NDVI_Carmen_Rosa_Field_',year2))
  
  # Save the names in a vector
  names(roi_1_crop2) <- names_file2
  
  ##############################################################################
  
  # Create a List of the crop list
  crop_list3 <- list()
  
  # For-loop to crop the raster in order to obtain the study area
  for (i in 1:nlayers(fllst_stack3)){
    crop_list3[[i]] <- mask(fllst_stack3[[i]],roi_1)
  }
  
  # Create a stack of Raster Files with all the *.tiff cropped
  roi_1_crop3 <- stack(crop_list3)
  
  # Create the vector with the name file
  names_file3 <- vector(mode='character')
  
  names_file3 <- c('B1_mean', 'B2_mean', 'B3_mean', 'B4_mean', 'B5_mean', 'B6_mean',
                   'B7_mean', 'B8_mean', 'B8A_mean', 'B9_mean', 'B10_mean', 'B11_mean',
                   'B12_mean', paste0('EVI_Carmen_Rosa_Field_',year3), 
                   paste0('GNDVI_Carmen_Rosa_Field_',year3),
                   paste0('NDVI_Carmen_Rosa_Field_',year3))
  
  # Save the names in a vector
  names(roi_1_crop3) <- names_file3
  
  ##############################################################################
  
  # Create a List of the crop list
  crop_list4 <- list()
  
  # For-loop to crop the raster in order to obtain the study area
  for (i in 1:nlayers(fllst_stack4)){
    crop_list4[[i]] <- mask(fllst_stack4[[i]],roi_1)
  }
  
  # Create a stack of Raster Files with all the *.tiff cropped
  roi_1_crop4 <- stack(crop_list4)
  
  # Create the vector with the name file
  names_file4 <- vector(mode='character')
  
  names_file4 <- c('B1_mean', 'B2_mean', 'B3_mean', 'B4_mean', 'B5_mean', 'B6_mean',
                   'B7_mean', 'B8_mean', 'B8A_mean', 'B9_mean', 'B10_mean', 'B11_mean',
                   'B12_mean', paste0('EVI_Carmen_Rosa_Field_',year4), 
                   paste0('GNDVI_Carmen_Rosa_Field_',year4),
                   paste0('NDVI_Carmen_Rosa_Field_',year4))
  
  # Save the names in a vector
  names(roi_1_crop4) <- names_file4
  
  ##############################################################################
  
  # Create a List of the crop list
  crop_list5 <- list()
  
  # For-loop to crop the raster in order to obtain the study area
  for (i in 1:nlayers(fllst_stack5)){
    crop_list5[[i]] <- mask(fllst_stack5[[i]],roi_1)
  }
  
  # Create a stack of Raster Files with all the *.tiff cropped
  roi_1_crop5 <- stack(crop_list5)
  
  # Create the vector with the name file
  names_file5 <- vector(mode='character')
  
  names_file5 <- c('B1_mean', 'B2_mean', 'B3_mean', 'B4_mean', 'B5_mean', 'B6_mean',
                   'B7_mean', 'B8_mean', 'B8A_mean', 'B9_mean', 'B10_mean', 'B11_mean',
                   'B12_mean', paste0('EVI_Carmen_Rosa_Field_',year5), 
                   paste0('GNDVI_Carmen_Rosa_Field_',year5),
                   paste0('NDVI_Carmen_Rosa_Field_',year5))
  
  # Save the names in a vector
  names(roi_1_crop5) <- names_file5
  
  ##############################################################################
  
  # Plot Area to observe scenes
    # Select band
  #band <- 15
  
  # Plotting RGB and Shapefile together
  #plot(roi_1_crop[[1]],  stretch = 'lin', axes = TRUE, ext=roi_1,
  #     main = paste(roi_1$Name,'/ Band:',names(roi_1_crop[[band]]),'/ Year:',year,'/ Sentinel-2'))
  #plot(roi_1, border = 'red', add = TRUE)
  
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
    
    band_mean5 <- cellStats(roi_1_crop5[[i]], 'mean', progress = 'text')
    my_df[i,6] <- as.numeric(band_mean5)
  }
  names(my_df) <- c('band', 
                    paste0('mean_',year), paste0('mean_',year2), 
                    paste0('mean_',year3),paste0('mean_',year4), 
                    paste0('mean_',year5))
  
  my_df_or <- transpose(my_df)
  colnames(my_df_or) <- rownames(my_df)
  rownames(my_df_or) <- colnames(my_df)
  my_df_or <- my_df_or[-1,]
  my_df_or[,17] <- c(year,year2,year3,year4,year5)
  my_df_or[,18] <- c(roi_1$Name,roi_1$Name,roi_1$Name,roi_1$Name,roi_1$Name)
  names(my_df_or) <- c('B1_mean', 'B2_mean', 'B3_mean', 'B4_mean', 'B5_mean', 'B6_mean',
                       'B7_mean', 'B8_mean', 'B8A_mean', 'B9_mean', 'B10_mean', 'B11_mean',
                       'B12_mean', 'EVI', 'GNDVI', 'NDVI', 'Year', 'Field´')
  
  # Create a List using the iterator
  df_band[[x]] <- my_df_or

}

# Execute a Function rbind
band_big_data <- do.call(rbind, df_band)

# Save dataframe as .CSV
write.csv(band_big_data,'./Original_data/Band_2017_to_2021.csv',row.names = F, quote = F)

# Disabling the cores on the device when the process ends
#endCluster()