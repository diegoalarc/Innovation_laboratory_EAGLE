# Libraries loading
library(tidyverse)
library(ggplot2)
library(raster) 
library(rgdal)
library(sp)

# Set the folder location
#setwd('~/Dropbox/RS Project/RSSTARTUP_repo/Product_Development/Yield_forcasting_R/Innovation_laboratory_EAGLE')
setwd('/home/diego/GITHUP_REPO/Innovation_laboratory_EAGLE')

# Read Shapefile with the roi
roi <- readOGR(dsn=file.path('./ROI/Carmen_Rosa_Field.shp'))
#plot(roi)

# Change path to folder containing rasters
rasdir <- file.path('./Images')

year <- '2017'
year2 <- '2018'
year3 <- '2019'
year4 <- '2020'

# List all GeoTIFF files in folder, change extension in pattern if different format
fllst <- list.files(path=rasdir,
                    full.names = TRUE,
                    pattern = paste0(year,'.tif$'))

fllst_stack <- stack(fllst[4])

fllst2 <- list.files(path=rasdir,
                     full.names = TRUE,
                     pattern = paste0(year2,'.tif$'))

fllst_stack2 <- stack(fllst2[4])

fllst3 <- list.files(path=rasdir,
                     full.names = TRUE,
                     pattern = paste0(year3,'.tif$'))

fllst_stack3 <- stack(fllst3[4])

fllst4 <- list.files(path=rasdir,
                     full.names = TRUE,
                     pattern = paste0(year4,'.tif$'))

fllst_stack4 <- stack(fllst4[4])

# Band EVi, GNVDI & NVDI
fllst_stack5 <- stack(fllst_stack, fllst_stack2, fllst_stack3, fllst_stack4)

# Progress bar creation using the number of images as total length
pb = txtProgressBar(min = 0, max = 12, initial = 0, char = " ^-^ ") 

df <- list()

for (x in 1:length(roi)){
  # Progress bar
  setTxtProgressBar(pb,x)
  
  print(x)
  
  # Subset the shapefile by Number
  roi_1 <- subset(roi, Number == x)
  #plot(roi_1)
  print(roi_1$Name)
  
  # Create a List of the crop list
  crop_list5 <- list()
  
  # For-loop to crop the raster in order to obtain the study area
  for (i in 1:nlayers(fllst_stack5)){
    crop_list5[[i]] <- mask(fllst_stack5[[i]],roi_1)
  }
  
  # Create a stack of Raster Files with all the *.tiff cropped
  roi_1_crop5 <- stack(crop_list5)
  
  nvdi_names <- c(paste0('NDVI_Carmen_Rosa_Field_',year),
                  paste0('NDVI_Carmen_Rosa_Field_',year2),
                  paste0('NDVI_Carmen_Rosa_Field_',year3),
                  paste0('NDVI_Carmen_Rosa_Field_',year4))
  
  # Save the names in a vector
  names(roi_1_crop5) <- nvdi_names
  
  # Create the data frame with the data by Band
  band_mean <- raster::extract(roi_1_crop5[[1]], roi_1, method='simple',df=TRUE)
  my_df <- matrix(data = roi_1$Name, nrow = 4*nrow(band_mean), ncol = 3)
  my_df <- data.frame(my_df)
  my_df[,2] <- data.frame(Year = year)
  my_df[,3] <- as.numeric(band_mean[,2])
  
  band_mean2 <- raster::extract(roi_1_crop5[[2]], roi_1, method='simple',df=TRUE)
  my_df2 <- matrix(data = roi_1$Name, nrow = 4*nrow(band_mean2), ncol = 3)
  my_df2 <- data.frame(my_df2)
  my_df2[,2] <- data.frame(Year = year2)
  my_df2[,3] <- as.numeric(band_mean2[,2])
  
  band_mean3 <- raster::extract(roi_1_crop5[[3]], roi_1, method='simple',df=TRUE)
  my_df3 <- matrix(data = roi_1$Name, nrow = 4*nrow(band_mean3), ncol = 3)
  my_df3 <- data.frame(my_df3)
  my_df3[,2] <- data.frame(Year = year3)
  my_df3[,3] <- as.numeric(band_mean3[,2])
  
  band_mean4 <- raster::extract(roi_1_crop5[[4]], roi_1, method='simple',df=TRUE)
  my_df4 <- matrix(data = roi_1$Name, nrow = 4*nrow(band_mean4), ncol = 3)
  my_df4 <- data.frame(my_df4)
  my_df4[,2] <- data.frame(Year = year4)
  my_df4[,3] <- as.numeric(band_mean4[,2])
  
  my_df5 <- rbind(my_df,my_df2,my_df3,my_df4)
  
  # Add ncolumn names into the dataframe
  names(my_df5) <- c('Field', 'Year', 'NDVI')
  
  df[[x]] <- my_df5
  
  # Save dataframe as .CSV
#  write.csv(my_df5,paste0('./Plots/',roi_1$Name,'_NDVI_2017_to_2020.csv'),row.names = F)
  
  # Save boxplot as .png
#  png(file = paste0('./Plots/',roi_1$Name,'_NDVI_2017_to_2020.png'), units = "px",
#      width = 1000, height = 450)
  
  # plot
#  p <- ggplot(my_df5, aes(x=Year, y=NDVI)) +
#    geom_boxplot(aes(fill=Field),alpha=0.7) +
#    stat_summary(fun=mean, geom="point", shape=20, size=7, color="red", fill="red") +
#    theme(legend.position="none") +
#    scale_fill_brewer(palette="Set1")
  
#  dev.off()
}

big_data <- do.call(rbind, df)

# Save dataframe as .CSV
write.csv(big_data,'./Plots/NDVI_2017_to_2020.csv',row.names = F)

# plot
p <- ggplot(data = big_data, aes(x=Year, y=NDVI)) + 
  geom_boxplot(aes(fill=Field)) + 
  stat_summary(fun=mean, geom="line", aes(group=Field)) +
  stat_summary(fun=mean, geom="point", shape=20, size=3, color="red", fill="red") +
  facet_wrap(~Field,ncol = 4)

# Save boxplot as .png
png(file = paste0('./Plots/NDVI_2017_to_2020.png'), units = "px",
    width = 1000, height = 450)

plot(p)
dev.off()