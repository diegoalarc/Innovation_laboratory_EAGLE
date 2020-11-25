library(raster)
library(cluster)

# Set the folder location
setwd('/home/diego/GITHUP_REPO/Innovation_laboratory_EAGLE')

# Stack firsts images
test <- stack('Sentinel_2/20160901T143752_20160901T144926_T19HCC.tif')

# Rescale images Sentine-2
test2 <- setValues(test, scales::rescale(values(test), to = c(0, 255)))  

# Print our ranges for each band 
cellStats(test2, min)
cellStats(test2, max)

# Stack RGB images
test3 <- stack(test2[[4]], test2[[3]], test2[[2]])

# Plotting RGB image
plotRGB(test3)
