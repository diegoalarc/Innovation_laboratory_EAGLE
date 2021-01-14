# [Innovation_laboratory_EAGLE](http://eagle-science.org/project/tmt1-innovation-laboratory/)
The innovation laboratory provides the opportunity to conduct a specified research project on a chosen research topic and explore the potential, challenges and limits of Earth Observation and geoanalysis in a practical approach during the 3rd term.
The Innovation Laboratory allows you to analyze one particular topic in your field of interest in depth. It allows to address own research in the field of the study program and offers the basis of practical attained knowledge. It is similar to an internship but allows to do it at the university (esp. our department).

## Content
The content of the innovation laboratory can be decided by each student individually and either a research topic is offered by a lecturer or the student is proposing an own topic. Research topics need to be discussed with and proposed to one EAGLE lecturer who will also be in charge of supervising and grading the students work. Topics of the innovation laboratory can cover all aspects of the EAGLE study program with a strong focus on applied Earth Observation and geoanalysis or its innovative potential for remote sensing sciences. It may comprise topics such as linking spectrometer field studies to remotely sensed data or the exploration of UAV based imagery, as well as space borne earth observation analysis such as time-series derivation for a variety of environmental studies i.e. resource mapping or spatial predictions and statistics of variables.

## Topic:
## Machine Learning process for yield prediction for the "Carmen Rosa" farm located in Chile.

Project carried out with the help of the company Carmen Rosa, who has provided data, both for its weather station and its annual production, in order to carry out an analysis and possible production optimization considering:

- Temperature
- Evapotranspiration
- Humidity
- Annual production
- Shapefile with the precise location of the crops

Image of Carmen Rosa Farm:
![Carmen Rosa farm location](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/blob/main/Plots/Carmen_Rosa_Farm.png?raw=true "Carmen Rosa farm location")

[Kmz of Carmen Rosa Farm](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/raw/main/Original_data/CARMEN_ROSA_farm.kmz)

## Google Earth Engine
To start we created two scripts in GEE to download the images cut with all the bands.

The codes are as follows:
[Sentinel-2](https://code.earthengine.google.com/10bef8017a4fcacec6ef47296e2b9018)

## R Code generated
Then a code was applied to obtain a unique value per field for each band generated by the Google Earth Engine code and for each year, which will be applied in a machine learning model.

The codes are as follows:
[R code](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/blob/main/NDVI_GNDVI_EVI_cellStats_by_Field.R)

## R Code boxplot
In order to get a general idea of each field, an NDVI analysis was performed for each field. For this, an analysis was performed through a boxplot of each NDVI image of each field and a point was placed that represents the mean of the total value of pixels of each image for each year.

The codes are as follows:
[R Boxplot code](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/blob/main/Boxplot.R)

Example of NDVI for each field from the years 2016 -2019 between November and December in the Carmen Rosa Farm:
![Example of NDVI for each field from the years 2016 -2019](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/blob/main/Plots/NDVI_2017_to_2020.png?raw=true "Example of NDVI for each field from the years 2016 -2019")