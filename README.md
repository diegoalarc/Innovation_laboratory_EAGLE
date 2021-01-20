# [Innovation_laboratory_EAGLE](http://eagle-science.org/project/tmt1-innovation-laboratory/)
The innovation laboratory provides the opportunity to conduct a specified research project on a chosen research topic and explore the potential, challenges and limits of Earth Observation and geoanalysis in a practical approach during the 3rd term.
The Innovation Laboratory allows you to analyze one particular topic in your field of interest in depth. It allows to address own research in the field of the study program and offers the basis of practical attained knowledge. It is similar to an internship but allows to do it at the university (esp. our department).

---

## Content
The content of the innovation laboratory can be decided by each student individually and either a research topic is offered by a lecturer or the student is proposing an own topic. Research topics need to be discussed with and proposed to one EAGLE lecturer who will also be in charge of supervising and grading the students work. Topics of the innovation laboratory can cover all aspects of the EAGLE study program with a strong focus on applied Earth Observation and geoanalysis or its innovative potential for remote sensing sciences. It may comprise topics such as linking spectrometer field studies to remotely sensed data or the exploration of UAV based imagery, as well as space borne earth observation analysis such as time-series derivation for a variety of environmental studies i.e. resource mapping or spatial predictions and statistics of variables.

---

## Topic:
### Machine Learning process for yield prediction for the "Carmen Rosa" farm located in Chile.

Project carried out with the help of the company Carmen Rosa, who has provided data, both for its weather station and its annual production, in order to carry out an analysis and possible production optimization considering:

- Temperature
- Evapotranspiration
- Humidity
- Annual production
- Shapefile with the precise location of the crops

Image of Carmen Rosa Farm:
![Carmen Rosa farm location](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/blob/main/Plots/Carmen_Rosa_Farm.png?raw=true "Carmen Rosa farm location")

#### For better precision, check the location by downloading the following kmz:
 - [Kmz of Carmen Rosa Farm](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/raw/main/Original_data/CARMEN_ROSA_farm.kmz)

---

## Google Earth Engine
To carry out this project, an algorithm was generated through Google Earth Engine to obtain the images between November and December from 2016 to 2019, which represent the most important phenological stage for these grapes in this area of Chile. In the algorithm the images of GNDVI, NDVI and EVI were generated, which give us information about the crops and their health. Within the same algorithm, a mean of all the images for all their bands was obtained, which were downloaded and processed by the algorithms generated in R.
To obtain the information for the prediction of the year 2021, this algorithm was also used to obtain the desired images and bands.

The codes are as follows:
[Sentinel-2](https://code.earthengine.google.com/10bef8017a4fcacec6ef47296e2b9018)

---

## R Code generated
Then a code was applied to obtain a unique value per field for each band generated by the Google Earth Engine code and for each year, which will be applied in a machine learning model.

The codes are as follows:
[R code](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/blob/main/NDVI_GNDVI_EVI_cellStats_by_Field.R)

---

## R Code boxplot
In order to get a general idea of each field, an NDVI analysis was performed for each field. For this, an analysis was performed through a boxplot of each NDVI image of each field and a point was placed that represents the mean of the total value of pixels of each image for each year.

The codes are as follows:
[R Boxplot code](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/blob/main/Boxplot.R)

#### Considerations when reading the graph:
For the fields 'INIAGRAPE ONE 15' and 'SABLE 16', only the years 2018 - 2019 are valid, since they did not present production in the years 2016 - 2017. This data was eliminated from the dataframe to create the machine learning model.

#### Boxplot:
![Example of NDVI for each field from the years 2016 -2019](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/blob/main/Plots/NDVI_2017_to_2021.png?raw=true "Example of NDVI for each field from the years 2016 -2019")

## R Code [Caret Package](https://topepo.github.io/caret/index.html)
Applying the [Caret](https://topepo.github.io/caret/index.html) de R package, a [Machine Learning](https://en.wikipedia.org/wiki/Machine_learning) model was made to make a prediction of the Yield of Carmen Rosa farm.
According to what has been done, the need for more data to make a prediction with better precision is evident. But considering the lack of these, it was decided to make (anyway) an [ML](https://en.wikipedia.org/wiki/Machine_learning) model, considering that it will improve over the years as more information is available.

### Evaluation and selection of Hyperparameters

When analyzing the values for tuning in the [Random Forest](https://en.wikipedia.org/wiki/Random_forest#:~:text=Random%20forests%20or%20random%20decision,average%20prediction%20(regression)%20of%20the) model and in the Conditional Random Forest model, the Caret package was used, which provides an analysis of the values and search for the best one for the model evaluation and for a better application. results.

Random Forest hyperparameters:
```r
44 samples
41 predictors

No pre-processing
Resampling: Cross-Validated (4 fold, repeated 100 times) 
Summary of sample sizes: 36, 32, 32, 32, 32, 34, ... 
Resampling results across tuning parameters:

  mtry  RMSE      Rsquared   MAE     
   1    11696.03  0.2650583  8908.104
   2    11644.86  0.2573990  8595.478
   3    11725.92  0.2491017  8578.859
   4    11801.20  0.2426962  8602.588
   5    11848.96  0.2400758  8615.154
   6    11893.66  0.2365719  8630.176
   7    11928.81  0.2357133  8647.704
   8    11962.77  0.2330954  8676.169
   9    11984.86  0.2317039  8684.750
  10    12009.54  0.2305464  8693.922
  11    12015.17  0.2310420  8703.591
  12    12045.41  0.2280370  8713.845
  13    12050.01  0.2293283  8715.078
  14    12076.34  0.2270550  8737.591
  15    12083.33  0.2269244  8741.872
  16    12087.71  0.2266707  8737.902
  17    12098.06  0.2262823  8746.408
  18    12106.52  0.2260254  8755.035
  19    12109.42  0.2262130  8756.194
  20    12124.95  0.2252901  8771.455

RMSE was used to select the optimal model using the smallest value.
The final value used for the model was mtry = 2.

predictions_rf
       2        6        7       15       16       23       24       27       31       35 
37649.06 50594.18 22991.68 48395.64 47672.84 32429.35 31227.15 48233.68 34352.09 37294.73 
      38       39 
51572.88 44684.72
```
![RMS vs Ramdom Predictors Random Forest model](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/blob/main/Plots/RMSE_vs_Ramdom_Predictors_rforest.png?raw=true "RMS vs Ramdom Predictors Random Forest model")


Conditional Random Forest hyperparameters:
```r
44 samples
41 predictors

No pre-processing
Resampling: Cross-Validated (4 fold, repeated 100 times) 
Summary of sample sizes: 36, 32, 32, 32, 32, 34, ... 
Resampling results across tuning parameters:

  mtry  RMSE      Rsquared   MAE      
   1    12475.15  0.2352746  10076.645
   2    12262.31  0.2360650   9875.970
   3    12149.49  0.2341696   9743.101
   4    12064.60  0.2347005   9635.064
   5    12022.64  0.2346035   9573.056
   6    11995.39  0.2326416   9528.815
   7    11981.69  0.2305238   9498.562
   8    11967.76  0.2313585   9472.172
   9    11966.86  0.2311443   9458.778
  10    11965.38  0.2307080   9440.515
  11    11967.84  0.2299629   9435.052
  12    11971.26  0.2270257   9425.480
  13    11978.07  0.2240470   9421.372
  14    11970.50  0.2259004   9402.157
  15    11986.66  0.2252591   9414.824
  16    11986.99  0.2239518   9403.290
  17    11998.19  0.2219648   9410.803
  18    11995.38  0.2218385   9395.224
  19    12001.60  0.2215265   9390.147
  20    11998.97  0.2213567   9374.369

RMSE was used to select the optimal model using the smallest value.
The final value used for the model was mtry = 10.

predictions_crf
 [1] 39555.69 45965.34 31059.18 48257.10 47660.85 34870.79 34825.09 43228.54 35510.17 35801.68
[11] 48564.00 44670.39
```
![RMS vs Ramdom Predictors Conditional Random Forest model](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/blob/main/Plots/RMSE_vs_Ramdom_Predictors_cforest.png?raw=true "RMS vs Ramdom Predictors Conditional Random Forest model")

### Plot of variable importance
Something important when looking at the generated model, is variable importance, which is possible thanks to Caret's "varImp" function.

- RandomForest model:
![Variable importance ML model](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/blob/main/Plots/Variable_Importance_rforest_ggplo2.png?raw=true "Variable importance ML model")

- Conditional RandomForest model:
![Variable importance ML model](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/blob/main/Plots/Variable_Importance_cforest_ggplo2.png?raw=true "Variable importance ML model")

We have also calculate Feature Importance Explanations As Loss From Feature Dropout.

- RandomForest model:
![Variable importance boxplot ML model](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/blob/main/Plots/Variable_Importance_boxplot_rforest_ggplo2.png?raw=true "Variable importance boxplot ML model")

- Conditional RandomForest model:
![Variable importance boxplot ML model](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/blob/main/Plots/Variable_Importance_boxplot_crforest_ggplo2.png?raw=true "Variable importance boxplot ML model")

### Values of R2, RMSE & MAE
These are the values with the best performance, after the auto-selection of the tuning through the use of the Caret package.

- RandomForest model:

| `R2` | `RMSE Kg` | `MAE Kg` |
| :-------: | :------: | :-----: |
| 0.9469607 | 3034.6863584 | 2311.3509723 |

- Conditional RandomForest model:

| `R2` | `RMSE Kg` | `MAE Kg` |
| :-------: | :------: | :-----: |
| 0.8286274 | 5916.6067670 | 4609.4892570 |

### Model comparison

To make this comparison, two functions of the Caret package were used:

1. A resamples of the train generated by each model was carried out.
2. A boxplot of these was generated by analyzing their Rsquared.

- Rsquared's Bloxplot:

![Rsquared's Bloxplot](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/blob/main/Plots/RF_vs_cRF_Boxplot_Rsquared.png?raw=true "Rsquared's Bloxplot")
