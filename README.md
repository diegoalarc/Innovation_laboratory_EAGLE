!["Uni Wuerzburg"](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/blob/main/EAGLE_logo.png?raw=true "EAGLE Msc")

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# [Innovation laboratory EAGLE](http://eagle-science.org/project/tmt1-innovation-laboratory/)
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
- Fields area
- Grade day
- Annual production
- KMZ (converted to Shapefile) with the precise location of the crops

Image of Carmen Rosa Farm:
![Carmen Rosa farm location](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/blob/main/Plots/Carmen_Rosa_Farm.png?raw=true "Carmen Rosa farm location")

#### For better precision, check the location by downloading the following kmz:
 - [Kmz of Carmen Rosa Farm](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/raw/main/Original_data/CARMEN_ROSA_farm.kmz)

---

## Google Earth Engine
To carry out this project, an algorithm was generated through Google Earth Engine to obtain the images between November and December from 2016 to 2019, which represent the most important phenological stage for these grapes in this area of Chile. In the algorithm the images of GNDVI, NDVI and EVI were generated, which give us information about the crops and their health. Within the same algorithm, a mean of all the images for all their bands was obtained, which were downloaded and processed by the algorithms generated in R.
To obtain the information for the prediction of the year 2021, this algorithm was also used..

The codes are as follows:
[Sentinel-2](https://code.earthengine.google.com/10bef8017a4fcacec6ef47296e2b9018)

---

## R Code generated
Then a code was applied to obtain a unique value per field for each band generated by the Google Earth Engine code and for each year, only with the data obtained for the desired phenological stage, which will be applied in a machine learning model.

The codes are as follows:
[R code](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/blob/main/NDVI_GNDVI_EVI_cellStats_by_Field.R)

---

## R Code boxplot
In order to get a general idea of each field, an NDVI analysis was performed for each field. For this, an analysis was performed through a boxplot of each NDVI image of each field and a point was placed that represents the mean of the total value of pixels of each image for each year.

The codes are as follows:
[R Boxplot code](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/blob/main/Boxplot.R)

##### Considerations when reading the graph:
```r
For the fields 'INIAGRAPE ONE 15' and 'SABLE 16', only the years 2018 to 2020 are valid.
The other data was removed from the data frame to create the machine learning model.
```

#### Boxplot:
![Example of NDVI for each field from the years 2016 -2020](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/blob/main/Plots/NDVI_2017_to_2021.png?raw=true "Example of NDVI for each field from the years 2016 -2020")

---

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
   2    11644.86  0.2573990  8595.478  <----- mtry selected
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
ID           2        6        7       15       16       23 
Kg_He 37649.06 50594.18 22991.68 48395.64 47672.84 32429.35

ID          24       27       31       35       38       39 
Kg_He 31227.15 48233.68 34352.09 37294.73 51572.88 44684.72
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
  10    11965.38  0.2307080   9440.515  <----- mtry selected
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
ID           2        6        7       15       16       23
Kg_He 39555.69 45965.34 31059.18 48257.10 47660.85 34870.79

ID          24       27       31       35       38       39 
Kg_He 34825.09 43228.54 35510.17 35801.68 48564.00 44670.39
```
![RMS vs Ramdom Predictors Conditional Random Forest model](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/blob/main/Plots/RMSE_vs_Ramdom_Predictors_cforest.png?raw=true "RMS vs Ramdom Predictors Conditional Random Forest model")

### Plot of variable importance
Something important when looking at the generated model, is variable importance, which is possible thanks to Caret's "varImp" function.

- RandomForest model:
![Variable importance ML model](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/blob/main/Plots/Variable_Importance_rforest_ggplo2.png?raw=true "Variable importance ML model")

- Conditional RandomForest model:
![Variable importance ML model](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/blob/main/Plots/Variable_Importance_cforest_ggplo2.png?raw=true "Variable importance ML model")

#### We have also calculate Feature Importance for each Variable vs Dropout Loss.

- RandomForest model:
![Variable importance boxplot ML model](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/blob/main/Plots/Variable_Importance_boxplot_rforest_ggplo2.png?raw=true "Variable importance boxplot ML model")

- Conditional RandomForest model:
![Variable importance boxplot ML model](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/blob/main/Plots/Variable_Importance_boxplot_crforest_ggplo2.png?raw=true "Variable importance boxplot ML model")

#### We have also calculate Feature Importance by each year used in the model.

- RandomForest model:
![Variable importance boxplot ML model by year](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/blob/main/Plots/Variable_Importance_boxplot_rforest_by_year_ggplo2.png?raw=true "Variable importance boxplot ML model by year")

- Conditional RandomForest model:
![Variable importance boxplot ML model by year](https://github.com/diegoalarc/Innovation_laboratory_EAGLE/blob/main/Plots/Variable_Importance_boxplot_crforest_by_year_ggplo2.png?raw=true "Variable importance boxplot ML model by year")

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

---

## Results: Prediction Yields

Finally the predictions obtained for each model are as follows.

### RandomForest model:

| `Variety`	| `Field`	| `Year`	| `Area_He`	| `Kg_He` |
| :-------: | :---------: | :---: | :---: | :--------------: |
| THOMPSON	| THOMPSON 00	| 2021	| 9.6	| 41357.7977096304 |
| THOMPSON	| THOMPSON 07	| 2021	| 3.4	| 36689.4504366268 |
| THOMPSON	| THOMPSON 08	| 2021	| 3.4	| 35831.9454180071 |
| THOMPSON	| THOMPSON 96	| 2021	| 9.5	| 35798.0735234639 |
| CRIMSON	| CRIMSON  99-00	| 2021	| 12.2	| 45890.9988594843 |
| CRIMSON	| CRIMSON 04	| 2021	| 2.4	| 47082.2595503199 |
| ARRA	| ARRA 15 CR	| 2021	| 2.9	| 44344.6312243955 |
| TIMCO	| TIMCO 14 CR	| 2021	| 4.1	| 43648.8121403448 |
| INIAGRAPEONE	| INIAGRAPE ONE 14	| 2021	| 1.1	| 42528.3456174134 |
| INIAGRAPEONE	| INIAGRAPE ONE 15	| 2021	| 3	| 42693.726853647 |
| SABLE	| SABLE 14	| 2021	| 4.2	| 36598.9735786702 |
| SABLE	| SABLE 16	| 2021	| 1.9	| 40690.0046396989 |

### Conditional RandomForest model:

| `Variety`	| `Field`	| `Year`	| `Area_He`	| `Kg_He` |
| :-------: | :---------: | :---: | :---: | :--------------: |
| THOMPSON	| THOMPSON 00	| 2021	| 9.6	| 40630.3478980829 |
| THOMPSON	| THOMPSON 07	| 2021	| 3.4	| 35272.6115914895 |
| THOMPSON	| THOMPSON 08	| 2021	| 3.4	| 35009.3554826691 |
| THOMPSON	| THOMPSON 96	| 2021	| 9.5	| 34895.3375997666 |
| CRIMSON	| CRIMSON  99-00	| 2021	| 12.2	| 44551.796429587 |
| CRIMSON	| CRIMSON 04	| 2021	| 2.4	| 45011.1310551689 |
| ARRA	| ARRA 15 CR	| 2021	| 2.9	| 42912.3193929731 |
| TIMCO	| TIMCO 14 CR	| 2021	| 4.1	| 42191.3154600087 |
| INIAGRAPEONE	| INIAGRAPE ONE 14	| 2021	| 1.1	| 41554.7408015287 |
| INIAGRAPEONE	| INIAGRAPE ONE 15	| 2021	| 3	| 41840.732953901 |
| SABLE	| SABLE 14	| 2021	| 4.2	| 35848.9520325514 |
| SABLE	| SABLE 16	| 2021	| 1.9	| 43400.768352273 |

---

### Bibliography

- Xia, Rong, "Comparison of Random Forests and Cforest: Variable Importance Measures and Prediction Accuracies" (2009). All Graduate Plan B and other Reports. 1255. https://digitalcommons.usu.edu/gradreports/1255

- Crane-Droesch, Andrew. (2018). Machine learning methods for crop yield prediction and climate change impact assessment in agriculture. Environmental Research Letters. 13. 10.1088/1748-9326/aae159. 

- Braga, Dieinison & Coelho da Silva, Ticiana & Rocha, Atslands & Coutinho do Rêgo, Luís Gustavo & Pires Magalhaes, Regis & Guerra, Paulo. (2020). Time Series Forecasting to Support Irrigation Management. Journal on Data Semantics. 10. 66-80. 

- Zhao, Yan & Potgieter, Andries & Zhang, Miao & Wu, Bingfang & Hammer, G.. (2020). remote sensing Predicting Wheat Yield at the Field Scale by Combining High-Resolution Sentinel-2 Satellite Imagery and Crop Modelling. Remote Sensing. 12. 10.3390/rs12061024. 

- Pantazi, X.E. & Moshou, Dimitrios & Alexandridis, Thomas & Whetton, Rebecca & Mouazen, Abdul. (2016). Wheat yield prediction using machine learning and advanced sensing techniques. Computers and Electronics in Agriculture. 121. 57-65. 10.1016/j.compag.2015.11.018. 

- Al-Gaadi, K.A. & Hassaballa, Abdalhaleem A. & Tola, Elkamil & Kayad, Ahmed & Madugundu, Rangaswamy & Alblewi, Bander & Assiri, Fahad. (2016). Prediction of Potato Crop Yield Using Precision Agriculture Techniques. PloS one. 11. e0162219. 10.1371/journal.pone.0162219. 

- Kuhn, M. (2008). Building Predictive Models in R Using the caret Package. Journal of Statistical Software, 28(5), 1 - 26. doi:http://dx.doi.org/10.18637/jss.v028.i05

- Wickham H (2016). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York. ISBN 978-3-319-24277-4, https://ggplot2.tidyverse.org.

- Wickham et al., (2019). Welcome to the tidyverse. Journal of Open Source Software, 4(43), 1686, https://doi.org/10.21105/joss.01686
---

#### *GNU General Public License v3.0 - Copyright (C)*

This script was made for testing purposes and may be used and modified in the future by those who see fit.