# Load packages
# https://www.machinelearningplus.com/machine-learning/caret-package/
# https://www.youtube.com/watch?v=fSytzGwwBVw
library(randomForest)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(caret)
library(DALEX)
library(party)

# Set the folder location
#setwd('~/Dropbox/RS Project/RSSTARTUP_repo/Product_Development/Yield_forcasting_R/Innovation_laboratory_EAGLE')
setwd('/home/diego/GITHUP_REPO/Innovation_laboratory_EAGLE')

# Import dataset
# The dataset used is summary.csv
Field_Carmen <- read.csv('./Original_data/summary.csv')

# Clean column 'Week' in the data frame
#Field_Carmen[,5] <- NULL
Field_Carmen <- Field_Carmen[ , -which(names(Field_Carmen) %in% c("Week"))]

# The dummyVars will transform all characters and factors columns
# The general rule for creating dummy variables is to have one less variable 
# than the number of categories present to avoid perfect collinearity (dummy variable trap).
# The id column is remove by applying -id inside of dummyVars.
dmy <- dummyVars(" ~ .",data = Field_Carmen, fullRank=T)
Field_Carmen <- data.frame(predict(dmy, newdata = Field_Carmen))
#print(Field_Carmen)

# Structure of the dataframe
#str(Field_Carmen)

# See top 6 rows and 10 columns
head(Field_Carmen[, 1:10])

# Settings
number <- 4
n_repeats <- 100
train_fraction <- 0.7
tunegrid <- expand.grid(.mtry=c(1:20))
ntree <- 1200
metric <- "RMSE"

# Split the data into training and test set
set.seed(123)
training.samples <- Field_Carmen$Kg_He %>%
  createDataPartition(p = train_fraction, list = FALSE)

train.data  <- Field_Carmen[training.samples, ]
test.data <- Field_Carmen[-training.samples, ]

################################################################################
# Define training control
set.seed(123)

# http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/
# https://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/
# Grid Search
train.control <- trainControl(method="repeatedcv", 
                              number = number, repeats = n_repeats,
                              savePredictions = T, search="grid")

################################################################################
# RandomForest
# https://topepo.github.io/caret/train-models-by-tag.html#random-forest
# https://stats.stackexchange.com/questions/348245/do-we-have-to-tune-the-number-of-trees-in-a-random-forest
# https://stats.stackexchange.com/questions/50210/caret-and-randomforest-number-of-trees

# Importance scores for each iteration
# set.seed(123)
# ctrl <- rfeControl(functions = caretFuncs, 
#                    method = "repeatedcv",
#                    number = number, repeats = n_repeats)
# 
# mod_rf <- rfe(Kg_He ~., data = Field_Carmen,
#                rfeControl = ctrl,
#                ## pass options to train(), 
#                tuneGrid = tunegrid,
#                method = "rf",
#                controls = cforest_unbiased(ntree = ntree))
# 
# mod_fr$variables

################################################################################

# Train the model
model_rf <- train(Kg_He ~., data = Field_Carmen, 
                  method = "rf",
                  ntree = ntree,
                  metric=metric,
                  tuneGrid = tunegrid,
                  #               tuneGrid = data.frame(mtry = mtry),
                  trControl = train.control)

# Summarize the results
print(model_rf)

png(file = './Plots/RMSE_vs_Ramdom_Predictors_rforest.png', units = "px",
    width = 600, height = 400)

plot(model_rf)

dev.off()

# Make predictions and compute the R2, RMSE and MAE
predictions_rf <- model_rf %>% predict(test.data, na.action=na.omit)

predictions_rf

postResample(pred = predictions_rf, obs = test.data$Kg_He)

# The RMSE and the MAE are measured in the same scale as the outcome variable. 
# Dividing the RMSE by the average value of the outcome variable will give you 
# the prediction error rate, which should be as small as possible:
RMSE(predictions_rf, test.data$Kg_He)/mean(test.data$Kg_He)

# See the confusion matrix of the model in the test set
#df1 <- data.frame(Original = test.data$Kg_He, Predicted = predictions_rf)
#confusionMatrix(table(df1$Original, df1$Predicted))

# variable importance
gbmImp_rf <- varImp(model_rf, useModel = T, scale = F)
gbmImp_rf

# Save plot as .png
png(file = './Plots/Variable_Importance_rforest.png', units = "px",
    width = 1200, height = 700)

plot(gbmImp_rf, top = 28, main = "Random Forest - Variable Importance plot")

dev.off()

# Save boxplot as .png
png(file = './Plots/Variable_Importance_rforest_ggplo2.png', units = "px",
    width = 1200, height = 700)

nrow(varImp(model_rf)$importance) #42 variables extracted

p_rf <- varImp(model_rf)$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall) %>%
  mutate(rowname = forcats::fct_inorder(rowname)) %>%
  ggplot() +
  geom_col(aes(x = rowname, y = Overall)) +
  coord_flip() +
  labs(title = "Random Forest - Variable Importance plot") +
  geom_hline(yintercept = 50, color = "blue", size=0.5) +
  ylab('Overall importance percentage') + xlab('Variables') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) +
  theme(axis.text.y = element_text(hjust = 1, colour = "black"))

plot(p_rf)

dev.off()

# https://www.machinelearningplus.com/machine-learning/feature-selection/
# Calculate Feature Importance Explanations As Loss From Feature Dropout
explained_rf <- explain(model_rf, data=Field_Carmen, y=Field_Carmen$Kg_He)

# you can find out how important a variable is based on a dropout loss, 
# that is how much loss is incurred by removing a variable from the model.
varimps_rf <- variable_importance(explained_rf, type='raw')

# Delete some rows that are not part of the study
varimps_rf <- varimps_rf[!varimps_rf$variable == "id", ]
varimps_rf <- varimps_rf[!varimps_rf$variable == "_baseline_", ]
varimps_rf <- varimps_rf[!varimps_rf$variable == "_full_model_", ]

# Rename data in the column label to separate RS and non RS data.
varimps_rf$label[grepl("_mean", varimps_rf$variable)] <- 'Remote Sensing'
varimps_rf$label[grepl("EVI", varimps_rf$variable)] <- 'Remote Sensing'
varimps_rf$label[grepl("GNDVI", varimps_rf$variable)] <- 'Remote Sensing'
varimps_rf$label[grepl("NDVI", varimps_rf$variable)] <- 'Remote Sensing'

varimps_rf$label[grepl("train.formula", varimps_rf$label)] <- 'Ground truth data'

# Save boxplot as .png
png(file = './Plots/Variable_Importance_boxplot_rforest_ggplo2.png', units = "px",
    width = 1400, height = 800)

p_rf2 <- varimps_rf %>% 
  ggplot(aes(x = dropout_loss, y = variable, fill=label)) +
  geom_boxplot(aes(x = dropout_loss, y = variable)) +
  coord_flip() +
  labs(title = "Random Forest - Variable Importance boxplot") +
  ylab('Variables') + xlab('Dropout Loss') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, colour = "black")) +
  theme(axis.text.y = element_text(hjust = 1, colour = "black"))
#  facet_wrap(~variable, ncol = 4)

plot(p_rf2)

dev.off()

################################################################################
# Conditional Inference Forest
# set.seed(123)
# ctrl <- rfeControl(functions = caretFuncs, 
#                    method = "repeatedcv",
#                    number = number, repeats = n_repeats)
# 
# mod_crf <- rfe(Kg_He ~., data = Field_Carmen,
#                rfeControl = ctrl,
#                ## pass options to train(), 
#                tuneGrid = tunegrid,
#                method = "cforest",
#                controls = cforest_unbiased(ntree = ntree))
# 
# mod_crf$variables

################################################################################

# Define training control
set.seed(123)

# Train the model
model_crf <- train(Kg_He ~., data = Field_Carmen, 
                   method = "cforest",
                   metric=metric,
                   tuneGrid = tunegrid,
                   #               tuneGrid = data.frame(mtry = mtry),
                   trControl = train.control)

# Summarize the results
print(model_crf)

png(file = './Plots/RMSE_vs_Ramdom_Predictors_cforest.png', units = "px",
    width = 600, height = 400)

plot(model_crf)

dev.off()

# Make predictions and compute the R2, RMSE and MAE
predictions_crf <- model_crf %>% predict(test.data, na.action=na.omit)

predictions_crf

postResample(pred = predictions_crf, obs = test.data$Kg_He)

# The RMSE and the MAE are measured in the same scale as the outcome variable. 
# Dividing the RMSE by the average value of the outcome variable will give you 
# the prediction error rate, which should be as small as possible:
RMSE(predictions_crf, test.data$Kg_He)/mean(test.data$Kg_He)

# See the confusion matrix of the model in the test set
#df1 <- data.frame(Original = test.data$Kg_He, Predicted = predictions_rf)
#confusionMatrix(table(df1$Original, df1$Predicted))

# variable importance
gbmImp_crf <- varImp(model_crf, scale = F)
gbmImp_crf

# Save boxplot as .png
png(file = './Plots/Variable_Importance_cforest.png', units = "px",
    width = 1200, height = 700)

plot(gbmImp_crf, top = 28, main = "Conditional Random Forests - Variable Importance plot")

dev.off()

# Save boxplot as .png
png(file = './Plots/Variable_Importance_cforest_ggplo2.png', units = "px",
    width = 1200, height = 700)

nrow(varImp(model_crf)$importance) #42 variables extracted

p_crf <- varImp(model_crf)$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall) %>%
  mutate(rowname = forcats::fct_inorder(rowname)) %>%
  ggplot() +
  geom_col(aes(x = rowname, y = Overall)) +
  coord_flip() +
  labs(title = "Conditional Inference Forest - Variable Importance plot") +
  geom_hline(yintercept = 50, color = "blue", size=0.5) +
  ylab('Overall importance percentage') + xlab('Variables') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) +
  theme(axis.text.y = element_text(hjust = 1, colour = "black"))

plot(p_crf)

dev.off()

# https://www.machinelearningplus.com/machine-learning/feature-selection/
# Calculate Feature Importance Explanations As Loss From Feature Dropout
explained_crf <- explain(model_crf, data=Field_Carmen, y=Field_Carmen$Kg_He)

# you can find out how important a variable is based on a dropout loss, 
# that is how much loss is incurred by removing a variable from the model.
varimps_crf <- variable_importance(explained_crf, type='raw')

# Delete some rows that are not part of the study
varimps_crf <- varimps_crf[!varimps_crf$variable == "id", ]
varimps_crf <- varimps_crf[!varimps_crf$variable == "_baseline_", ]
varimps_crf <- varimps_crf[!varimps_crf$variable == "_full_model_", ]

# Rename data in the column label to separate RS and non RS data.
varimps_crf$label[grepl("_mean", varimps_crf$variable)] <- 'Remote Sensing'
varimps_crf$label[grepl("EVI", varimps_crf$variable)] <- 'Remote Sensing'
varimps_crf$label[grepl("GNDVI", varimps_crf$variable)] <- 'Remote Sensing'
varimps_crf$label[grepl("NDVI", varimps_crf$variable)] <- 'Remote Sensing'

varimps_crf$label[grepl("train.formula", varimps_crf$label)] <- 'Ground truth data'

# Save boxplot as .png
png(file = './Plots/Variable_Importance_boxplot_crforest_ggplo2.png', units = "px",
    width = 1400, height = 800)

p_crf2 <- varimps_crf %>% 
  ggplot(aes(x = dropout_loss, y = variable, fill=label)) +
  geom_boxplot(aes(x = dropout_loss, y = variable)) +
  coord_flip() +
  labs(title = "Conditional Inference Forest - Variable Importance boxplot") +
  ylab('Variables') + xlab('Dropout Loss') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, colour = "black")) +
  theme(axis.text.y = element_text(hjust = 1, colour = "black"))
#  facet_wrap(~variable, ncol = 4)

plot(p_crf2)

dev.off()
################################################################################

# Resampling train values for each model
resamps <- resamples(list(model_rf, model_crf), 
                     c("Random Forest", "Conditional Inference Forest"))

# Save boxplot as .png
png(file = './Plots/RF_vs_cRF_Boxplot_Rsquared.png', units = "px",
    width = 600, height = 400)

# Boxplot of Rsquared for each model
bwplot(resamps, metric = "Rsquared",
       main = "RF vs cRF Boxplot Rsquared",
       par.settings = list(box.rectangle = list(fill= rep(c('blue','green'),2)),
                           box.rectangle = list(col= c('blue','green'))),
       horizontal = T)

dev.off()

################################################################################

# Import dataset for predictions
# The dataset used is for_pred.csv
Field_Carmen_pred <- read.csv('./Original_data/for_pred.csv')

# Delete columns usless
Field_Carmen_pred <- Field_Carmen_pred[ , -which(names(Field_Carmen_pred) %in% c("Week","Kg_He"))]

# Convert to non categorical values
dmy_pred <- dummyVars(" ~ .",data = Field_Carmen_pred, fullRank=T)
Field_Carmen_pred <- data.frame(predict(dmy_pred, newdata = Field_Carmen_pred))

# Random Forest
# Make predictions and compute the R2, RMSE and MAE
pred_rf_2021 <- model_rf %>% predict(Field_Carmen_pred, na.action=na.omit)

unname(pred_rf_2021)

# Conditional Random Forest
# Make predictions and compute the R2, RMSE and MAE
pred_crf_2021 <- model_crf %>% predict(Field_Carmen_pred, na.action=na.omit)

pred_crf_2021

################################################################################

# The dataset used is for_pred.csv
Field_Carmen_fn_rf <- read.csv('./Original_data/for_pred.csv')
Field_Carmen_fn_crf <- read.csv('./Original_data/for_pred.csv')

Field_Carmen_fn_rf$Kg_He <- unname(pred_rf_2021)
Field_Carmen_fn_crf$Kg_He <- pred_crf_2021

delete_col <- c('Week','Average_Temp', 'Max_Temp', 'Min_Temp', 'Average_Hum',
                'Average_Evapotranspiration', 'Accumulated_degree_Days', 'B1_mean',
                'B2_mean', 'B3_mean', 'B4_mean', 'B5_mean', 'B6_mean', 'B7_mean',
                'B8_mean', 'B8A_mean', 'B9_mean', 'B10_mean', 'B11_mean', 'B12_mean',
                'EVI', 'GNDVI', 'NDVI')

Field_Carmen_fn_rf[ ,delete_col] <- list(NULL)
Field_Carmen_fn_crf[ ,delete_col] <- list(NULL)

write.csv(Field_Carmen_fn_rf,'./Original_data/final_pred_rf.csv',row.names = F, quote = F)
write.csv(Field_Carmen_fn_crf,'./Original_data/final_pred_crf.csv',row.names = F, quote = F)