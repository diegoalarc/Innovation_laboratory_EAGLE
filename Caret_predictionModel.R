# Load packages
# https://www.machinelearningplus.com/machine-learning/caret-package/
# https://www.youtube.com/watch?v=fSytzGwwBVw
library(randomForest)
library(tidyverse)
library(mlbench)
library(ggplot2)
library(ggpubr)
library(caret)

# Set the folder location
#setwd('~/Dropbox/RS Project/RSSTARTUP_repo/Product_Development/Yield_forcasting_R/Innovation_laboratory_EAGLE')
setwd('/home/diego/GITHUP_REPO/Innovation_laboratory_EAGLE')

# Import dataset
# The dataset used is summary_fn.csv, which was clean but the original one
# is summary.csv
Field_Carmen <- read.csv('./Original_data/summary.csv')

# Clean column 'Week' in the data frame
Field_Carmen[,5] <- NULL

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

# Split the data into training and test set
set.seed(123)
training.samples <- Field_Carmen$Kg_He %>%
  createDataPartition(p = 0.7, list = FALSE)

train.data  <- Field_Carmen[training.samples, ]
test.data <- Field_Carmen[-training.samples, ]

# Algorithm Tune (tuneRF)
# https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
# https://machinelearningmastery.com/machine-learning-evaluation-metrics-in-r/
dataset <- Field_Carmen
x <- cbind(dataset[,2:19],dataset[,21:43])
y <- dataset$Kg_He

#ntree <- 1500

#set.seed(123)
#bestmtry <- tuneRF(x, y, stepFactor=0.5, improve=1e-5, ntree=ntree)
#print(bestmtry)

#bestmtry[-1]

#mtry <- min(bestmtry[,1])
#mtry

################################################################################

# RandomForest
# https://topepo.github.io/caret/train-models-by-tag.html#random-forest
# https://stats.stackexchange.com/questions/348245/do-we-have-to-tune-the-number-of-trees-in-a-random-forest
# https://stats.stackexchange.com/questions/50210/caret-and-randomforest-number-of-trees

# Define training control
set.seed(123)

# http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/
# https://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/
# Leave one out cross validation - LOOCV
#train.control <- trainControl(method = "LOOCV", savePredictions = T)

#train.control

# Random Search
#train.control <- trainControl(method = "LOOCV", savePredictions = T,
#                              search="random")
#train.control

#tunegrid <- expand.grid(.mtry=mtry)

# Grid Search
train.control <- trainControl(method="cv", number=10,
                              savePredictions = T, search="grid")

#train.control

tunegrid <- expand.grid(.mtry=c(1:10))
ntree <- 1200
metric <- "RMSE"

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
    width = 1200, height = 700)

plot(model_rf)

dev.off()

# Make predictions and compute the R2, RMSE and MAE
predictions_rf <- model_rf %>% predict(test.data, na.action=na.omit)

predictions_rf

data.frame( R2 = R2(predictions_rf, test.data$Kg_He),
            # RMSE will be expressed in kilograms
            RMSE = RMSE(predictions_rf, test.data$Kg_He),
            MAE = MAE(predictions_rf, test.data$Kg_He))

# The RMSE and the MAE are measured in the same scale as the outcome variable. 
# Dividing the RMSE by the average value of the outcome variable will give you 
# the prediction error rate, which should be as small as possible:
RMSE(predictions_rf, test.data$Kg_He)/mean(test.data$Kg_He)

# See the confusion matrix of the model in the test set
#confusionMatrix(predictions_rf, test.data$Kg_He)

# variable importance
gbmImp_rf <- varImp(model_rf, scale = F)
gbmImp_rf

# Save boxplot as .png
png(file = './Plots/Variable_Importance_rforest.png', units = "px",
    width = 1200, height = 700)

plot(gbmImp_rf, top = 29, main = "Random Forest - Variable Importance plot")

dev.off()

# Save boxplot as .png
png(file = './Plots/Variable_Importance_rforest_ggplo2.png', units = "px",
    width = 1200, height = 700)

nrow(varImp(model_rf)$importance) #34 variables extracted

p <- varImp(model_rf)$importance %>% 
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

plot(p)

dev.off()

################################################################################
# Conditional Random Forests
# Define training control
set.seed(123)

# http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/
# Leave one out cross validation - LOOCV
# Leave one out cross validation - LOOCV
#train.control <- trainControl(method = "LOOCV", savePredictions = T)

#train.control

# Random Search
#train.control <- trainControl(method = "LOOCV", savePredictions = T,
#                              search="random")

#train.control

#tunegrid <- expand.grid(.mtry=mtry)

# Grid Search
train.control <- trainControl(method="cv", number=10,
                              savePredictions = T, search="grid")

#train.control

tunegrid <- expand.grid(.mtry=c(1:30))

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
    width = 1200, height = 700)

plot(model_crf)

dev.off()

# Make predictions and compute the R2, RMSE and MAE
predictions_crf <- model_crf %>% predict(test.data, na.action=na.omit)

predictions_crf

data.frame( R2 = R2(predictions_crf, test.data$Kg_He),
            # RMSE will be expressed in kilograms
            RMSE = RMSE(predictions_crf, test.data$Kg_He),
            MAE = MAE(predictions_crf, test.data$Kg_He))

# The RMSE and the MAE are measured in the same scale as the outcome variable. 
# Dividing the RMSE by the average value of the outcome variable will give you 
# the prediction error rate, which should be as small as possible:
RMSE(predictions_crf, test.data$Kg_He)/mean(test.data$Kg_He)

# See the confusion matrix of the model in the test set
#confusionMatrix(predictions_crf, test.data$Kg_He)

# variable importance
gbmImp_crf <- varImp(model_crf, scale = F)
gbmImp_crf

# Save boxplot as .png
png(file = './Plots/Variable_Importance_cforest.png', units = "px",
    width = 1200, height = 700)

plot(gbmImp_crf, top = 29, main = "Conditional Random Forests - Variable Importance plot")

dev.off()

# Save boxplot as .png
png(file = './Plots/Variable_Importance_cforest_ggplo2.png', units = "px",
    width = 1200, height = 700)

nrow(varImp(model_crf)$importance) #34 variables extracted

p <- varImp(model_crf)$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall) %>%
  mutate(rowname = forcats::fct_inorder(rowname)) %>%
  ggplot() +
  geom_col(aes(x = rowname, y = Overall)) +
  coord_flip() +
  labs(title = "Conditional Random Forests - Variable Importance plot") +
  geom_hline(yintercept = 50, color = "blue", size=0.5) +
  ylab('Overall importance percentage') + xlab('Variables') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) +
  theme(axis.text.y = element_text(hjust = 1, colour = "black"))

plot(p)

dev.off()

# Resampling train values for each model
resamps <- resamples(list(model_rf, model_crf), c("Random Forest", "Conditional Random Forest"))

# Save boxplot as .png
png(file = './Plots/RF_vs_cRF_Boxplot_Rsquared.png', units = "px",
    width = 1200, height = 700)

# Boxplot of Rsquared for each model
bwplot(resamps, col=c('powderblue', 'mistyrose'), metric = "Rsquared")

dev.off()

df = data.frame(MAE=c(model_rf$resample$Rsquared, model_crf$resample$Rsquared),
                model=rep(c("nnet","svm"),each=length(svm2$resample$Rsquared)))

ggboxplot(df, x = "model",y= "Rsquared",col="model",palette = c("#00AFBB", "#E7B800")) + 
  stat_compare_means()