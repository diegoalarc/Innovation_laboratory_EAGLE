# Load packages
# https://www.machinelearningplus.com/machine-learning/caret-package/
# https://www.youtube.com/watch?v=fSytzGwwBVw
library(randomForest)
library(tidyverse)
library(mlbench)
library(ggplot2)
library(caret)

# Set the folder location
#setwd('~/Dropbox/RS Project/RSSTARTUP_repo/Product_Development/Yield_forcasting_R/Innovation_laboratory_EAGLE')
setwd('/home/diego/GITHUP_REPO/Innovation_laboratory_EAGLE')

# Import dataset
# The dataset used is summary_fn.csv, which was clean but the original one
# is summary.csv
Field_Carmen <- read.csv('./Original_data/summary.csv')

# Clean data frame of data without importance
Field_Carmen[,5] <- NULL

# The dummyVars will transform all characters and factors columns
# The general rule for creating dummy variables is to have one less variable 
# than the number of categories present to avoid perfect collinearity (dummy variable trap).
# The id column is remove by applying -id inside of dummyVars.
dmy <- dummyVars(" ~ .",-id, data = Field_Carmen, fullRank=T)
Field_Carmen <- data.frame(predict(dmy, newdata = Field_Carmen))
print(Field_Carmen)

# Structure of the dataframe
str(Field_Carmen)

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
dataset <- Field_Carmen
x <- cbind(dataset[,2:5],dataset[,7:29])
y <- dataset[,6]

set.seed(123)
bestmtry <- tuneRF(x, y, stepFactor=0.5, improve=0.05, ntree=1000)
print(bestmtry)

mtry <- max(bestmtry[,1])

################################################################################

# RandomForest
# https://topepo.github.io/caret/train-models-by-tag.html#random-forest
# https://stats.stackexchange.com/questions/348245/do-we-have-to-tune-the-number-of-trees-in-a-random-forest
# https://stats.stackexchange.com/questions/50210/caret-and-randomforest-number-of-trees

# Define training control
set.seed(123)

# http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/
# Leave one out cross validation - LOOCV
train.control <- trainControl(method = "LOOCV")

# cross-validation
#train.control <- trainControl(method = "cv", number = 5)

# Train the model
model <- train(Kg_He ~., data = Field_Carmen, 
               method = "rf",
               ntree = 1000,
               trControl = train.control,
               tuneGrid = data.frame(mtry = mtry))

# Summarize the results
print(model)

# Make predictions and compute the R2, RMSE and MAE
predictions <- model %>% predict(test.data, na.action=na.omit)

predictions

data.frame( R2 = R2(predictions, test.data$Kg_He),
            # RMSE will be expressed in kilograms
            RMSE = RMSE(predictions, test.data$Kg_He),
            MAE = MAE(predictions, test.data$Kg_He))

# The RMSE and the MAE are measured in the same scale as the outcome variable. 
# Dividing the RMSE by the average value of the outcome variable will give you 
# the prediction error rate, which should be as small as possible:
RMSE(predictions, test.data$Kg_He)/mean(test.data$Kg_He)

# See the confusion matrix of the model in the test set
#confusionMatrix(predictions, test.data$Kg_He)

# variable importance
gbmImp <- varImp(model, scale = F)
gbmImp

# Save boxplot as .png
png(file = './Plots/Variable_Importance_rforest.png', units = "px",
    width = 1200, height = 700)

plot(gbmImp, top = 29, main = "Random Forest - Variable Importance plot")

dev.off()

# Save boxplot as .png
png(file = './Plots/Variable_Importance_rforest_ggplo2.png', units = "px",
    width = 1200, height = 700)

nrow(varImp(model)$importance) #34 variables extracted

p <- varImp(model)$importance %>% 
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
train.control <- trainControl(method = "LOOCV")

# cross-validation
#train.control <- trainControl(method = "cv", number = 5)

# Train the model
model <- train(Kg_He ~., data = Field_Carmen, 
               method = "cforest",
               trControl = train.control,
               tuneGrid = data.frame(mtry = mtry))

# Summarize the results
print(model)

# Make predictions and compute the R2, RMSE and MAE
predictions <- model %>% predict(test.data, na.action=na.omit)

predictions

data.frame( R2 = R2(predictions, test.data$Kg_He),
            # RMSE will be expressed in kilograms
            RMSE = RMSE(predictions, test.data$Kg_He),
            MAE = MAE(predictions, test.data$Kg_He))

# The RMSE and the MAE are measured in the same scale as the outcome variable. 
# Dividing the RMSE by the average value of the outcome variable will give you 
# the prediction error rate, which should be as small as possible:
RMSE(predictions, test.data$Kg_He)/mean(test.data$Kg_He)

# See the confusion matrix of the model in the test set
#confusionMatrix(predictions, test.data$Kg_He)

# variable importance
gbmImp <- varImp(model, scale = F)
gbmImp

# Save boxplot as .png
png(file = './Plots/Variable_Importance_cforest.png', units = "px",
    width = 1200, height = 700)

plot(gbmImp, top = 29, main = "Conditional Random Forests - Variable Importance plot")

dev.off()

# Save boxplot as .png
png(file = './Plots/Variable_Importance_cforest_ggplo2.png', units = "px",
    width = 1200, height = 700)

nrow(varImp(model)$importance) #34 variables extracted

p <- varImp(model)$importance %>% 
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