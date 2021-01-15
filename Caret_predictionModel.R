# Load packages
# https://www.machinelearningplus.com/machine-learning/caret-package/
# https://www.youtube.com/watch?v=fSytzGwwBVw
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
Field_Carmen <- read.csv('./Original_data/summary_fn.csv')

# Structure of the dataframe
str(Field_Carmen)

# See top 6 rows and 10 columns
head(Field_Carmen[, 1:10])

# Split the data into training and test set
set.seed(123)
training.samples <- Field_Carmen$Kg_He %>%
  createDataPartition(p = 0.75, list = FALSE)

train.data  <- Field_Carmen[training.samples, ]
test.data <- Field_Carmen[-training.samples, ]

################################################################################

# Build the model
#model <- lm(Kg_He ~., data = train.data)

# Observe the model
#model

# It is because, one of your dependent variables has NA for Coefficients given as
# output by the lm(..) function. Such a variable is making no difference to the 
# model, often due to multicollinearity problem ie, that predictor variable is 
# linearly dependent on other predictor variables OR because, that predictor 
# variable is constant for all the records(rows). The best thing to do is to drop
# that variable from the formula in lm(..) function and do the regression again.

# Make predictions and compute the R2, RMSE and MAE
#predictions <- model %>% predict(test.data, na.action=na.exclude)

#data.frame( R2 = R2(predictions, test.data$Kg_He),
#            RMSE = RMSE(predictions, test.data$Kg_He),
#            MAE = MAE(predictions, test.data$Kg_He))

#RMSE(predictions, test.data$Kg_He)/mean(test.data$Kg_He)

################################################################################

# RandomForest
# https://topepo.github.io/caret/train-models-by-tag.html#random-forest
# https://stats.stackexchange.com/questions/348245/do-we-have-to-tune-the-number-of-trees-in-a-random-forest
# https://stats.stackexchange.com/questions/50210/caret-and-randomforest-number-of-trees

# Define training control
set.seed(234)

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
               tuneGrid = data.frame(mtry = 6))

# Summarize the results
summary(model)
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

# Building data for confusion matrix of the model
#pred <- factor(unname(predictions))
#true_value <- factor(test.data$Kg_He)

#my_data1 <- data.frame(data = pred, type = "prediction")
#my_data2 <- data.frame(data = true_value, type = "real")
#my_data3 <- rbind(my_data1,my_data2)

#identical(levels(my_data3[my_data3$type == "prediction",1]) , levels(my_data3[my_data3$type == "real",1]))

# See the confusion matrix of the model in the test set
#confusionMatrix(my_data3[my_data3$type == "prediction",1], my_data3[my_data3$type == "real",1])

# variable importance
gbmImp <- varImp(model, scale = T)
gbmImp

# Save boxplot as .png
png(file = './Plots/Variable_Importance.png', units = "px",
    width = 1200, height = 700)
#plot(gbmImp, top = 19, main = "Random Forest - Variable Importance plot")

nrow(varImp(model)$importance) #34 variables extracted

p <- varImp(model)$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall) %>%
  mutate(rowname = forcats::fct_inorder(rowname )) %>%
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