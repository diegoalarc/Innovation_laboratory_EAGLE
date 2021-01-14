# Load packages
# https://www.machinelearningplus.com/machine-learning/caret-package/
# https://www.youtube.com/watch?v=fSytzGwwBVw
library(tidyverse)
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
train.control <- trainControl(method = "none")

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
predictions <- model %>% predict(test.data, na.action=na.exclude)

predictions

data.frame( R2 = R2(predictions, test.data$Kg_He),
            RMSE = RMSE(predictions, test.data$Kg_He),
            MAE = MAE(predictions, test.data$Kg_He))

RMSE(predictions, test.data$Kg_He)/mean(test.data$Kg_He)

# variable importance
gbmImp <- varImp(model, scale = T)
gbmImp

# Save boxplot as .png
png(file = './Plots/Variable_Importance.png', units = "px",
    width = 600, height = 700)
plot(gbmImp, top = 19, main = "Random Forest - Variable Importance plot")
dev.off()