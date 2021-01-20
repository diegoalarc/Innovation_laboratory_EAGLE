# Load packages
library(randomForestSRC)
library(randomForest)
library(tidyverse)
library(mlbench)
library(ggplot2)
library(ggpubr)
library(caret)
library(DALEX)
library(party)

# Set the folder location
#setwd('~/Dropbox/RS Project/RSSTARTUP_repo/Product_Development/Yield_forcasting_R/Innovation_laboratory_EAGLE')
setwd('/home/diego/GITHUP_REPO/Innovation_laboratory_EAGLE')

# Import dataset
Field_Carmen <- read.csv('./Original_data/summary.csv')

# Clean column 'Week' in the data frame
Field_Carmen[,5] <- NULL

# The dummyVars will transform all characters and factors columns
dmy <- dummyVars(" ~ .",data = Field_Carmen, fullRank=T)
Field_Carmen <- data.frame(predict(dmy, newdata = Field_Carmen))

# See top 6 rows and 10 columns
head(Field_Carmen[, 1:10])

# Settings
number <- 4
n_repeats <- 100
train_fraction <- 0.7
tunegrid <- expand.grid(.mtry=c(1:20))
ntree <- 1200
metric <- "RMSE"

for (i in 123:223) {

  iteration_n <- i-122
  print(iteration_n)
  
  # Split the data into training and test set
  set.seed(i)
    
  training.samples <- Field_Carmen$Kg_He %>%
    createDataPartition(p = train_fraction, list = FALSE)
  
  train.data  <- Field_Carmen[training.samples, ]
  test.data <- Field_Carmen[-training.samples, ]
  
  ################################################################################
  # Define training control
  set.seed(i)
  
  # Grid Search
  train.control <- trainControl(method="repeatedcv", 
                                number = number, repeats = n_repeats,
                                savePredictions = T, search="grid")
  
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
  
  # Make predictions and compute the R2, RMSE and MAE
  predictions_rf <- model_rf %>% predict(test.data, na.action=na.omit)
  
  predictions_rf
  
  postResample(pred = predictions_rf, obs = test.data$Kg_He)
  
  # # variable importance
  # gbmImp_rf <- varImp(model_rf, useModel = T, scale = F)
  # gbmImp_rf
  
  # Calculate Feature Importance Explanations As Loss From Feature Dropout
  explained_rf <- explain(model_rf, data=test.data, y=test.data$Kg_He)
  
  # you can find out how important a variable is based on a dropout loss, 
  # that is how much loss is incurred by removing a variable from the model.
  varimps_rf <- variable_importance(explained_rf, type='raw')
  
  varimps_rf <- varimps_rf[!varimps_rf$variable == "id", ]
  varimps_rf <- varimps_rf[!varimps_rf$variable == "_baseline_", ]
  varimps_rf <- varimps_rf[!varimps_rf$variable == "_full_model_", ]
  
  varimps_rf$label[grepl("_mean", varimps_rf$variable)] <- 'Remote Sensing'
  varimps_rf$label[grepl("EVI", varimps_rf$variable)] <- 'Remote Sensing'
  varimps_rf$label[grepl("GNDVI", varimps_rf$variable)] <- 'Remote Sensing'
  varimps_rf$label[grepl("NDVI", varimps_rf$variable)] <- 'Remote Sensing'
  
  varimps_rf$label[grepl("train.formula", varimps_rf$label)] <- 'Other'
  
  ################################################################################
  
  # Define training control
  set.seed(i)
  
  # Train the model
  model_crf <- train(Kg_He ~., data = Field_Carmen, 
                     method = "cforest",
                     metric=metric,
                     tuneGrid = tunegrid,
                     #               tuneGrid = data.frame(mtry = mtry),
                     trControl = train.control)
  
  # Summarize the results
  print(model_crf)
  
  # Make predictions and compute the R2, RMSE and MAE
  predictions_crf <- model_crf %>% predict(test.data, na.action=na.omit)
  
  predictions_crf
  
  postResample(pred = predictions_crf, obs = test.data$Kg_He)
  
  # # variable importance
  # gbmImp_crf <- varImp(model_crf, scale = F)
  # gbmImp_crf
  
  # Calculate Feature Importance Explanations As Loss From Feature Dropout
  explained_crf <- explain(model_crf, data=test.data, y=test.data$Kg_He)
  
  # you can find out how important a variable is based on a dropout loss, 
  # that is how much loss is incurred by removing a variable from the model.
  varimps_crf <- variable_importance(explained_crf, type='raw')
  
  varimps_crf <- varimps_crf[!varimps_crf$variable == "id", ]
  varimps_crf <- varimps_crf[!varimps_crf$variable == "_baseline_", ]
  varimps_crf <- varimps_crf[!varimps_crf$variable == "_full_model_", ]
  
  varimps_crf$label[grepl("_mean", varimps_crf$variable)] <- 'Remote Sensing'
  varimps_crf$label[grepl("EVI", varimps_crf$variable)] <- 'Remote Sensing'
  varimps_crf$label[grepl("GNDVI", varimps_crf$variable)] <- 'Remote Sensing'
  varimps_crf$label[grepl("NDVI", varimps_crf$variable)] <- 'Remote Sensing'
  
  varimps_crf$label[grepl("train.formula", varimps_crf$label)] <- 'Other'
  
  ################################################################################
  
  df_rf[[iteration_n]] <- varimps_rf
  
  df_crf[[iteration_n]] <- varimps_crf
  
  print(paste0("Ready Iteration: ", iteration_n))

}

# Execute a Function rbind
big_data_rf <- do.call(rbind, df_rf)
big_data_crf <- do.call(rbind, df_crf)

# Save dataframe as .CSV
write.csv(big_data_rf,'./Original_data/VarIpm_rf.csv',row.names = F, quote = F)
write.csv(big_data_crf,'./Original_data/VarIpm_crf.csv',row.names = F, quote = F)

################################################################################

# plot RandomForest Variable Importance
p_rf <- ggplot(data = big_data_rf, aes(x=Year, y=NDVI, fill=Field)) + 
  geom_boxplot(aes(fill=Field)) + 
  stat_summary(fun=mean, geom="line", aes(group=Field)) +
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red") +
  labs(title = "Time-series NDVI between November and December in the Carmen Rosa Farm for each year", 
       caption = "All data here is produced under the Copernicus Programme, free of charge, without restriction of use.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) +
  theme(axis.text.y = element_text(hjust = 1, colour = "black")) +
  facet_wrap(~Field, ncol = 4)

# Save boxplot as .png
png(file = './Plots/VarIpm_rf.png', units = "px",
    width = 1200, height = 700)

# Plot the graph
plot(p_rf)
dev.off()

# plot Conditional RandomForest Variable Importance
p_crf <- ggplot(data = big_data_crf, aes(x=Year, y=NDVI, fill=Field)) + 
  geom_boxplot(aes(fill=Field)) + 
  stat_summary(fun=mean, geom="line", aes(group=Field)) +
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red") +
  labs(title = "Time-series NDVI between November and December in the Carmen Rosa Farm for each year", 
       caption = "All data here is produced under the Copernicus Programme, free of charge, without restriction of use.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) +
  theme(axis.text.y = element_text(hjust = 1, colour = "black")) +
  facet_wrap(~Field, ncol = 4)

# Save boxplot as .png
png(file = './Plots/VarIpm_crf.png', units = "px",
    width = 1200, height = 700)

# Plot the graph
plot(p_crf)
dev.off()