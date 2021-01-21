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

# Import dataset as a list of dataframes
df_rf <- list()
df_crf <- list()

for (i in 1:4) {
  # list of dataframes
  #Field_Carmen[[i]] <- list(read.csv(paste0('./Original_data/summary_',2016+i,'.csv')))
  Field_Carmen <- read.csv(paste0('./Original_data/summary_',2016+i,'.csv'))

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
  train_fraction <- 0.6
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
  
  varimps_rf[,5] <- data.frame(Year = as.character(2015 + i))
  
  varimps_crf[,5] <- data.frame(Year = as.character(2015 + i))
  
  df_rf[[i]] <- varimps_rf
  
  df_crf[[i]] <- varimps_crf
  
  print(paste0("Ready Iteration: ", i))

}

# Execute a Function rbind
big_data_rf <- do.call(rbind, df_rf)
big_data_crf <- do.call(rbind, df_crf)

# Save dataframe as .CSV
write.csv(big_data_rf,'./Original_data/VarIpm_rf_by_year.csv',row.names = F, quote = F)
write.csv(big_data_crf,'./Original_data/VarIpm_crf_by_year.csv',row.names = F, quote = F)

################################################################################

big_data_rf <- read.csv('./Original_data/VarIpm_rf_by_year.csv')
big_data_crf <- read.csv('./Original_data/VarIpm_crf_by_year.csv')

# Save boxplot as .png
png(file = './Plots/Variable_Importance_boxplot_rforest_by_year_ggplo2.png', units = "px",
    width = 1400, height = 800)

p_rf2 <- big_data_rf %>% 
  ggplot(aes(x = dropout_loss, y = variable, fill=label)) +
  geom_boxplot(aes(x = dropout_loss, y = variable)) +
  coord_flip() +
  labs(title = "Conditional Random Forest - Variable Importance boxplot by year") +
  ylab('Variables') + xlab('Dropout Loss') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, colour = "black")) +
  theme(axis.text.y = element_text(hjust = 1, colour = "black")) +
  facet_wrap(~Year, scales = "free_y")

plot(p_rf2)

dev.off()

# Save boxplot as .png
png(file = './Plots/Variable_Importance_boxplot_crforest_by_year_ggplo2.png', units = "px",
    width = 1400, height = 800)

p_crf2 <- big_data_crf %>% 
  ggplot(aes(x = dropout_loss, y = variable, fill=label)) +
  geom_boxplot(aes(x = dropout_loss, y = variable)) +
  coord_flip() +
  labs(title = "Conditional Random Forest - Variable Importance boxplot by year") +
  ylab('Variables') + xlab('Dropout Loss') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, colour = "black")) +
  theme(axis.text.y = element_text(hjust = 1, colour = "black")) +
  facet_wrap(~Year, scales = "free_y")

plot(p_crf2)

dev.off()

################################################################################
# 
# big_data_rf <- read.csv('./Original_data/VarIpm_rf_by_year.csv')
# big_data_crf <- read.csv('./Original_data/VarIpm_crf_by_year.csv')
# 
# # Save boxplot as .png
# png(file = './Plots/Variable_Importance_boxplot_rforest_by_year_ggplo2.png', units = "px",
#     width = 1400, height = 800)
# 
# p_rf2 <- big_data_rf %>% 
#   ggplot(aes(x = dropout_loss, y = variable, fill=label)) +
#   geom_boxplot(aes(x = dropout_loss, y = variable)) +
#   coord_flip() +
#   labs(title = "Conditional Random Forest - Variable Importance boxplot by year") +
#   ylab('Variables') + xlab('Dropout Loss') + 
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, colour = "black")) +
#   theme(axis.text.y = element_text(hjust = 1, colour = "black")) +
#   facet_wrap(~Year, scales = "free_y")
# 
# plot(p_rf2)
# 
# dev.off()
# 
# # Save boxplot as .png
# png(file = './Plots/Variable_Importance_boxplot_crforest_by_year_ggplo2.png', units = "px",
#     width = 1400, height = 800)
# 
# p_crf2 <- big_data_crf %>% 
#   ggplot(aes(x = dropout_loss, y = variable, fill=label)) +
#   geom_boxplot(aes(x = dropout_loss, y = variable)) +
#   coord_flip() +
#   labs(title = "Conditional Random Forest - Variable Importance boxplot by year") +
#   ylab('Variables') + xlab('Dropout Loss') + 
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, colour = "black")) +
#   theme(axis.text.y = element_text(hjust = 1, colour = "black")) +
#   facet_wrap(~Year, scales = "free_y")
# 
# plot(p_crf2)
# 
# dev.off()