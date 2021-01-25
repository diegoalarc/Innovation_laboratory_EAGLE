# Load packages
library(randomForest)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(caret)
library(DALEX)
library(party)
library(vivo)

# Set the folder location
#setwd('~/Dropbox/RS Project/RSSTARTUP_repo/Product_Development/Yield_forcasting_R/Innovation_laboratory_EAGLE')
setwd('/home/diego/GITHUP_REPO/Innovation_laboratory_EAGLE')

# Settings
number <- 4
n_repeats <- 100
train_fraction <- 0.6
tunegrid <- expand.grid(.mtry=c(1:20))
ntree <- 1200
metric <- "RMSE"

# Define training control
set.seed(123)

train.control <- trainControl(method="repeatedcv", allowParallel = TRUE,
                              number = number, repeats = n_repeats,
                              savePredictions = T, search="grid")

################################################################################

# Import dataset
# The dataset used is summary.csv
Field_Carmen <- read.csv('./Original_data/summary.csv')

# Clean columns 'id' & 'Week' in the data frame
Field_Carmen <- Field_Carmen[ , -which(names(Field_Carmen) %in% c("id","Week"))]

# The dummyVars will transform all characters and factors columns
dmy <- dummyVars(" ~ .",data = Field_Carmen, fullRank = T)
Field_Carmen <- data.frame(predict(dmy, newdata = Field_Carmen))

# See top 6 rows and 10 columns
#head(Field_Carmen[, 1:10])

# Split the data into training and test set
set.seed(123)

training.samples <- Field_Carmen$Kg_He %>%
  createDataPartition(p = train_fraction, list = FALSE)

train.data <- Field_Carmen[training.samples, ]
test.data <- Field_Carmen[-training.samples, ]

################################################################################

# Train the model
set.seed(123)

model_rf <- train(Kg_He ~., data = Field_Carmen, 
                  method = "rf",
                  ntree = ntree,
                  metric=metric,
                  tuneGrid = tunegrid,
                  #                  tuneGrid = data.frame(mtry = mtry),
                  trControl = train.control)

# Summarize the results
print(model_rf)

# Make predictions and compute the R2, RMSE and MAE
predictions_rf <- model_rf %>% predict(test.data, na.action = na.omit)

predictions_rf
################################################################################

# Define training control
set.seed(123)

# Train the model
model_crf <- train(Kg_He ~., data = Field_Carmen, 
                   method = "cforest",
                   metric=metric,
                   tuneGrid = tunegrid,
                   #                   tuneGrid = data.frame(mtry = mtry),
                   trControl = train.control)

# Summarize the results
print(model_crf)

# Make predictions and compute the R2, RMSE and MAE
predictions_crf <- model_crf %>% predict(test.data, na.action = na.omit)

predictions_crf

# Import dataset as a list of dataframes
df_rf <- list()
df_crf <- list()

for (i in 1:4) {

  # Dataframe by year
  Field_Carmen_by_year <- Field_Carmen %>% filter(Year == 2016+i)

  # https://cran.r-project.org/web/packages/vivo/vignettes/vignette_apartments_local.html
  # use explain from DALEX.
  explainer_rf2 <- explain(model_rf, data = Field_Carmen_by_year, 
                          y = Field_Carmen_by_year$Kg_He)
  
  
  # you can find out how important a variable is based on a dropout loss, 
  # that is how much loss is incurred by removing a variable from the model.
  varimps_rf2 <- variable_importance(explainer_rf2, type='raw')
  
  varimps_rf2 <- varimps_rf2[!varimps_rf2$variable == "id", ]
  varimps_rf2 <- varimps_rf2[!varimps_rf2$variable == "Kg_He", ]
  varimps_rf2 <- varimps_rf2[!varimps_rf2$variable == "Year", ]
  varimps_rf2 <- varimps_rf2[!varimps_rf2$variable == "_baseline_", ]
  varimps_rf2 <- varimps_rf2[!varimps_rf2$variable == "_full_model_", ]
  
  varimps_rf2$label[grepl("_mean", varimps_rf2$variable)] <- 'Remote Sensing'
  varimps_rf2$label[grepl("EVI", varimps_rf2$variable)] <- 'Remote Sensing'
  varimps_rf2$label[grepl("GNDVI", varimps_rf2$variable)] <- 'Remote Sensing'
  varimps_rf2$label[grepl("NDVI", varimps_rf2$variable)] <- 'Remote Sensing'
  
  varimps_rf2$label[grepl("train.formula", varimps_rf2$label)] <- 'Ground truth data'

  ################################################################################

  # https://cran.r-project.org/web/packages/vivo/vignettes/vignette_apartments_local.html
  # use explain from DALEX.
  explainer_crf2 <- explain(model_crf, data = Field_Carmen_by_year, 
                          y = Field_Carmen_by_year$Kg_He)
  
  
  # you can find out how important a variable is based on a dropout loss, 
  # that is how much loss is incurred by removing a variable from the model.
  varimps_crf2 <- variable_importance(explainer_crf2, type='raw')
  
  varimps_crf2 <- varimps_crf2[!varimps_crf2$variable == "id", ]
  varimps_crf2 <- varimps_crf2[!varimps_crf2$variable == "Kg_He", ]
  varimps_crf2 <- varimps_crf2[!varimps_crf2$variable == "Year", ]
  varimps_crf2 <- varimps_crf2[!varimps_crf2$variable == "_baseline_", ]
  varimps_crf2 <- varimps_crf2[!varimps_crf2$variable == "_full_model_", ]
  
  varimps_crf2$label[grepl("_mean", varimps_crf2$variable)] <- 'Remote Sensing'
  varimps_crf2$label[grepl("EVI", varimps_crf2$variable)] <- 'Remote Sensing'
  varimps_crf2$label[grepl("GNDVI", varimps_crf2$variable)] <- 'Remote Sensing'
  varimps_crf2$label[grepl("NDVI", varimps_crf2$variable)] <- 'Remote Sensing'
  
  varimps_crf2$label[grepl("train.formula", varimps_crf2$label)] <- 'Ground truth data'
  
  ################################################################################
  
  varimps_rf2[,5] <- data.frame(Year = as.character(2015 + i))
  
  varimps_crf2[,5] <- data.frame(Year = as.character(2015 + i))
  
  df_rf[[i]] <- varimps_rf2
  
  df_crf[[i]] <- varimps_crf2
  
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
  labs(title = "Random Forest - Variable Importance boxplot by year") +
  ylab('Variables') + xlab('Dropout Loss') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, colour = "black")) +
  theme(axis.text.y = element_text(hjust = 1, colour = "black")) +
  facet_wrap(~Year, scales = "fixed")

plot(p_rf2)

dev.off()

# Save boxplot as .png
png(file = './Plots/Variable_Importance_boxplot_crforest_by_year_ggplo2.png', units = "px",
    width = 1400, height = 800)

p_crf2 <- big_data_crf %>% 
  ggplot(aes(x = dropout_loss, y = variable, fill=label)) +
  geom_boxplot(aes(x = dropout_loss, y = variable)) +
  coord_flip() +
  labs(title = "Conditional Inference Forest - Variable Importance boxplot by year") +
  ylab('Variables') + xlab('Dropout Loss') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, colour = "black")) +
  theme(axis.text.y = element_text(hjust = 1, colour = "black")) +
  facet_wrap(~Year, scales = "fixed")

plot(p_crf2)

dev.off()