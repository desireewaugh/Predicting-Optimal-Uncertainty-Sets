# Libraries
library(glmnet)
library(caret)
library(lubridate)
library(caTools)
library(dplyr)

# Read in data
data <- read.csv("Final_Metadata_Reduced.csv")
data$uncertainty_set <- as.factor(data$uncertainty_set)


# Initial values
train_percent <- 0.7
data_shuffled <- data[sample(nrow(data)),]
data_train <- data_shuffled[1:(round(train_percent*nrow(data_shuffled))),]
data_test <- data_shuffled[(round(train_percent*nrow(data_shuffled))+1):nrow(data_shuffled),]


# Model with important variables (Determined by Optimal Classification Tree variable importance)
# This is the best model
log_mod <- multinom(uncertainty_set ~ n+p+mean_cor, data=data_train)
predictions_test <- predict(log_mod, newdata=data_test)
predictions_train <- predict(log_mod, newdata=data_train)

confusion_matrix_test <- table(predictions_test, data_test$uncertainty_set)
accuracy_test <- (confusion_matrix_test[1,1] + confusion_matrix_test[2,2] + confusion_matrix_test[3,3]) / nrow(data_test)

confusion_matrix_train <- table(predictions_train, data_train$uncertainty_set)
accuracy_train <- (confusion_matrix_train[1,1] + confusion_matrix_train[2,2] + confusion_matrix_train[3,3]) / nrow(data_train)

print(paste("Test accuracy", accuracy_test, sep=" "))
print(paste("Train accuracy", accuracy_train, sep=" "))



# Model with all variables (Not the best)
log_mod_full <- multinom(uncertainty_set ~y_miss_per+avg_per_miss_x+r2_rr+n+p+
                           ind_Disp_y+median_x_norm_var+range_norm_x_var+
                           y_outliers_per+x_corr_per+mean_cor, data=data_train)

predict_full <- predict(log_mod_full, newdata = data_test)

conf_mat_full <- table(predict_full, data_test$uncertainty_set)
accuracy_full <- (conf_mat_full[1,1]+conf_mat_full[2,2]+conf_mat_full[3,3]) / nrow(data_test)
accuracy_full



### Baseline Models - Predict Most Common Class ###
baseline_table <- data %>%
  group_by(uncertainty_set) %>%
  summarise(Percent = n() / nrow(data))

