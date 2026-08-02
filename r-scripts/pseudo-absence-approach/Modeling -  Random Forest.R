#=======================================================================
# Nonlinear Model - Random Forest
# Semipalmated Sandpiper (SESA) Migration Stopover Site Analysis

#=======================================================================
# load in packages 
library(tidyverse)
library(DBI)
library(RPostgres)
library(randomForest)
library(pROC) # for calculating and plotting AUC-ROC curves


#=======================================================================
#1. Get data from database 

con <- dbConnect(RPostgres::Postgres(),
                 dbname = "SESA Project",
                 host = "localhost",
                 port = 5432,
                 user = "postgres", 
                 password = "sesaproject")

model_data <- dbGetQuery(con, "SELECT * FROM habitat_model_data")
dbDisconnect(con)

colnames(model_data)
nrow(model_data)


#=======================================================================
#2. Data cleaning and preparation for models

# drop unecesary columns 
model_data <- model_data |> select(-locality_id, -nlcd_2025_NA, -n_detections, -n_years)

#make all column names clean and uniform - remove spaces and commas and slashes
colnames(model_data) <- make.names(colnames(model_data))

# fill na values 
model_data_clean <- model_data |> 
  mutate(
    land_cover_changed = replace_na(land_cover_changed, 0L),
    habitat_degraded = replace_na(habitat_degraded, 0L),
    habitat_improved = replace_na(habitat_improved, 0L))

# convert presence to factor because this is a classification problem 
model_data_clean$presence <- as.factor(model_data_clean$presence)


#=======================================================================
#3. Train Test Split 

set.seed(42)
train_index <- sample(1:nrow(model_data_clean), size = 0.7 * nrow(model_data_clean))
train <- model_data_clean[train_index, ]
test <- model_data_clean[-train_index, ]

message(paste("Training rows:", nrow(train)))
message(paste("Test rows:", nrow(test)))


#=======================================================================
#4. Fit Random Forest Model

rf_model <- randomForest(presence ~ ., #presence ~. predicts presence using all other columns (. is every feature)
                data = train, 
                ntree = 500,   # 500 decision trees for stable results 
                importance = TRUE) # track how much each feature contributes to predictions

print(rf_model)


#=======================================================================
#5. Predictions - Random Forest Model

rf_probs <- as.numeric(predict(rf_model, newdata = test, type = "prob")[,2]) # prob returns probabilities between 0 and 1
rf_pred <- predict(rf_model, newdata = test, type = "class") # class converts probabilities to class labels using 0.5 as the decision threshold (>0.5 presence, <0.5 absence)

#=======================================================================
#6. Random Forest Evaluation Metrics 

# confusion matrix
conf_matrix <- table(Predicted = rf_pred, Actual = test$presence)
print(conf_matrix)

# accuracy 
accuracy <- mean(rf_pred == test$presence)
message(paste("Accuracy:", round(accuracy, 3)))

# AUC-ROC 
roc_curve <- roc(test$presence, rf_probs)
message(paste("AUC-ROC:", round(auc(roc_curve), 3)))
par(mfrow = c(1,1))
plot(roc_curve, main = "LR ROC Curve", col = "#E84855")

# precision, recall, f1
precision <- conf_matrix[2,2] / sum(conf_matrix[2,])
recall <- conf_matrix[2,2] / sum(conf_matrix[,2])
f1 <- 2 * (precision * recall) / (precision + recall)                       

message(paste("Precision:", round(precision, 3)))
message(paste("Recall:", round(recall, 3)))
message(paste("F1:", round(f1, 3)))


#=======================================================================
#7. Feature Importance 

varImpPlot(rf_model, main = "RF Feature Importance", col = "#E84855")


#=======================================================================
#8. Hyperparameter Tuning 
set.seed(42)
rf_mtry3 <- randomForest(presence ~ ., data = train, ntree = 500, mtry = 3, importance = TRUE)
rf_mtry4 <- randomForest(presence ~ ., data = train, ntree = 500, mtry = 4, importance = TRUE)
rf_mtry6 <- randomForest(presence ~ ., data = train, ntree = 500, mtry = 6, importance = TRUE)

message(paste("mtry=3 OOB error:", round(rf_mtry3$err.rate[500,1], 3)))
message(paste("mtry=4 OOB error:", round(rf_mtry4$err.rate[500,1], 3)))
message(paste("mtry=6 OOB error:", round(rf_mtry6$err.rate[500,1], 3)))


#=======================================================================
#9. Fit Tuned Random Forest Model

rf_model_tuned <- randomForest(presence ~ ., #presence ~. predicts presence using all other columns (. is every feature)
                         data = train, 
                         ntree = 500,   # 500 decision trees for stable results
                         mtry = 6,
                         importance = TRUE) # track how much each feature contributes to predictions

print(rf_model_tuned)


#=======================================================================
#10. Predictions - Random Forest Model

rf_probs_tuned <- as.numeric(predict(rf_model_tuned, newdata = test, type = "prob")[, 2]) # prob returns probabilities between 0 and 1
rf_pred_tuned <- predict(rf_model_tuned, newdata = test, type = "class") # class converts probabilities to class labels using 0.5 as the decision threshold (>0.5 presence, <0.5 absence)

#=======================================================================
#11. Random Forest Evaluation Metrics 

# confusion matrix
conf_matrix_tuned <- table(Predicted = rf_pred_tuned, Actual = test$presence)
print(conf_matrix_tuned)

# accuracy 
accuracy_tuned <- mean(rf_pred_tuned == test$presence)
message(paste("Accuracy:", round(accuracy_tuned, 3)))

# AUC-ROC 
roc_tuned <- roc(test$presence, rf_probs_tuned)
message(paste("AUC-ROC:", round(auc(roc_tuned), 3)))
par(mfrow = c(1,1))
plot(roc_curve, main = "LR ROC Curve", col = "#E84855")

# precision, recall, f1
precision_tuned <- conf_matrix_tuned[2,2] / sum(conf_matrix_tuned[2,])
recall_tuned <- conf_matrix_tuned[2,2] / sum(conf_matrix_tuned[,2])
f1_tuned <- 2 * (precision_tuned * recall_tuned) / (precision_tuned + recall_tuned)                       

message(paste("Precision:", round(precision_tuned, 3)))
message(paste("Recall:", round(recall_tuned, 3)))
message(paste("F1:", round(f1_tuned, 3)))
