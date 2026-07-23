#=======================================================================
# Baseline Model - Logistic Regression
# Semipalmated Sandpiper (SESA) Migration Stopover Site Analysis

#=======================================================================
# load in packages 
library(tidyverse)
library(DBI)
library(RPostgres)
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


# fill na with 0 
model_data_cleaned <- model_data |> 
  mutate(  
    land_cover_changed = replace_na(land_cover_changed, 0L), # 0 means no change 
    habitat_degraded = replace_na(habitat_degraded, 0L)) # 0 means no habitat degradation 

colnames(model_data_cleaned)

# checking variance of all columns 
col_variance <- model_data_cleaned |> 
  select(-presence) |> 
  summarise(across(everything(), var, na.rm = TRUE)) |>
  pivot_longer(everything(), names_to = "feature", values_to = "variance") |> 
  arrange(variance)
print(col_variance)

# remove features with near zero variance 
low_var_cols <- col_variance |> 
  filter(variance < 0.01) |> 
  pull(feature)
print(low_var_cols)

model_data_cleaned <- model_data_cleaned |> select(-all_of(low_var_cols))
#=======================================================================
#3. Train Test Split 

set.seed(42)
train_index <- sample(1:nrow(model_data_cleaned), size = 0.7 * nrow(model_data_cleaned))
train <- model_data_cleaned[train_index, ]
test <- model_data_cleaned[-train_index, ]

message(paste("Training rows:", nrow(train)))
message(paste("Test rows:", nrow(test)))


#=======================================================================
#4. Fit Logistic Regression Model

lr_model <- glm(presence ~ ., #glm fits generalized linear model, presence ~. predicts presence using all other columns (. is every feature)
                data = train, 
                family = binomial) # binomial designates it as logistic not linear regression

summary(lr_model)


#=======================================================================
#5. Predictions - Logistic Regression Model

lr_probs <- predict(lr_model, newdata = test, type = "response") # response returns probabilities between 0 and 1
lr_pred <- ifelse(lr_probs > 0.5, 1, 0) # ifelse converts probabilities to class labels using 0.5 as the decision threshold (>0.5 presence, <0.5 absence)

#=======================================================================
#6. Logistic Regression Evaluation Metrics 

# confusion matrix
conf_matrix <- table(Predicted = lr_pred, Actual = test$presence)
print(conf_matrix)

# accuracy - of all predictions made how many were correct 
accuracy <- mean(lr_pred == test$presence)
message(paste("Accuracy:", round(accuracy, 3)))

# AUC-ROC - how well the model is at separating presence sites above absence sites 
# sensitivity (recall) - of all presence sites how many did the model label correctly 
# specificity - of all absence sites how many did the model label correctly 
roc_curve <- roc(test$presence, lr_probs)
message(paste("AUC-ROC:", round(auc(roc_curve), 3)))
par(mfrow = c(1,1))
plot(roc_curve, main = "LR ROC Curve", col = "#E84855")

# precision, recall, f1
# of all sites predicted as presence how many actually were
precision <- conf_matrix[2,2] / sum(conf_matrix[2,]) # tp / (tp + fp)

# of all presence sites how many did the model catch
recall <- conf_matrix[2,2] / sum(conf_matrix[,2]) # tp / (tp + fn)

# balances precision and recall 
f1 <- 2 * (precision * recall) / (precision + recall) # 2*(prec * rec) / (prec + rec)                    

message(paste("Precision:", round(precision, 3)))

message(paste("Recall:", round(recall, 3)))
message(paste("F1:", round(f1, 3)))


#========================================================================
#7. K-fold CV
#install.packages("caret")
library(caret) #package that standardizes model training and evaluation

set.seed(42)
# defines rules for training and evaluation
cv_control <- trainControl(method = "cv", # cross-validation
                           number = 5,    # 5 folds
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary) # gives ROC, sensitivity, specificity

# creates data specifically formatted for the package caret
model_data_cv <- model_data_cleaned |>
  mutate(presence = ifelse(presence ==1, "present", "absent"), # convert 0/1 to absent/present
         presence = factor(presence, levels = c("present", "absent"))) # convert to factor - lets caret know this is a classification problem

cv_model <- train(presence ~., 
                  data = model_data_cv,
                  method = "glm", # use regression
                  family = "binomial", # logistic not linear
                  trControl = cv_control, # uses previosuly defined control settings
                  metric = "ROC") # determine the metric is AUC-ROC to evaluate
print(cv_model$results)


#========================================================================
#8. Naive Baseline Comparison

# counts up 0 and 1 and determines which is more and labels it the majority class
majority_class <- names(which.max(table(test$presence)))

# creates a vector of predictions that is all the majority class
naive_pred <- rep(majority_class, nrow(test))

# calculates accuracy of just guessing majority class
naive_accuracy <- mean(naive_pred ==test$presence)

message(paste("Naive Baseline Accuracy:", round(naive_accuracy, 3)))
message("Naive Baseline AUC-ROC: 0.5 (random classifier)")
message(paste("Logistic Regression Accuracy improvement over Naive Baseline:", round(accuracy - naive_accuracy, 3)))
message(paste("Logistic AUC-ROC improvement over Naive Baseline:", round(auc(roc_curve) - 0.5, 3)))


#========================================================================
#9. Error Analysis

# add 3 new columns to test set - predicted, probability, correct
test_results <- test |> 
  mutate(predicted = lr_pred,
         probability = lr_probs,
         correct = ifelse(predicted == presence, "Correct", "Incorrect"))

# rows that the model was incorrect predicting 
misclassified <- test_results |> filter(correct == "Incorrect")
message(paste("Total misclassified:", nrow(misclassified)))

# false negatives - shows where model predicted absent and there was presence ( a hotspot)
false_negatives <- test_results |> 
  filter(predicted == 0 & presence == 1)
message(paste("False Negatives (missed hotspots):", nrow(false_negatives)))

# false positives - falsely reported as hotspots when they are not 
false_positives <- test_results |> 
  filter(predicted == 1 & presence == 0)
message(paste("False Positives (labeled hotspots falsely):", nrow(false_positives)))
print(false_positives)
print(false_negatives)

#========================================================================
#10. Learning Curves

# create seq of proportions from 10% to 90% - the training subset sizes
train_sizes <- seq(0.1, 0.9, by = 0.1)

# empty vectors to store auc scores 
train_auc <- c()
val_auc <- c()

set.seed(42)

for (size in train_sizes) {
  idx <- sample(1:nrow(train), size = floor(size * nrow(train))) #randomly selects the set amount of training data
  train_subset <- train[idx, ] # based on the rows selected create a smaller traiing data set
  
  #fits temporary model on the subset
  model_temp <- glm(presence ~., data = train_subset, family = binomial)
  
  # gets pred on training subset and full test set 
  train_probs <- predict(model_temp, newdata = train_subset, type = "response")
  val_probs <- predict(model_temp, newdata = test, type = "response")
  
  # calculates auc for particular subset and adds it to the list to build the full curve 
  train_auc <- c(train_auc, as.numeric(auc(roc(train_subset$presence, train_probs))))
  val_auc <- c(val_auc, as.numeric(auc(roc(test$presence, val_probs))))
}

# how performance changes with increased amount of training 
# red is train blue is val
# not overfitting because not large gap between train and val 
# not underfitting because not doing poorly
plot(train_sizes * nrow(train), train_auc,
     type = "l", col = "#E84855", ylim = c(0.5, 1.0),
     xlab = "Training Size", ylab = "AUC-ROC",
     main = "Learning Curves")
lines(train_sizes *nrow(train), val_auc, col = "#2E86AB")
legend("bottomright", legend = c("Train", "Validation"),
       col = c("#E84855", "#2E86AB"), lty = 1)

#========================================================================
#11. Training and Testing Time 

start_train <- proc.time()
lr_model_timed <- glm(presence ~., data = train, family = binomial)
end_train <- proc.time()

start_test <- proc.time()
timed_probs <- predict(lr_model_timed, newdata = test, type = "response")
end_test <- proc.time()

train_time <- (end_train - start_train)["elapsed"]
test_time <- (end_test - start_test)["elapsed"]

message(paste("Training time:", round(train_time, 3), "seconds"))
message(paste("Testing time:", round(test_time, 3), "seconds"))


#========================================================================
#12. Save 

project_root <- r"(C:\Users\cnreg\iCloudDrive\Summer 2026\Capstone\Project Data)"
saveRDS(lr_model, file.path(project_root, "data", "LogisticRegression_Model.rds"))
