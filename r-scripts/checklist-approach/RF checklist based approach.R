#=======================================================================
# Random Forest - Checklist based approach (instead of pseudo-absence)
# Semipalmated Sandpiper (SESA) Migration Stopover Site Analysis


##Main Goal
#Identify which habitat and land cover characteristics predict SESA stopover site usage during migration, 
#and evaluate the long-term vulnerability of these stopover sites to sea level rise. 

##Hypothesis
#SESA stopover site usage is predictable using land cover characteristics (particularly the presence of coastal wetland habitats). 
#Sites that have experienced habitat degradation between the years 2001-2025 will be less likely to be used as stopover sites and 
#therefore will have a lower chance of SESA being present. 

#=======================================================================
# load in packages 
library(tidyverse)
library(randomForest)
library(pROC) 
library(caret)


#=======================================================================
#load in data 
project_root <- r"(C:\Users\cnreg\iCloudDrive\Summer 2026\Capstone\Project Data)"
model_data <- readRDS(file.path(project_root, "data", "final_locations_ohe.rds"))

nrow(model_data)
colnames(model_data)


#=======================================================================
#data prep 

# drop columns not needed for model 
model_data <- model_data |> select(-locality_id, -n_checklists)

# check variance of data
col_variance <- model_data |>
  select(-presence) |> #exclude presence (the predictor variable)
  summarise(across(everything(), var, na.rm = TRUE)) |> # variance to every column at once
  pivot_longer(everything(), names_to = "feature", values_to = "variance") |> # reshape so each feature has its own row 
  arrange(variance) # sort low to high for easy evaluation

# remove low variance features because they are useless and have essentially no predictive power 
low_var_cols <- col_variance |> filter(variance < 0.01) |> pull(feature)
print(low_var_cols)
model_data <- model_data |> select(-all_of(low_var_cols))

model_data$presence <- as.factor(model_data$presence)
#=======================================================================
#train test split 
set.seed(42)
train_index <- sample(1:nrow(model_data), size = 0.7 * nrow(model_data))
train <- model_data[train_index, ]
test <- model_data[-train_index, ]

nrow(train)
nrow(test)


#=======================================================================
# fit rf model 
set.seed(42)
rf_model <- randomForest(presence ~ .,
                         data = train,
                         ntree = 500,
                         importance = TRUE)
print(rf_model)


#=======================================================================
# evaluate model 
#predictions 
rf_probs <- as.numeric(predict(rf_model, newdata = test, type = "prob")[,2]) #run model on test data and return probabilities 0-1
rf_pred <- predict(rf_model, newdata = test, type = "class") 

#confusion matrix 
cm_rf <- confusionMatrix(rf_pred, test$presence, positive = "1")
print(cm_rf)

#auc-roc
roc_rf <- roc(as.numeric(test$presence), rf_probs)
auc(roc_rf)

cm_rf$byClass[c("Precision", "Recall", "F1")]

# feature importance plot 
varImpPlot(rf_model, main = "RF Feature Importance", col = "#E84855")
#=======================================================================
# tune model 
set.seed(42)
tuneRF(x = train |> select(-presence),
       y = train$presence,
       ntreeTry = 500,
       stepFactor = 1.5,
       improve = 0.01,
       trace = TRUE,
       plot = TRUE)

#fit tuned model 
set.seed(42)
rf_tuned <- randomForest(presence ~ .,
                         data = train,
                         ntree = 500,
                         mtry = 2,
                         importance = TRUE)
print(rf_tuned)


#=======================================================================
# evaluate model 

#predictions 
rf_probs_tuned <- as.numeric(predict(rf_tuned, newdata = test, type = "prob")[,2]) #run model on test data and return probabilities 0-1
rf_pred_tuned <- predict(rf_tuned, newdata = test, type = "class") 

#confusion matrix 
cm_rf_tuned <- confusionMatrix(rf_pred_tuned, test$presence, positive = "1")
print(cm_rf_tuned)

#auc-roc
roc_rf_tuned <- roc(as.numeric(test$presence), rf_probs_tuned)
auc(roc_rf_tuned)

cm_rf_tuned$byClass[c("Precision", "Recall", "F1")]



#=======================================================================
# K-fold CV

set.seed(42)
# defines rules for training and evaluation
cv_control <- trainControl(method = "cv", # cross-validation
                           number = 5,    # 5 folds
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary) # gives ROC, sensitivity, specificity

# creates data specifically formatted for the package caret
model_data_cv <- model_data |>
  mutate(presence = ifelse(presence ==1, "present", "absent"), # convert 0/1 to absent/present
         presence = factor(presence, levels = c("present", "absent"))) # convert to factor - lets caret know this is a classification problem

cv_rf <- train(presence ~., 
                  data = model_data_cv,
                  method = "rf", # use rf
                  trControl = cv_control, # uses previosuly defined control settings
                  metric = "ROC") # determine the metric is AUC-ROC to evaluate
print(cv_rf$results)

#savae model
saveRDS(rf_model, file.path(project_root, "data", "RF_checklist_model.rds"))
