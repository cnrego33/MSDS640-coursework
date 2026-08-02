#=======================================================================
# Logistic Regression - Checklist based approach (instead of pseudo-absence)
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
library(DBI)
library(RPostgres)
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


#=======================================================================
#train test split 
set.seed(42)
train_index <- sample(1:nrow(model_data), size = 0.7 * nrow(model_data))
train <- model_data[train_index, ]
test <- model_data[-train_index, ]

nrow(train)
nrow(test)


#=======================================================================
#fit logistic regression model 
lr_model <- glm(presence ~ ., data = train, family = binomial)
summary(lr_model)

#predictions 
lr_probs <- predict(lr_model, newdata = test, type = "response") #run model on test data and return probabilities 0-1
lr_pred <- ifelse(lr_probs > 0.4, 1, 0) # convert probabilities to binary predictions 50% confidence threshold 

#confusion matrix 
conf_matrix <- table(Predicted = lr_pred, Actual = test$presence)
cm <- confusionMatrix(as.factor(lr_pred), as.factor(test$presence), positive = "1")
cm
#auc-roc
roc_curve <- roc(test$presence, lr_probs)
auc(roc_curve)

cm$byClass[c("Precision", "Recall", "F1")]


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

cv_model <- train(presence ~., 
                  data = model_data_cv,
                  method = "glm", # use regression
                  family = "binomial", # logistic not linear
                  trControl = cv_control, # uses previosuly defined control settings
                  metric = "ROC") # determine the metric is AUC-ROC to evaluate
print(cv_model$results)


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



#save model
saveRDS(lr_model, file.path(project_root, "data", "LR_checklist_model.rds"))
