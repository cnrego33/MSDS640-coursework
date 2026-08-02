#=======================================================================
# Boosted Regression Trees - Checklist based approach (instead of pseudo-absence)
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
library(gbm)
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
# fit brt model 

set.seed(42)

brt_model <- gbm(presence ~ ., 
                 data = train, 
                 distribution = "bernoulli", #binary classification 
                 n.trees = 1000,             #max number of trees to build
                 interaction.depth = 3,      #how deep each tree can grow, complexity
                 shrinkage = 0.01,           #learning rate, smaller = more accurate but more conservative
                 cv.folds = 5,               # 5-fold cv
                 verbose = FALSE)

#find optimal number of trees 
best_trees <- gbm.perf(brt_model, method = "cv")  #gbm.perf() finds the # trees where cv error is lowest to avoid overfitting 
cat("Optimal number of trees:", best_trees)


#=======================================================================
# evaluate brt model 
#predictions 
brt_probs <- predict(brt_model, newdata = test, n.trees = best_trees, type = "response") #run model on test data and return probabilities 0-1
brt_pred <- as.factor(ifelse(brt_probs > 0.5, 1, 0))

#confusion matrix 
cm_brt <- confusionMatrix(brt_pred, as.factor(test$presence), positive = "1")
print(cm_brt)

cm_brt$byClass[c("Precision", "Recall", "F1")]

#auc-roc
roc_brt <- roc(test$presence, brt_probs)
auc(roc_brt)


# feature importance plot 
summary(brt_model, n.trees = best_trees, main = "BRT Feature Importance")


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

cv_brt <- train(presence ~., 
               data = model_data_cv,
               method = "gbm", # use gbm
               trControl = cv_control, # uses previosuly defined control settings
               metric = "ROC",
               verbose = FALSE) # determine the metric is AUC-ROC to evaluate
print(cv_brt$results)


#save model 
saveRDS(brt_model, file.path(project_root, "data", "BRT_checklist_model.rds"))
