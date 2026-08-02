#=======================================================================
# Model COmparison - Checklist based approach (instead of pseudo-absence)
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
library(pROC)
library(gbm)
library(randomForest) 
library(caret)


#=======================================================================
#load in data and models 
project_root <- r"(C:\Users\cnreg\iCloudDrive\Summer 2026\Capstone\Project Data)"
model_data <- readRDS(file.path(project_root, "data", "final_locations_ohe.rds"))

lr_model <- readRDS(file.path(project_root, "data", "LR_checklist_model.rds"))
rf_model <- readRDS(file.path(project_root, "data", "RF_checklist_model.rds"))
brt_model <- readRDS(file.path(project_root, "data", "BRT_checklist_model.rds"))

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


#=======================================================================
#predictions for each model 
lr_probs <- predict(lr_model, newdata = test, type = "response") #run model on test data and return probabilities 0-1

model_data_rf <- model_data
model_data_rf$presence <- as.factor(model_data_rf$presence)
test_rf <- model_data_rf[-train_index, ]
rf_probs <- as.numeric(predict(rf_model, newdata = test, type = "prob")[,2]) #run model on test data and return probabilities 0-1

best_trees <- gbm.perf(brt_model, method = "cv")  #gbm.perf() finds the # trees where cv error is lowest to avoid overfitting 
brt_probs <- predict(brt_model, newdata = test, n.trees = best_trees, type = "response") #run model on test data and return probabilities 0-1

nrow(test)


#=======================================================================
#ROC curves and delongs test for each model 
roc_lr <- roc(test$presence, lr_probs)
roc_rf <- roc(as.numeric(test$presence), rf_probs)
roc_brt <- roc(test$presence, brt_probs)

#AUC w/ CI
cat("LR AUC:", round(auc(roc_lr), 4), "\n")
cat("RF AUC:", round(auc(roc_rf), 4), "\n")
cat("BRT AUC:", round(auc(roc_brt), 4), "\n")

ci.auc(roc_lr)
ci.auc(roc_rf)
ci.auc(roc_brt)

#Delong's test 
roc.test(roc_lr, roc_rf, method = "delong")
roc.test(roc_lr, roc_brt, method = "delong")
roc.test(roc_rf, roc_brt, method = "delong")


#ROC plot
plot(roc_lr, col = "#2E86AB", main = "ROC Curve Comparison")
lines(roc_rf, col = "#E84855")
lines(roc_brt, col = "#28A745")
legend("bottomright",
       legend = c(paste("GLM AUC=", round(auc(roc_lr), 3)),
                  paste("RF AUC=", round(auc(roc_rf), 3)),
                  paste("BRT AUC=", round(auc(roc_brt), 3))),
       col = c("#2E86AB", "#E84855", "#28A745"),
       lty = 1)
       























