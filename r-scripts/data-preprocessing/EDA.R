#=======================================================================
# EDA and feature engineering 
# Semipalmated Sandpiper (SESA) Migration Stopover Site Analysis

#=======================================================================
# Load in packages 

library(tidyverse)
library(tidyr)
library(sf)
library(terra)
library(DBI)
library(RPostgres)
library(ggplot2)
library(reshape2)


#=======================================================================
#1. Get data from database 

con <- dbConnect(RPostgres::Postgres(),
                 dbname = "SESA Project",
                 host = "localhost",
                 port = 5432,
                 user = "postgres", 
                 password = "sesaproject")

ebd_db <- dbGetQuery(con, "SELECT * FROM ebd_occurences")
hotspots_db <- dbGetQuery(con, "SELECT * FROM hotspot_sites")
habitat_db <- dbGetQuery(con, "SELECT * FROM habitat_extracted")

dbDisconnect(con)

nrow(ebd_db)
nrow(hotspots_db)
nrow(habitat_db)

#-----------------------------------------------------------------------
#1.1 get data from data folder 
project_root <- r"(C:\Users\cnreg\iCloudDrive\Summer 2026\Capstone\Project Data)"
hotspot_sf <- readRDS(file.path(project_root, "data", "hotspot_sf.rds"))
psuedo_abs <- readRDS(file.path(project_root, "data", "pseudo_abs.rds"))
pseudo_features <- readRDS(file.path(project_root, "data", "pseudo_nlcd_features.rds"))

#=======================================================================
#2. Missing values 

# ebird data
message("EBD Missing Values")
colSums(is.na(ebd_db))

rowSums(is.na(ebd_db))
colMeans(is.na(ebd_db))
dim(ebd_db)

colnames(ebd_db)

ebd_db <- ebd_db |> 
  mutate( 
    duration_minutes = replace_na(duration_minutes, median(duration_minutes, na.rm = TRUE)),
    effort_distance_km = replace_na(effort_distance_km, median(effort_distance_km, na.rm = TRUE)),
    number_observers = replace_na(number_observers, median(number_observers, na.rm = TRUE)),
    observation_count_clean = replace_na(observation_count_clean, 0)
    )

colSums(is.na(ebd_db))

# missing values are not in columns that will effect my models. 
# missing values in columns - effort_distance_km, number_observers, and observation_count_clean, duration_minutes 
# fill with median values (except fill observation count with zeros because SESA was present but not counted)

#write fillna data back to database to replace original ebd_occurences file 
con <- dbConnect(RPostgres::Postgres(),
                 dbname = "SESA Project",
                 host = "localhost",
                 port = 5432,
                 user = "postgres", 
                 password = "sesaproject")

dbWriteTable(con, "ebd_occurences", as.data.frame(ebd_db), overwrite = TRUE)
dbGetQuery(con, "SELECT COUNT(*) FROM ebd_occurences")
dbDisconnect(con)


# habitat data - no missing values 
message("Habitat missing values")
colSums(is.na(habitat_db))
rowSums(is.na(habitat_db))
colMeans(is.na(habitat_db))



# hotspot data - no missing values 
message("Hotspot missing values")
colSums(is.na(hotspots_db))
rowSums(is.na(hotspots_db))
colMeans(is.na(hotspots_db))

#=======================================================================
#3. Data Imbalance
# the data is not imbalanced so ther is no need for oversampling or class weights
# 500 hotspots and 200 random absences 
presence <- data.frame(presence = 1L, source = "hotspot")
absence <- data.frame(presence = 0L, source = "pseudo-absence")

target_dist <- bind_rows(presence[rep(1, nrow(hotspot_sf)), ],
                         absence[rep(1, nrow(psuedo_abs)), ])

ggplot(target_dist, aes(x=as.factor(presence), fill = as.factor(presence))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "#555555", "1" = "#E84855"),
                    labels = c("Pseudo-absence", "Presence")) +
  scale_x_discrete(labels = c("0" = "Absent", "1" = "Present")) + 
  labs(
    title = "Distrubition of Presence/Absense",
    x = "SESA Presence", 
    y = "Count",
    fill = "Class"
  ) +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))
#=======================================================================
#4. Summary Stats

message("EBD Summary")
summary(ebd_db)

message("Hotspot Summary")
summary(hotspots_db)

message("Habitat Summary")
summary(habitat_db)


#=======================================================================
#5. Feature Engineering

# install packages needed for feature engineering
# install.packages("fastDummies")
library(fastDummies)
# install.packages("corrplot")
library(corrplot)

# encode land cover class categories
habitat_encoded <- dummy_cols(habitat_db, 
                              select_columns = c("nlcd_2001", "nlcd_2025"), 
                              remove_first_dummy = TRUE, 
                              remove_selected_columns = TRUE)

# write new OHE table back to db
dbWriteTable(con, "habitat_encoded", as.data.frame(habitat_encoded), overwrite = TRUE)

# scale data for linear model 
habitat_encoded_scaled <- habitat_encoded |> 
  mutate( 
    n_detections_scaled = scale(n_detections), 
    n_years_scaled = scale(n_years))

# write new scaled OHE table back to db
con <- dbConnect(RPostgres::Postgres(),
                 dbname = "SESA Project",
                 host = "localhost",
                 port = 5432,
                 user = "postgres", 
                 password = "sesaproject")
dbWriteTable(con, "habitat_encoded_scaled", as.data.frame(habitat_encoded_scaled), overwrite = TRUE)

# correlation plot 

# select numeric features only 
numeric_features <- habitat_encoded |> select(where(is.numeric))

# corr mat 
corr_matrix <- cor(numeric_features, use = "complete.obs")

# plot 
jpeg(file.path(project_root, "data", "correlation_heatmap.jpg"), width = 1400, height = 1400, res = 120)
corrplot(corr_matrix, 
         method = "color", 
         type = "upper", 
         addCoef.col = "black", 
         number.cex = 0.6, 
         tl.col = "black", 
         tl.cex = 0.7,
         title = "Feature Correlation Heatmap", 
         mar = c(0,0,2,0))
dev.off()
graphics.off()

# encoding habitat changes from 2001-2025 
unique(habitat_db$nlcd_2025)

good_habitat<- c("Emergent Herbaceous Wetlands", 
                 "Open Water",
                 "Woody Wetlands",
                 "Grassland/Herbaceous",
                 "Pasture/Hay",
                 "Barren Land")

habitat_features<- habitat_db |> 
  mutate(
    land_cover_changed = ifelse(nlcd_2001 != nlcd_2025, 1L, 0L),
    habitat_degraded = ifelse(nlcd_2001 %in% good_habitat & !nlcd_2025 %in% good_habitat, 1L, 0L), 
    habitat_improved = ifelse(!nlcd_2001 %in% good_habitat & nlcd_2025 %in% good_habitat, 1L, 0L)
  )

# write new habitat change encoded table back to db
dbWriteTable(con, "habitat_features", as.data.frame(habitat_features), overwrite = TRUE)


# drop habitat features that arent needed 
colnames(habitat_features)

habitat_model_data <- habitat_features |> select(-slr_05m, -slr_10m, -nlcd_2001, -seasons)

habitat_model_data <- dummy_cols(habitat_model_data, 
                              select_columns = c("nlcd_2025"), 
                              remove_first_dummy = TRUE, 
                              remove_selected_columns = TRUE)

colnames(habitat_model_data)

# encoding pseudo habitat changes from 2001-2025 
nrow(pseudo_features)
sum(is.na(pseudo_features$nlcd_2001))

sum(is.na(pseudo_features$nlcd_2025))
pseudo_features<- pseudo_features |> 
  mutate(
    land_cover_changed = ifelse(nlcd_2001 != nlcd_2025, 1L, 0L),
    habitat_degraded = ifelse(nlcd_2001 %in% good_habitat & !nlcd_2025 %in% good_habitat, 1L, 0L), 
    habitat_improved = ifelse(!nlcd_2001 %in% good_habitat & nlcd_2025 %in% good_habitat, 1L, 0L)
  )|> select(-slr_05m, -slr_10m, -nlcd_2001)

pseudo_nlcd_features <- dummy_cols(pseudo_features, 
                                 select_columns = c("nlcd_2025"), 
                                 remove_first_dummy = TRUE, 
                                 remove_selected_columns = TRUE)



# combine hotspot & pseudo-absence features to prepare for modeling

# add presence column to model data
habitat_model_data <- habitat_model_data |> mutate(presence = 1L)

# combine the two 
colnames(habitat_model_data)
colnames(pseudo_nlcd_features)

habitat_model_data <- bind_rows(habitat_model_data, pseudo_nlcd_features)

# Replace NA with 0
ohe_cols<- grep("nlcd_2025_", colnames(habitat_model_data), value = TRUE)
habitat_model_data <- habitat_model_data |> mutate(across(all_of(ohe_cols), ~replace_na(.,0))) 

# save habitat model data
saveRDS(habitat_model_data, file.path(project_root, "data", "habitat_model_data.rds"))


message(paste("total rows:", nrow(habitat_model_data)))
message(paste("presences:", sum(habitat_model_data$presence ==1)))
message(paste("absences:", sum(habitat_model_data$presence ==0)))
#=======================================================================
#6. Plot Distributions

project_root <- r"(C:\Users\cnreg\iCloudDrive\Summer 2026\Capstone\Project Data)"
rm(nlcd_2001, nlcd_2025)

# Detections per hotspot
ggplot(hotspots_db, aes(x= n_detections)) +
  geom_histogram(bins = 30, fill = "#E84855", color = "white") + 
  labs(
    title = "Detections per Hotspot Site Dist", 
    x = "# Detections", 
    y = "# of Sites") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
  

# NLCD Land Cover Class 
ggplot(habitat_db, aes(x=fct_infreq(nlcd_2001))) + 
  geom_bar(fill = "#E84855") +
  labs(
    title = "NLCD Land Cover CLasses at Hotspots (2001)", 
    x = "Land Cover Class", 
    y = "# of Sites") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 45, hjust =1))
  
ggplot(habitat_db, aes(x=fct_infreq(nlcd_2025))) + 
  geom_bar(fill = "#E84855") +
  labs(
    title = "NLCD Land Cover CLasses at Hotspots (2025)", 
    x = "Land Cover Class", 
    y = "# of Sites") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 45, hjust =1))

# Stopover site SLR Vulnerability 
slr_summary <- habitat_db |> 
  summarise(
    '0.5m inundated' = sum(slr_05m ==1), 
    '0.5m not inundated' = sum(slr_05m ==0), 
    '1.0m inundated' = sum(slr_10m ==1), 
    '1.0m not inundated' = sum(slr_10m ==0),
  ) |>
  pivot_longer(everything(), names_to = "Category", values_to = "Sites")

ggplot(slr_summary, aes( x = Category, y = Sites, fill = Category)) +
  geom_col() + 
  scale_fill_manual(values = c("#E84855", "#555555", "#FF7F00", "#CCCCCC")) + 
  labs(
    title = "SLR Inundation at Hotspots (2025)", 
    x = " ", 
    y = "# of Sites") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none")

# Outlier detection 
# detections & years 
outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  sum(x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR), na.rm = TRUE)
}

message(paste("detection outliers:", outliers(habitat_model_data$n_detections)))
message(paste("years outliers:", outliers(habitat_model_data$n_years)))

# boxplots 
par(mfrow = c(1,2))
boxplot(habitat_model_data$n_detections, 
        main = "Detections Boxplot", 
        ylab = " # Detections", 
        col = "#E84855")

# boxplots 
boxplot(habitat_model_data$n_years, 
        main = "Years Boxplot", 
        ylab = " # Years Detected", 
        col = "#E84855")

# class boxplots 
par(mfrow = c(1,3))
#land cover changed 
lcc <- tapply(habitat_model_data$land_cover_changed, habitat_model_data$presence, mean, na.rm = TRUE)
barplot(lcc, names.arg = c("Absence", "Presence"),
        main = "Land Cover Changed", 
        ylab = " Proportion", 
        col = c("#2E86AB", "#E84855"))

hab_deg <- tapply(habitat_model_data$habitat_degraded, habitat_model_data$presence, mean, na.rm = TRUE)
barplot(lcc, names.arg = c("Absence", "Presence"),
        main = "Habitat Degraded", 
        ylab = " Proportion", 
        col = c("#2E86AB","#E84855"))

hab_imp <- tapply(habitat_model_data$habitat_improved, habitat_model_data$presence, mean, na.rm = TRUE)
barplot(lcc, names.arg = c("Absence", "Presence"),
        main = "Habitat Improved", 
        ylab = " Proportion", 
        col = c("#2E86AB","#E84855"))

# scatterplots 
par(mfrow = c(2,2))

# duration v  ddetections
plot(ebd_db$duration_minutes, ebd_db$observation_count_clean,
     main = "Duration v Detections", 
     xlab = "Duration (mins)", ylab = "Observations",
     col = alpha("#E84855", 0.3), pch = 16)
abline(lm(observation_count_clean ~ duration_minutes, data = ebd_db), lty =2)

# observers v detections
plot(ebd_db$number_observers, ebd_db$observation_count_clean,
     main = "Observers v Detections", 
     xlab = "# Observers", ylab = "Observations",
     col = alpha("#E84855", 0.3), pch = 16)
abline(lm(observation_count_clean ~ number_observers, data = ebd_db), lty =2)

# effort distance v detections 
plot(ebd_db$effort_distance_km, ebd_db$observation_count_clean,
     main = "Distance v Detections", 
     xlab = "Distance (km)", ylab = "Observations",
     col = alpha("#E84855", 0.3), pch = 16)
abline(lm(observation_count_clean ~ effort_distance_km, data = ebd_db), lty =2)

# detections v years (hotspot)
plot(habitat_db$n_years, habitat_db$n_detections,
     main = "Site Years v Detections", 
     xlab = "# Years Detected", ylab = "# Detections",
     col = alpha("#E84855", 0.3), pch = 16)
abline(lm(n_detections ~ n_years, data = habitat_db), lty =2)
par(mfrow = c(1,1))
#=======================================================================
#7. Heat Map 

heatmap_data <- ebd_db |> 
  mutate(state_abbr = state.abb[match(state, state.name)]) |> 
  group_by(year, state_abbr) |> 
  summarise(n_detections = n(), .groups = "drop")

ggplot(heatmap_data, aes(x=year, y=state_abbr, fill=n_detections)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low="#FFFFCC", high = "#E84855", name = "Detections") +
  labs(title = "SESA Detections by Year and State", 
       x = "Year", 
       y = "State") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
