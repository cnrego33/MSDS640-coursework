#=======================================================================
# EDA
# Semipalmated Sandpiper (SESA) Migration Stopover Site Analysis

#=======================================================================
# Load in packages 

library(tidyverse)
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


#=======================================================================
#2. Missing values 

message("EBD Missing Values")
colSums(is.na(ebd_db))

# missing values are not in columns that will effect my models. 
# missing values in columns - effort_distance_km, number_observers, and observation_count_clean, duration_minutes 
# therefore nothing needs to be done with missing values here 


message("Habitat missing values")
colSums(is.na(habitat_db))

message("Hotspot missing values")
colSums(is.na(hotspots_db))


#=======================================================================
#3. Summary Stats

message("EBD Summary")
summary(ebd_db)

message("Hotspot Summary")
summary(hotspots_db)

message("Habitat Summary")
summary(habitat_db)


#=======================================================================
#4. Plot Distributions

project_root <- r"(C:\Users\cnreg\iCloudDrive\Summer 2026\Capstone\Project Data)"

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


#=======================================================================
#5. Heat Map 

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
