#=======================================================================
# Temporal Site Analysis - Checklist based approach (instead of pseudo-absence)
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
library(sf)


#=======================================================================
#load in data
project_root <- r"(C:\Users\cnreg\iCloudDrive\Summer 2026\Capstone\Project Data)"

ebd_clean <- readRDS(file.path(project_root, "data", "ebd_clean.rds"))
final_locations_sf <- readRDS(file.path(project_root, "data", "final_locations_sf.rds")) 
sampling <- read_sampling(file.path(project_root, "data", "ebd_sampling_filtered.txt"))
final_locations_ohe <- readRDS(file.path(project_root, "data", "final_locations_OHE.rds")) 


nrow(ebd_clean)
colnames(ebd_clean)



#=======================================================================
#get locaitons for presence sites

presence_sites <- final_locations_sf |> filter(presence == 1) |> pull(locality_id)

#count detections per year at presence sites 
temporal_trends <- ebd_clean |> filter(locality_id %in% presence_sites) |> group_by(locality_id, year) |> summarise(n_detections = n(), .groups = "drop")

nrow(temporal_trends)
head(temporal_trends)


#=======================================================================
#plot detections at presence sites by year 

yearly_totals <- temporal_trends |>
  group_by(year) |>
  summarise (total_detections = sum(n_detections),
             n_sites_active = n_distinct(locality_id))

#this plot is biased by increased usage of ebird in more recent years
ggplot(yearly_totals, aes(x=year, y=total_detections)) +
  geom_line(color = "#E84855", linewidth = 1) +
  geom_point(color = "#E84855", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "#2E86AB", linetype = "dashed") +
  labs(title = "Temporal trends in SESA Detections at Stopover Sites",
       x = "Year",
       y = "Total Detections") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#to get rid of bias here i will plot detection rate not total detections 

#checklist counts by month and location 
checklist_counts <- sampling |>
  filter(locality_id %in% presence_sites) |>
  filter(month(observation_date) %in% c(5, 6, 7, 8, 9)) |>
  group_by(year = year(observation_date)) |>
  summarise(n_checklists = n(), .groups = "drop")

#join, calculate detection rate
yearly_totals <- yearly_totals |> left_join(checklist_counts, by = "year") |>
  mutate(detection_rate = total_detections / n_checklists)

ggplot(yearly_totals, aes(x = year, y = detection_rate)) +
  geom_line(color = "#E84855", linewidth = 1) +
  geom_point(color = "#E84855", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "#2E86AB", linetype = "dashed") +
  labs(title = "SESA Detection Rate at Stopover Sites",
       x = "Year",
       y = "Detections per Checklist") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))



#checking if my plots are showing statistically significant trends or capturing noise in my data
trend_model <- lm(detection_rate ~ year, data = yearly_totals)
summary(trend_model)


#pinpointing the sites that are declining 
library(broom)

site_trends <- temporal_trends |> 
  group_by(locality_id) |> 
  filter(n() >=5) |> #sites with at least 5 years of data 
  do(tidy(lm(n_detections ~ year, data = .))) |>
  filter(term == "year") |>
  select(locality_id, slope = estimate, p_value = p.value)

#declining sites will have negative slopes 
declining_sites <- site_trends |>
  filter(slope < 0 & p_value < 0.05) |> 
  arrange(slope)

nrow(declining_sites)
head(declining_sites)


#identify the delcining site 
ebd_clean |> 
  filter(locality_id == "L455080") |> 
  select(locality_id, state, latitude, longitude) |>
  distinct()

#is there habitat degredation at the RI site 
final_locations_sf |> 
  filter(locality_id == "L455080") |>
  st_drop_geometry()

final_locations_ohe |> filter(locality_id == "L455080")



#create detection rate in temporal trends 
temporal_trends <- temporal_trends |> 
  left_join(
    sampling |> 
      filter(locality_id %in% presence_sites) |>
      filter(month(observation_date) %in% c(5, 6, 7, 8, 9)) |>
      group_by(locality_id, year = year(observation_date)) |> 
      summarise(n_checklists = n(), .groups = "drop"),
    by = c("locality_id", "year")
  ) |> 
  mutate(detection_rate = n_detections / n_checklists)




#plot decline at this RI site 
temporal_trends |> filter(locality_id == "L455080")|> filter(n_checklists >=5) |>
  ggplot(aes(x = year, y = detection_rate)) +
  geom_line(color = "#E84855", linewidth = 1) +
  geom_point(color = "#E84855", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "#2E86AB", linetype = "dashed") +
  labs(title = "SESA Detection Rate at Declining RI Stopover Site",
       x = "Year",
       y = "Detections per Checklist") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#confirm statistical significance
site_lm <- temporal_trends |> filter(locality_id == "L455080") |> filter(n_checklists >=5) 
  
summary(lm(detection_rate ~ year, data = site_lm))

