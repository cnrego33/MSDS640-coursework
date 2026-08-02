#=======================================================================
# Change Presence/Absence Approach -> Checklist based (instead of pseudo-absence)
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
library(sf) 
library(auk)


#=======================================================================
#load in data 
project_root <- r"(C:\Users\cnreg\iCloudDrive\Summer 2026\Capstone\Project Data)"
sampling <- read_sampling(file.path(project_root, "data", "ebd_sampling_filtered.txt"))

nrow(sampling)
colnames(sampling)


#filter to remove incomplete checklists (true absences need all species to be recorded)
sampling_complete <- sampling |> 
  filter(all_species_reported == TRUE) |> 
  filter(month(observation_date) %in% c(5, 6, 7, 8, 9))

nrow(sampling_complete)

#load SESA detections 
sesa <- read_ebd(file.path(project_root, "data", "ebd_SESA_filtered.txt"))

# get the checklist IDs where SESA was detected
sesa_checklists <- unique(sesa$sampling_event_identifier)

#create presence column 
sampling_complete <- sampling_complete |> 
  mutate(presence = as.integer(sampling_event_identifier %in% sesa_checklists))

table(sampling_complete$presence)

#aggregate to unique locations using locality_id
location_data <- sampling_complete |> 
  group_by(locality_id, latitude, longitude) |> 
  summarise(
    presence = as.integer(any(presence == 1)),
    n_checklists = n(),
    .groups = "drop"
  )

nrow(location_data)
table(location_data$presence)

# filter to top 500 presence locations by number of detections 
top_sites <- location_data |> 
  filter(presence == 1) |>
  arrange(desc(n_checklists)) |>
  slice(1:500)

# 1000 absence locations 
set.seed(42)
sampled_absences <- location_data |>
  filter(presence == 0) |>
  sample_n(1000)

final_locations <- bind_rows(top_sites, sampled_absences)

nrow(final_locations)
table(final_locations$presence)

#convert to sf object 
final_locations_sf <- st_as_sf(final_locations,
                               coords = c("longitude", "latitude"),
                               crs = 4326)

saveRDS(final_locations_sf, file.path(project_root, "data", "final_locations_sf.rds"))

nrow(final_locations_sf)
colnames(final_locations_sf)
