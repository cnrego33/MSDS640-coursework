#=======================================================================
# Identify Stopover Hotspots (locality detection frequency) 
# Semipalmated Sandpiper (SESA) Migration Stopover Site Analysis

#=======================================================================
# Load in packages 

library(tidyverse)
library(sf)
library(rnaturalearth)

#=======================================================================
# 1. Set file paths 

# set project root 
project_root <- r"(C:\Users\cnreg\iCloudDrive\Summer 2026\Capstone\Project Data)"

# load spatial data .rds file saved from previous notebook 
ebd_clean <- readRDS(file.path(project_root, "data", "ebd_clean.rds"))
ebd_sf <- readRDS(file.path(project_root, "data", "ebd_sf.rds"))


#=======================================================================
# 2 Define Hotspots 

# each eBird locality_is is a named survey site with fixed coordinates - rank these by total SESA detection count and take top 200 
# Kelling et al for literature reference - use occurrence frequency at named sites as valid hotspot detection method 

hotspot_sites <- ebd_clean |> 
  group_by(locality_id, latitude, longitude) |>
  summarise(
    n_detections = n(),
    n_years = n_distinct(year),
    seasons = paste(sort(unique(season)), collapse = "/"),
    .groups = "drop"
  ) |> 
  arrange(desc(n_detections)) |>
  slice_head(n=200)

message(paste("Hotspot sites identified:", nrow(hotspot_sites)))
print(head(hotspot_sites, 10))


# Convert to a spatial object
hotspot_sf <- hotspot_sites |>
  filter(!is.na(latitude), !is.na(longitude)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs=4326)


#=======================================================================
# 3. Generating Pseudo-Absences 

set.seed(42)
study_bbox <- st_bbox(ebd_sf) |> st_as_sfc()

pseudo_abs <- st_sample(study_bbox, size = nrow(hotspot_sf)) |> 
  st_as_sf() |>
  mutate(presence = 0L)

message(paste("Pseudo_absences generated:", nrow(pseudo_abs)))


#=======================================================================
# 4. Visual 

coast <- ne_states(country = "united states of america", returnclass = "sf") |>
  filter(postal %in% c("VA", "MD", "DE", "NJ", "NY", "CT", "RI", "MA", "NH", "ME"))

ggplot() +
  geom_sf(data = coast, fill = "grey90", color = "grey60") +
  geom_sf(data = pseudo_abs, fill = "grey50", size = 1, alpha = 0.5) + 
  geom_sf(data = hotspot_sf, aes(size = n_detections), color = "#E84855", alpha = 0.7) +
  scale_size_continuous(range = c(1,6)) + 
  labs(
    title = "Top 200 SESA Stopover Hotspots - US Atlantic Coast", 
    subtitle = "Point size = # detections | Grey points represents pseudo-absences",
    size = "Detections"
  ) + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.title.position = "plot")

# save plot and outputs 
ggsave(file.path(project_root, "data", "hotspot_map.png"), width=8, height=10, dpi=300)

saveRDS(hotspot_sf, file.path(project_root, "data", "hotspot_sf.rds"))
saveRDS(pseudo_abs, file.path(project_root, "data", "pseudo_abs.rds"))
