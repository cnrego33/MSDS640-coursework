#=======================================================================
# Additional data sources to improve models
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
library(elevatr)
library(nasapower)
library(rnaturalearth)
library(rnaturalearthdata)
library(fastDummies)
#=======================================================================
#1. Get data from database

#con <- dbConnect(RPostgres::Postgres(),
#                 dbname = "SESA Project",
#                 host = "localhost",
#                 port = 5432,
#                 user = "postgres", 
#                 password = "sesaproject")

#model_data <- dbGetQuery(con, "SELECT * FROM habitat_model_data")
#dbDisconnect(con)

#colnames(model_data)
#nrow(model_data)

#-----------------------------------------------------------------------
#1.1 get data from data folder 
project_root <- r"(C:\Users\cnreg\iCloudDrive\Summer 2026\Capstone\Project Data)"
hotspot_sf <- readRDS(file.path(project_root, "data", "hotspot_sf.rds"))
pseudo_abs <- readRDS(file.path(project_root, "data", "pseudo_abs.rds"))

nrow(hotspot_sf)
nrow(pseudo_abs)

#=======================================================================
#2. Elevation of hotspots and pseudo-absence sites

hotspot_elev <- get_elev_point(hotspot_sf, src="aws", z=10) 
#get_elev_point() queries the AWS terrain tiles for elevation at each point
#src = "aws uses amazon web services elevation data which is free and reliable 
# z = 10 is the zoom level 
hotspot_sf$elevation <- hotspot_elev$elevation
message(paste("Elevation extracted for", nrow(hotspot_sf), "hotspots."))
summary(hotspot_sf$elevation)

pseudo_elev <- get_elev_point(pseudo_abs, src="aws", z=10) 
#get_elev_point() queries the AWS terrain tiles for elevation at each point
#src = "aws uses amazon web services elevation data which is free and reliable 
# z = 10 is the zoom level 
pseudo_abs$elevation <- pseudo_elev$elevation
message(paste("Elevation extracted for", nrow(pseudo_abs), "pseudo-absences."))
summary(pseudo_abs$elevation)

#remove extreme pseudo absence outliers 
pseudo_abs <- pseudo_abs |> filter( elevation > -100 & elevation < 500)
message(paste("Pseudo-absences after filtering out outliers:", nrow(pseudo_abs)))


#=======================================================================
#3. Distance of site to coast 

coastline <- ne_coastline(scale = "medium", returnclass = "sf") #downloas the US coastline

coastline <- st_transform(coastline, st_crs(hotspot_sf)) # use same coordinate system for both layers

hotspot_sf$dist_from_coast <- as.numeric(st_distance(hotspot_sf, coastline |> st_union())) / 1000
# calculate distance between hotspots and nearest coastline 

summary(hotspot_sf$dist_from_coast)


pseudo_abs$dist_from_coast <- as.numeric(st_distance(pseudo_abs, coastline |> st_union())) / 1000
# calculate distance between hotspots and nearest coastline 

summary(pseudo_abs$dist_from_coast)


#=======================================================================
#4. Weather Data 

#test - query one hotspot
test_query <- get_power(
  community = "ag", # agricultural community dataset, good for surface weather variables 
  lonlat = c(st_coordinates(hotspot_sf)[1,1], st_coordinates(hotspot_sf)[1,2]), 
  pars = c("WS2M", "WD2M", "T2M", "PRECTOTCORR"), #four variables - wind speed, wind direction, temperature, precipitation
  dates = c("2023-05-01", "2023-09-30"),
  temporal_api = "daily"
)

head(test_query)

#loop through all hotspots over all years 
coords <- st_coordinates(hotspot_sf)
weather_results <- list()

for (i in 1:nrow(hotspot_sf)) {
  tryCatch({
    w <- get_power(
      community = "ag", # agricultural community dataset, good for surface weather variables 
      lonlat = c(coords[i,1], coords[i,2]), 
      pars = c("WS2M", "T2M", "PRECTOTCORR"), #three variables - wind speed, temperature, precipitation
      dates = c("2001-04-01", "2024-09-30"),
      temporal_api = "daily"
    )
    
    w_migration <- w |> filter(MM %in% c(4, 5, 6, 7, 8, 9)) #filter for migration months only 
    
    weather_results[[i]] <- data.frame(
      mean_wind_speed = mean(w_migration$WS2M, na.rm = TRUE),
      mean_temp = mean(w_migration$T2M, na.rm = TRUE),
      mean_precip = mean(w_migration$PRECTOTCORR, na.rm = TRUE)
    )
  }, error = function(e) {
    weather_results[[i]] <<- data.frame(mean_wind_speed = NA, mean_temp = NA, mean_precip = NA)
  })
  if (i %% 10 == 0) message(paste("Processed", i, "of", nrow(hotspot_sf), "hotspots"))
}

weather_df <- bind_rows(weather_results)
hotspot_sf <- cbind(hotspot_sf, weather_df)
head(hotspot_sf)


#loop through all pseudo-absences over all years 
coords_pseudo <- st_coordinates(pseudo_abs)
weather_results_pseudo <- list()

for (i in 1:nrow(pseudo_abs)) {
  tryCatch({
    w <- get_power(
      community = "ag", # agricultural community dataset, good for surface weather variables 
      lonlat = c(coords_pseudo[i,1], coords_pseudo[i,2]), 
      pars = c("WS2M", "T2M", "PRECTOTCORR"), #three variables - wind speed, temperature, precipitation
      dates = c("2001-04-01", "2024-09-30"),
      temporal_api = "daily"
    )
    
    w_migration <- w |> filter(MM %in% c(4, 5, 6, 7, 8, 9)) #filter for migration months only 
    
    weather_results_pseudo[[i]] <- data.frame(
      mean_wind_speed = mean(w_migration$WS2M, na.rm = TRUE),
      mean_temp = mean(w_migration$T2M, na.rm = TRUE),
      mean_precip = mean(w_migration$PRECTOTCORR, na.rm = TRUE)
    )
  }, error = function(e) {
    weather_results_pseudo[[i]] <<- data.frame(mean_wind_speed = NA, mean_temp = NA, mean_precip = NA)
  })
  if (i %% 10 == 0) message(paste("Processed", i, "of", nrow(pseudo_abs), "pseudo-absences"))
}

weather_df_pseudo <- bind_rows(weather_results_pseudo)
pseudo_abs <- cbind(pseudo_abs, weather_df_pseudo)
head(pseudo_abs)


pseudo_nlcd <- readRDS(file.path(project_root, "data", "pseudo_nlcd_features.rds"))
colnames(pseudo_nlcd)
nrow(pseudo_nlcd)

#=======================================================================
#5. merge pseudo_abs and pseudo_nlcd

# loaf original pseudo absence file
original_pseudo_abs <- readRDS(file.path(project_root, "data", "pseudo_abs.rds"))


#add coordinates to both files 
pseudo_nlcd$lon <- st_coordinates(original_pseudo_abs)[, 1]
pseudo_nlcd$lat <- st_coordinates(original_pseudo_abs)[, 2]

pseudo_abs$lon <- st_coordinates(pseudo_abs)[, 1]
pseudo_abs$lat <- st_coordinates(pseudo_abs)[, 2]

#join on the coordinates 
pseudo_combined <- left_join(
  st_drop_geometry(pseudo_abs),
  pseudo_nlcd, 
  by = c("lon", "lat"))

nrow(pseudo_combined)
colnames(pseudo_combined)

#drop column and rename column
pseudo_combined <- pseudo_combined |> select(-presence.y) |> rename(presence = presence.x)
colnames(pseudo_combined)


#same for hotspot_sf
hotspot_sf$lon <- st_coordinates(hotspot_sf)[, 1]
hotspot_sf$lat <- st_coordinates(hotspot_sf)[, 2]

hotspot_combined <- st_drop_geometry(hotspot_sf)
habitat_table <- readRDS(file.path(project_root, "data", "habitat_table.rds"))

hotspot_combined <- left_join(hotspot_combined, habitat_table, by = "locality_id")

hotspot_combined$presence <- 1

colnames(hotspot_combined)
nrow(hotspot_combined)

#drop duplicate columns and rename columns 
hotspot_combined <- hotspot_combined |> 
  select(-n_detections.y, -n_years.y, -seasons.y) |>
  rename(n_detections = n_detections.x, n_years = n_years.x, seasons = seasons.x)

colnames(hotspot_combined)


updated_model_data <- bind_rows(hotspot_combined, pseudo_combined)

nrow(updated_model_data)
colnames(updated_model_data)
table(updated_model_data$presence)


#=======================================================================
#6. OHE
sort(unique(updated_model_data$nlcd_2001))
sort(unique(updated_model_data$nlcd_2025))

updated_model_data <- updated_model_data |>
  mutate(
    land_cover_changed = as.integer(nlcd_2001 != nlcd_2025),
    habitat_degraded = as.integer(land_cover_changed == 1 & nlcd_2025 %in% c("Open Water", "Developed, Open Space", "Developed, Low Intensity", "Developed, Medium Intensity", "Developed, High Intensity")),
    habitat_improved = as.integer(land_cover_changed == 1 & nlcd_2025 %in% c("Woody Wetlands", "Emergent Herbaceous Wetlands", "Barren Land"))
  )

updated_model_data <- updated_model_data |> select(-nlcd_2001)

model_data_v2_OHE <- dummy_cols(updated_model_data, 
                                select_columns = c("nlcd_2025"),
                                remove_first_dummy = TRUE, 
                                remove_selected_columns = TRUE)

colnames(model_data_v2_OHE)

model_data_v2_OHE <- model_data_v2_OHE |> select(-'nlcd_2025_Perennial Ice/Snow', -'nlcd_2025_NA')

colnames(model_data_v2_OHE) <- make.names(colnames(model_data_v2_OHE))
#=======================================================================
#7. save updated model data 

con <- dbConnect(RPostgres::Postgres(),
                 dbname = "SESA Project",
                 host = "localhost",
                 port = 5432,
                 user = "postgres", 
                 password = "sesaproject")

saveRDS(updated_model_data, file.path(project_root, "data", "habitat_model_data_v2.rds"))
dbWriteTable(con, "habitat_model_data_v2", updated_model_data, overwrite = TRUE)

saveRDS(model_data_v2_OHE, file.path(project_root, "data", "habitat_model_data_v2_OHE.rds"))
dbWriteTable(con, "habitat_model_data_v2_OHE", model_data_v2_OHE, overwrite = TRUE)

dbDisconnect(con)
