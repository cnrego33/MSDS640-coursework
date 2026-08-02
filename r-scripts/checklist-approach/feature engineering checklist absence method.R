#=======================================================================
# Feature ENgineering Checklist based approach (instead of pseudo-absence)
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
#load in data 
project_root <- r"(C:\Users\cnreg\iCloudDrive\Summer 2026\Capstone\Project Data)"
final_locations_sf <- readRDS(file.path(project_root, "data", "final_locations_sf.rds"))

nrow(final_locations_sf)
colnames(final_locations_sf)


#elevation data
elev <- get_elev_point(final_locations_sf, src = "aws", z=10)
final_locations_sf$elevation <- elev$elevation

summary(final_locations_sf$elevation)

sum(final_locations_sf$elevation < -100)


#distance to coast line 
coastline <- ne_coastline(scale = "medium", returnclass = "sf")
coastline <- st_transform(coastline, st_crs(final_locations_sf))
final_locations_sf$dist_from_coast <- as.numeric(st_distance(final_locations_sf, st_union(coastline))) / 1000

summary(final_locations_sf$dist_from_coast)


#weather data
coords <- st_coordinates(final_locations_sf)
weather_results <- list()

for  (i in 1:nrow(final_locations_sf)) { 
  tryCatch({
    w <- get_power(community = "ag",
                   lonlat = c(coords[i, 1], coords[i, 2]),
                   pars = c("WS2M", "T2M", "PRECTOTCORR"),
                   dates = c("2001-05-01", "2024-09-30"),
                   temporal_api = "daily")
    w_migration <- w |> filter(MM %in% c(5, 6, 7, 8, 9))
    weather_results[[i]] <- data.frame(
      mean_wind_speed = mean(w_migration$WS2M, na.rm = TRUE),
      mean_temp = mean(w_migration$T2M, na.rm = TRUE),
      mean_precip = mean(w_migration$PRECTOTCORR, na.rm = TRUE))
    
  }, error = function(e) {
    weather_results[[i]] <<- data.frame(mean_wind_speed = NA, mean_temp = NA, mean_precip = NA)
  })
  if (i %% 10 == 0) message(paste("Processed", i, "of", nrow(final_locations_sf), "locations"))
}

weather_df <- bind_rows(weather_results)
final_locations_sf <- cbind(final_locations_sf, weather_df)


summary(final_locations_sf$mean_wind_speed)
summary(final_locations_sf$mean_temp)
summary(final_locations_sf$mean_precip)
sum(is.na(final_locations_sf$mean_wind_speed))


#land cover data 
library(terra)

# load NLCD rasters
nlcd_2001 <- rast(file.path(project_root, "land cover data", "Annual_NLCD_LndCov_2001_CU_C1V2", "Annual_NLCD_LndCov_2001_CU_C1V2.tif"))
nlcd_2025 <- rast(file.path(project_root, "land cover data", "Annual_NLCD_LndCov_2025_CU_C1V2", "Annual_NLCD_LndCov_2025_CU_C1V2.tif"))

# convert sf to terra vect and reproject
locations_vector <- vect(final_locations_sf)
locations_nlcd <- project(locations_vector, crs(nlcd_2001))

# extract values
nlcd_values_2001 <- extract(nlcd_2001, locations_nlcd)
nlcd_values_2025 <- extract(nlcd_2025, locations_nlcd)

# add to sf object
final_locations_sf$nlcd_2001 <- nlcd_values_2001[, 2]
final_locations_sf$nlcd_2025 <- nlcd_values_2025[, 2]

table(final_locations_sf$nlcd_2001)
table(final_locations_sf$nlcd_2025)

#create land change detection columns 
final_locations_sf <- final_locations_sf |> 
  mutate(
    land_cover_changed = as.integer(nlcd_2001 != nlcd_2025),
    habitat_degraded = as.integer(land_cover_changed == 1 & nlcd_2025 %in% c("Open Water", "Developed, Open Space", "Developed, Low Intensity", "Developed, Medium Intensity", "Developed, High Intensity")),
    habitat_improved = as.integer(land_cover_changed == 1 & nlcd_2025 %in% c("Woody Wetlands", "Emergent Herbaceous Wetlands", "Barren Land"))
  )

#drop nlcd_2001 because it is not needed after making the change columns 
final_locations_sf <- final_locations_sf |> select(-nlcd_2001)

#=======================================================================
#OHE 
final_locations_df <- st_drop_geometry(final_locations_sf)
final_locations_ohe <- dummy_cols(final_locations_df,
                                  select_columns = "nlcd_2025",
                                  remove_first_dummy = TRUE,
                                  remove_selected_columns = TRUE)

#clean up columns and names 
final_locations_ohe <- final_locations_ohe |> select(-'nlcd_2025_Perennial Ice/Snow')

colnames(final_locations_ohe) <- make.names(colnames(final_locations_ohe))
colnames(final_locations_ohe)

final_locations_ohe <- final_locations_ohe |> select(-nlcd_2025_NA)

#identify na
colSums(is.na(final_locations_ohe))

#drop na because there are only 5 rows 
final_locations_ohe <- final_locations_ohe |> drop_na()

nrow(final_locations_ohe)
table(final_locations_ohe$presence)

#=======================================================================
#save and push to database 

saveRDS(final_locations_ohe, file.path(project_root, "data", "final_locations_ohe.rds"))

con <- dbConnect(RPostgres::Postgres(),
                 dbname = "SESA Project",
                 host = "localhost",
                 port = 5432,
                 user = "postgres", 
                 password = "sesaproject")

dbWriteTable(con, "final_locations_ohe", final_locations_ohe, overwrite = TRUE)
dbListTables(con)
dbDisconnect(con)
