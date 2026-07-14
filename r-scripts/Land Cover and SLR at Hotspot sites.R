#=======================================================================
# Extract NLCD land cover and SLR values at hotspot sites (locality detection frequency) 
# Semipalmated Sandpiper (SESA) Migration Stopover Site Analysis

#=======================================================================
# Load in packages 

library(tidyverse)
library(sf)
library(terra)

#=======================================================================
# 1. Set file paths and load data

# set project root 
project_root <- r"(C:\Users\cnreg\iCloudDrive\Summer 2026\Capstone\Project Data)"

# load spatial data .rds file saved from previous notebook 
hotspot_sf <- readRDS(file.path(project_root, "data", "hotspot_sf.rds"))


# convert to terra-compatible vector object 
hotspot_vector <- vect(hotspot_sf)


#=======================================================================
# 2. NLCD Land Cover 
# NLCD uses Albers Equal Area (EPSG:5070) so I need to reproject the points to match 

nlcd_2001 <- rast(file.path(project_root, 
                            "land cover data", 
                            "Annual_NLCD_LndCov_2001_CU_C1V2", 
                            "Annual_NLCD_LndCov_2001_CU_C1V2.tif"))

nlcd_2025 <- rast(file.path(project_root, 
                            "land cover data", 
                            "Annual_NLCD_LndCov_2025_CU_C1V2", 
                            "Annual_NLCD_LndCov_2025_CU_C1V2.tif"))

# reproject hotspot points to match NLCD (only need to do once because both use the same coordinate system, doing for both would be redundant)
hotspot_nlcd <- project(hotspot_vector, crs(nlcd_2001))

# extract these new values 
nlcd_values_2001 <- extract(nlcd_2001, hotspot_nlcd)
nlcd_values_2025 <- extract(nlcd_2025, hotspot_nlcd)



#=======================================================================
# 3. SLR (Sea Level Rise)

slr_05_files <- list.files(file.path(project_root, "slr", "0_5m"), pattern = "\\.tif$", full.names=TRUE)
slr_10_files <- list.files(file.path(project_root, "slr", "1_0m"), pattern = "\\.tif$", full.names=TRUE)

# issues with extracting all tiles so trying to extract each tile separately 
extract_from_tiles <- function(files, points) {
  results <- rep(0L, length(points))
  points_buf <- buffer(points, width = 1000) #1km buffer around each point
  for (f in files) {
    r <- rast(f)
    pts_proj <- project(points_buf, crs(r))
    vals <- extract(r, pts_proj, fun = "max", na.rm = TRUE)[[2]] # fun = "max" - is any pixel within the buffer is inundated (=1) it returns 1
    results[!is.na(vals) & vals == 1] <- 1L
  }
  return(results)
}

#merge to one raster per scenario 
slr_values_05 <- extract_from_tiles(slr_05_files, hotspot_vector)
slr_values_10 <- extract_from_tiles(slr_10_files, hotspot_vector)


#=======================================================================
# 4. Combine into table 

hotspot_sites <- readRDS(file.path(project_root, "data", "hotspot_sf.rds")) |> st_drop_geometry() # drop geometry for tabular storage 

habitat_table <- hotspot_sites |>
  mutate(
    nlcd_2001 = nlcd_values_2001[[2]],
    nlcd_2025 = nlcd_values_2025[[2]],
    slr_05m = slr_values_05[[2]],
    slr_10m = slr_values_10[[2]]
  )

print(head(habitat_table))
message(paste("Rows in habitat table:", nrow(habitat_table)))


#=======================================================================
# 5. Save data 
saveRDS(habitat_table, file.path(project_root, "data", "habitat_table.rds"))


#=======================================================================
# 6. Psuedo-absence site habitat information 

# load int eh pseudo absences 
pseudo_abs <- readRDS(file.path(project_root, "data", "pseudo_abs.rds"))
pseudo_vector <- vect(pseudo_abs)

# project coordinates again 
pseudo_nlcd <- project(pseudo_vector, crs(nlcd_2001))

# extract these new values again 
nlcd_pseudo_2001 <- extract(nlcd_2001, pseudo_nlcd)
nlcd_pseudo_2025 <- extract(nlcd_2025, pseudo_nlcd)

# extract SLR values at these pseudo sites 
slr_pseudo_05 <- extract_from_tiles(slr_05_files, pseudo_vector)
slr_pseudo_10 <- extract_from_tiles(slr_10_files, pseudo_vector)

# create a feature table 
pseudo_nlcd_features <- data.frame(
  locality_id = NA, 
  n_detections = NA, 
  n_years = NA, 
  nlcd_2001 = nlcd_pseudo_2001[[2]],
  nlcd_2025 = nlcd_pseudo_2025[[2]],
  slr_05m = slr_pseudo_05, 
  slr_10m = slr_pseudo_10, 
  presence = 0L
)
head(pseudo_nlcd_features)

# save extracted pseudo features
saveRDS(pseudo_nlcd_features, file.path(project_root, "data", "pseudo_nlcd_features.rds"))
