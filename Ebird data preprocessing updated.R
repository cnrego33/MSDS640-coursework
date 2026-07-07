#==========================================================
#EBIRD Import - Semipalmated Sandpiper (SESA) Migration Stopover Site Analysis
# import, filter, clean ebird data

#==========================================================
# Definitions 
# auk - an R package built specifically for working with eBird data
      # provides functions such as auk_state(), auk_date(), and auk_filter() - lets you subset data before reading it into R
# awk - text-processing tool that runs at the command line and reads through large text files efficiently
# auk is the R interface and awk is the engine that does the actual filtering 
# tidyverse - data manipulation, cleaning, visualization
# sf - "simple features" - spatial vector data (plots, lines, polygons), convert ebird coordinates into spatial objects
# lubridate - used for dates and times 

#==========================================================
# Load in packages 

library (auk)
library(tidyverse)
library(sf)
library(lubridate)
library(knitr)
library (rnaturalearth)

#==========================================================
# 1. Set file paths 

# set project root 
project_root <- r"(C:\Users\cnreg\iCloudDrive\Summer 2026\Capstone\Project Data)"

# EBird files required by auk

# observation file (one row per species detected per checklist)
ebd_file <- file.path(project_root, 
                      "ebd_US_semsan_smp_relMay-2026",
                      "ebd_US_semsan_smp_relMay-2026.txt")

# sampling file (one row per checklist, used to generate psuedo-absences)
sampling_file <- file.path(project_root, 
                           "ebd_US_semsan_smp_relMay-2026",
                           "ebd_US_semsan_smp_relMay-2026_sampling.txt")

# output filtered files (auk writes these before reading into R)
ebd_filtered <- file.path(project_root, "data", "ebd_SESA_filtered.txt")
sampling_filtered <- file.path(project_root, "data", "ebd_sampling_filtered.txt")


# create data output folder
dir.create(file.path(project_root, "data"), showWarnings = FALSE)

# confirm the input files I created do in fact exist 
stopifnot(
  "EBD file not found - check your file path" = file.exists(ebd_file),
  "Sampling file not found - check your file path" = file.exists(sampling_file)
)
message("Input files found. Starting auk filtering...")

#==========================================================
# 2. AWK Filtering 

auk_set_awk_path("C:/rtools45/usr/bin/awk.exe", overwrite = TRUE)

# auk uses AWK to efficiently filter the large EBD file before reafing it into R 

if (!file.exists(ebd_filtered)) {
  
  auk_ebd(ebd_file, file_sampling = sampling_file) |>
    
    # States: VA to ME 
    auk_state(c("US-VA", "US-MD", "US-DE", "US-NJ", "US-NY", "US-CT", "US-RI", "US-MA", "US-NH", "US-ME")) |>
    
    # Migration window: Apr - Oct (split into spring and fall at a later time)
    auk_date(date = c("*-04-01", "*-10-31")) |> 
    
    
    # Run filter, write output files 
    auk_filter(
      file = ebd_filtered, 
      file_sampling = sampling_filtered, 
      overwrite = TRUE
    )
  message("Filtering complete.")
    
} else {
  message("Filtered files already exist - skipping AWK filtering step. Delete data/ebd_SESA_filtered.txt to re-run.")
}


#==========================================================
# 3. Read AWK filtered data into R 

ebd <- read_ebd(ebd_filtered)
nrow(ebd)


#==========================================================
# 4. Clean and prep data
names(ebd)

ebd_clean <- ebd |> 
  
  # split date, keep useful time variables 
  mutate( 
    observation_date = as.Date(observation_date),
    year = year(observation_date),
    month = month(observation_date),
    
      
    # determine migration season
     season = case_when(
       month %in% c(4,5)     ~ "spring",
       month %in% c(8,9,10)  ~ "fall",
       TRUE                  ~ NA_character_
      )
    ) |>
  # drop June and July because they are not migration months for SESA
  filter(!is.na(season)) |> 
  
  # filter to data from 2001-2025
  filter(year >= 2001) |>
  filter(year <= 2025) |>
  
  # handle "X" counts (present but not counted")
  mutate(
    observation_count_clean = suppressWarnings(as.numeric(observation_count)), 
    presence = 1L # all remaining rows are detections
  ) |>
  

  # keep useful columns for analysis 
  select(
    checklist_id,
    observation_date,
    year,
    month,
    season,
    state,
    latitude,
    longitude,
    observation_count,
    observation_count_clean,
    presence,
    duration_minutes,
    effort_distance_km,
    number_observers,
    protocol_name,
    observation_type,
    locality_id
  )


message(paste("Rows after season filtering:", nrow(ebd_clean)))
message(paste("Year range:", min(ebd_clean$year), "to", max(ebd_clean$year)))

head(ebd_clean)
#==========================================================
# 5. Convert to spatial object 

ebd_sf <- ebd_clean |>
  filter(!is.na(latitude), !is.na(longitude)) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs=4326)

message(paste("Spatial points created:", nrow(ebd_sf)))

head(ebd_sf)
#==========================================================
# 6. Initial data summary & visuals

message("n-- Detection summary -------------------")
message(paste("Total detections:", nrow(ebd_clean)))

# By state 
state_summary <- ebd_clean |> count(state, season) |> arrange(season, desc(n))
print(state_summary)

# By year 
year_summary <- ebd_clean |> count(year) |> arrange(year)
print(year_summary)

# By season 
season_summary <- ebd_clean |> count(season)
print(season_summary)


# Summary table 
summary_table <- ebd_clean |> 
  group_by(year) |>
  summarise(
    total_detections = n(), 
    #year_range = paste(min(year), "to", max(year)), 
    n_states = n_distinct(state),
    n_localities = n_distinct(locality_id), 
    spring_detections = sum(season == "spring"), 
    fall_detections = sum(season == "fall")
  ) |> arrange(year)

print(summary_table, n=25, width = Inf)


kable(summary_table |> mutate(year = as.character(year)),
      col.names = c("Year", "Total Detections", "States", 
                    "Localities", "Spring Detections", "Fall Detections"),
      format.args = list(big.mark = ","),
      align = "c")

# Detections by state and season 
state_season <- ebd_clean |> 
  mutate(state_abbr = state.abb[match(state,state.name)]) |>
  count(state_abbr, season)

ggplot(state_season, aes(x = reorder(state_abbr, -n), y = n, fill = season)) + 
  geom_col(position = "dodge") + 
  scale_fill_manual(values = c("spring" = "#4DAF4A", "fall" = "#FF7F00")) +
  scale_y_continuous(labels=function(x) x / 1000) +
  labs( 
    title = "SESA Detections by State and Season",
    x = "State",
    y = "Number of Detections (thousands)",
    fill = "Season") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust=0.5))

# Detections by year 
year_detections <- ebd_clean |> 
  mutate(state_abbr = state.abb[match(state,state.name)]) |>
  count(year, state_abbr)

ggplot(year_detections, aes(x = year, y=n, color = state_abbr)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = function(x) x/1000) + 
  labs(
    title = "SESA Detections by Year",
    x = "Year",
    y = "Number of Detections (thousands)",
    color = "State")+
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
  


# Detections by month Apr-Oct
full_year_month_detections <- ebd |> 
  mutate(
    observation_date=as.Date(observation_date),
    month = month(observation_date),
    year = year(observation_date),
    state_abbr = state.abb[match(state,state.name)]) |>
  filter(year >= 2001, year<=2025) |>
  count(month, state_abbr)

ggplot(full_year_month_detections, aes(x=month, y=n, fill = month %in% c(6,7))) +
  geom_col() + 
  scale_fill_manual(values = c("FALSE" = "#555555", "TRUE" = "#CCCCCC")) +
  scale_x_continuous(breaks = 1:12, 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(labels=function(x) x/1000) +
  labs(
    title = "SESA Detections Per Month (April-October)", 
    x = "Month",
    y = "Number of Detections (thousands)")+
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
  


# Detections by migration months 
migration_month_detections <- ebd_clean |> 
  mutate(state_abbr = state.abb[match(state,state.name)]) |>
  count(month, state_abbr)

ggplot(migration_month_detections, aes(x=month, y=n)) + 
  geom_col(fill="#555555") +
  scale_x_continuous(breaks = 1:12, 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(labels=function(x) x/1000) + 
  labs(
    title = "SESA Detections During Migration Months",
    x="Month",
    y="Number of Detections (thousands)")+
  theme_minimal() + 
  theme(plot.title = element_text(hjust=0.5))
  
  
#==========================================================
# 7. Initial plot
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")
# install.packages("pak")
# install.packages("rnaturalearthhires", repos = "https://ropensci.r-universe.dev")
coast <- ne_states(country = "united states of america", returnclass="sf") |>
  filter(postal %in% c("VA", "MD", "DE", "NJ", "NY", "CT", "RI", "MA", "NH", "ME"))

# dev.off()

ggplot() +
  geom_sf(data = coast, fill = "grey90", color = "grey60") + 
  geom_sf(data = ebd_sf, aes(color=season), size=0.5, alpha = 0.4) +
  scale_color_manual(values = c("spring" = "#4DAF4A", "fall" = "#FF7F00")) + 
  labs(
    title = "Semipalmated Sandpiper Detections - US Atlantic Coast", 
    #subtitle = paste0("n = ", nrow(ebd_sf), " detections | eBird EBD May 2026"),
    color = "Season")+
    #caption = "Source: eBird Basic Dataset, Cornell Lab of Ornithology")+ 
  theme_minimal()

# save plot as png
ggsave(file.path(project_root, "data", "1_SESA_Detections_Map.png"),
       width = 8, height = 10, dpi = 300)


#==========================================================
# 8. Save Cleaned Data 
saveRDS(ebd_clean, file.path(project_root, "data", "ebd_clean.rds"))
saveRDS(ebd_sf, file.path(project_root, "data", "ebd_sf.rds"))
