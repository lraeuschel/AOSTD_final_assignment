# ============================================================
# 01_data_import_cleaning.R
# Import and preprocessing of air quality and spatial data
# ============================================================

# ----------------------------
# 0. Libraries
# ----------------------------
library(dplyr)
library(sf)
library(lubridate)
library(readr)
library(tidyr)
library(ggplot2)

# ----------------------------
# 1. Spatial data
# ----------------------------
stations <- st_read("data/raw/EPD_Current_AQHI/EPD_Current_AQHI.shp")
districts <- st_read("data/raw/boundaries/hk_boundaries.json")
districts <- st_transform(districts, st_crs(stations))

# ----------------------------
# 2. Prepare year list
# ----------------------------
years <- 2021:2025

# Create a list to store all years' data
all_years_list <- list()

for (yr in years) {
  
  message("Processing year: ", yr)
  
  # ----------------------------
  # 2a. List CSV files for that year
  # ----------------------------
  aq_files <- list.files(
    path = paste0("data/raw/AQHI Data/", yr, "/"),
    pattern = "\\d{6}_Eng\\.csv",  
    full.names = TRUE
  )
  
  if(length(aq_files) == 0) next  # skip year if no files
  
  # ----------------------------
  # 2b. Read CSVs as characters to avoid bind_rows errors
  # ----------------------------
  aq_raw <- lapply(aq_files, function(f) {
    df <- read_csv(f, skip = 7, col_types = cols(.default = "c"), show_col_types = FALSE)
    # Clean column names: remove leading/trailing spaces
    colnames(df) <- trimws(colnames(df))
    df
  }) %>% bind_rows()
  
  # ----------------------------
  # 2c. Clean & format
  # ----------------------------
  aq_clean <- aq_raw %>%
    tidyr::fill(Date) %>%                   # fill Date downward
    mutate(across(-c(Date, Hour), ~ gsub("\\*", "", .x))) %>%  # remove asterisks
    mutate(across(-c(Date, Hour), ~ as.numeric(.x))) %>%        # convert to numeric
    mutate(
      Hour = as.integer(Hour),
      datetime = lubridate::ymd(Date) + lubridate::hours(Hour - 1),
      date = as.Date(datetime),
      year = lubridate::year(datetime),
      month = lubridate::month(datetime, label = TRUE)
    )
  
  # ----------------------------
  # 2d. Transform to long format
  # ----------------------------
  aq_long <- aq_clean %>%
    pivot_longer(
      cols = -c(Date, Hour, datetime, date, year, month),
      names_to = "station_name",
      values_to = "AQHI"
    )
  
  # ----------------------------
  # 2e. Add spatial info
  # ----------------------------
  aq_sf <- aq_long %>%
    left_join(
      stations %>% select(Name, geometry),
      by = c("station_name" = "Name")
    ) %>%
    st_as_sf(sf_column_name = "geometry", crs = st_crs(stations))
  
  # ----------------------------
  # 2f. Append to all_years_list
  # ----------------------------
  all_years_list[[as.character(yr)]] <- aq_sf
}

# ----------------------------
# 3. Combine all years into one sf object
# ----------------------------
aq_all_years_sf <- bind_rows(all_years_list)

# ----------------------------
# 4. Save combined dataset
# ----------------------------
saveRDS(aq_all_years_sf, "data/processed/AQHI_hourly_long_all_years_sf.rds")


aq_all_years_sf %>%
  st_drop_geometry() %>%   # drop geometry for faster computation
  group_by(year) %>%
  summarise(
    total = n(),
    na_count = sum(is.na(AQHI)),
    na_percent = 100 * na_count / total
  )
