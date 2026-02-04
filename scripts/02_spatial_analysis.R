# ==============================
# 03_spatial_analysis_kriging_one_month.R
# Ordinary Kriging for one specific month
# ==============================

library(dplyr)
library(sf)
library(terra)
library(gstat)
library(ggplot2)
library(stars)

# ----------------------------
# 0. Set working directory
# ----------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ----------------------------
# 1. Load data
# ----------------------------
stations <- st_read("../data/raw/EPD_Current_AQHI/EPD_Current_AQHI.shp")
districts <- st_read("../data/raw/boundaries/hk_boundaries.json") %>%
  st_transform(st_crs(stations))

aq_long <- readRDS("../data/processed/AQHI_hourly_long_all_years_sf.rds")
aq_long
# ----------------------------
# 2. Loop through years and months
# ----------------------------
years <- 2021:2025
months <- c("Jan", "Feb", "Mrz", "Apr", "Mai", "Jun", 
            "Jul", "Aug", "Sep", "Okt", "Nov", "Dez")

# Create base raster grid from HK bounding box (once, outside loop)
res <- 500  # resolution in meters
hk_grid_template <- st_as_stars(st_bbox(districts), dx = res, dy = res)

# Convert raster cell centers to points for Kriging (once, outside loop)
grid_pts <- st_coordinates(hk_grid_template) %>% as.data.frame()
names(grid_pts) <- c("x", "y")
grid_coords <- st_as_sf(grid_pts, coords = c("x", "y"), crs = st_crs(districts))

for(year_sel in years) {
  for(month_sel in months) {
    
    cat(sprintf("Processing %s %d...\n", month_sel, year_sel))
    
    # ----------------------------
    # 3. Filter month & aggregate to monthly mean
    # ----------------------------
    aq_month <- aq_long %>%
      filter(year == year_sel, month == month_sel) %>%
      group_by(station_name) %>%
      summarise(
        AQHI = mean(AQHI, na.rm = TRUE),
        geometry = st_union(geometry),
        .groups = "drop"
      ) %>%
      st_as_sf()
    
    # Skip if no data for this month
    if(nrow(aq_month) == 0) {
      cat(sprintf("  No data for %s %d, skipping...\n", month_sel, year_sel))
      next
    }
    
    # ----------------------------
    # 4. Compute variogram (robust)
    # ----------------------------
    if(nrow(aq_month) < 2 || var(aq_month$AQHI, na.rm = TRUE) == 0){
      vgm_fit <- vgm(psill = 0.01, model = "Sph", range = 10000, nugget = 0.01)
    } else {
      vgm_emp <- variogram(AQHI ~ 1, aq_month)
      vgm_start <- vgm(psill = var(aq_month$AQHI), model = "Sph", range = 10000, nugget = 0.01)
      vgm_fit <- tryCatch(
        fit.variogram(vgm_emp, vgm_start, fit.method = 1),
        error = function(e){
          vgm(psill = var(aq_month$AQHI), model = "Sph", range = 10000, nugget = 0.01)
        }
      )
    }
    
    # ----------------------------
    # 5a. Ordinary Kriging
    # ----------------------------
    hk_grid_kriging <- hk_grid_template
    hk_grid_kriging$AQHI <- NA_real_
    
    if(nrow(aq_month) < 2){
      # Only one point → fill raster with constant value
      hk_grid_kriging[[1]] <- array(aq_month$AQHI[1], dim = dim(hk_grid_kriging[[1]]))
    } else {
      kr <- krige(AQHI ~ 1, aq_month, grid_coords, model = vgm_fit)
      
      # Fill stars raster
      hk_grid_kriging[[1]] <- matrix(kr$var1.pred, nrow = nrow(hk_grid_kriging[[1]]), ncol = ncol(hk_grid_kriging[[1]]), byrow = FALSE)
    }
    
    names(hk_grid_kriging) <- paste0("AQHI_", year_sel, "_", month_sel)
    
    # ----------------------------
    # 5b. Inverse Distance Weighting (IDW)
    # ----------------------------
    hk_grid_idw <- hk_grid_template
    hk_grid_idw$AQHI <- NA_real_
    
    if(nrow(aq_month) < 2){
      # Only one point → fill raster with constant value
      hk_grid_idw[[1]] <- array(aq_month$AQHI[1], dim = dim(hk_grid_idw[[1]]))
    } else {
      idw_result <- idw(AQHI ~ 1, aq_month, grid_coords, idp = 2.0)
      
      # Fill stars raster
      hk_grid_idw[[1]] <- matrix(idw_result$var1.pred, nrow = nrow(hk_grid_idw[[1]]), ncol = ncol(hk_grid_idw[[1]]), byrow = FALSE)
    }
    
    names(hk_grid_idw) <- paste0("AQHI_", year_sel, "_", month_sel)
    
    # ----------------------------
    # 6. Save rasters
    # ----------------------------
    # Save Kriging
    write_stars(
      hk_grid_kriging,
      paste0("../data/processed/AQHI_kriging_", year_sel, "_", month_sel, ".tif"),
      options = "COMPRESS=LZW"
    )
    
    # Save IDW
    write_stars(
      hk_grid_idw,
      paste0("../data/processed/AQHI_idw_", year_sel, "_", month_sel, ".tif"),
      options = "COMPRESS=LZW"
    )
    
    cat(sprintf("  Saved: AQHI_kriging_%d_%s.tif & AQHI_idw_%d_%s.tif\n", year_sel, month_sel, year_sel, month_sel))
  }
}

cat("\nKriging and IDW complete for all years and months!\n")

# ----------------------------
# 7. Annual averages (per year)
# ----------------------------
cat("\nProcessing annual averages...\n")

for(year_sel in years) {
  
  cat(sprintf("Processing year %d...\n", year_sel))
  
  # Aggregate to yearly mean
  aq_year <- aq_long %>%
    filter(year == year_sel) %>%
    group_by(station_name) %>%
    summarise(
      AQHI = mean(AQHI, na.rm = TRUE),
      geometry = st_union(geometry),
      .groups = "drop"
    ) %>%
    st_as_sf()
  
  # Skip if no data for this year
  if(nrow(aq_year) == 0) {
    cat(sprintf("  No data for year %d, skipping...\n", year_sel))
    next
  }
  
  # Compute variogram
  if(nrow(aq_year) < 2 || var(aq_year$AQHI, na.rm = TRUE) == 0){
    vgm_fit <- vgm(psill = 0.01, model = "Sph", range = 10000, nugget = 0.01)
  } else {
    vgm_emp <- variogram(AQHI ~ 1, aq_year)
    vgm_start <- vgm(psill = var(aq_year$AQHI), model = "Sph", range = 10000, nugget = 0.01)
    vgm_fit <- tryCatch(
      fit.variogram(vgm_emp, vgm_start, fit.method = 1),
      error = function(e){
        vgm(psill = var(aq_year$AQHI), model = "Sph", range = 10000, nugget = 0.01)
      }
    )
  }
  
  # Kriging
  hk_grid_kriging <- hk_grid_template
  hk_grid_kriging$AQHI <- NA_real_
  
  if(nrow(aq_year) < 2){
    hk_grid_kriging[[1]] <- array(aq_year$AQHI[1], dim = dim(hk_grid_kriging[[1]]))
  } else {
    kr <- krige(AQHI ~ 1, aq_year, grid_coords, model = vgm_fit)
    hk_grid_kriging[[1]] <- matrix(kr$var1.pred, nrow = nrow(hk_grid_kriging[[1]]), ncol = ncol(hk_grid_kriging[[1]]), byrow = FALSE)
  }
  
  names(hk_grid_kriging) <- paste0("AQHI_", year_sel, "_annual")
  
  # IDW
  hk_grid_idw <- hk_grid_template
  hk_grid_idw$AQHI <- NA_real_
  
  if(nrow(aq_year) < 2){
    hk_grid_idw[[1]] <- array(aq_year$AQHI[1], dim = dim(hk_grid_idw[[1]]))
  } else {
    idw_result <- idw(AQHI ~ 1, aq_year, grid_coords, idp = 2.0)
    hk_grid_idw[[1]] <- matrix(idw_result$var1.pred, nrow = nrow(hk_grid_idw[[1]]), ncol = ncol(hk_grid_idw[[1]]), byrow = FALSE)
  }
  
  names(hk_grid_idw) <- paste0("AQHI_", year_sel, "_annual")
  
  # Save rasters
  write_stars(
    hk_grid_kriging,
    paste0("../data/processed/AQHI_kriging_", year_sel, "_annual.tif"),
    options = "COMPRESS=LZW"
  )
  
  write_stars(
    hk_grid_idw,
    paste0("../data/processed/AQHI_idw_", year_sel, "_annual.tif"),
    options = "COMPRESS=LZW"
  )
  
  cat(sprintf("  Saved: AQHI_kriging_%d_annual.tif & AQHI_idw_%d_annual.tif\n", year_sel, year_sel))
}

cat("\nAnnual averages complete!\n")

# ----------------------------
# 8. Hourly averages (across all years)
# ----------------------------
cat("\nProcessing hourly averages across all years...\n")

for(hour_sel in 1:24) {
  
  cat(sprintf("Processing hour %02d...\n", hour_sel))
  
  # Aggregate to hourly mean across all years
  aq_hour <- aq_long %>%
    filter(Hour == hour_sel) %>%
    group_by(station_name) %>%
    summarise(
      AQHI = mean(AQHI, na.rm = TRUE),
      geometry = st_union(geometry),
      .groups = "drop"
    ) %>%
    st_as_sf()
  
  # Skip if no data for this hour
  if(nrow(aq_hour) == 0) {
    cat(sprintf("  No data for hour %02d, skipping...\n", hour_sel))
    next
  }
  
  # Compute variogram
  if(nrow(aq_hour) < 2 || var(aq_hour$AQHI, na.rm = TRUE) == 0){
    vgm_fit <- vgm(psill = 0.01, model = "Sph", range = 10000, nugget = 0.01)
  } else {
    vgm_emp <- variogram(AQHI ~ 1, aq_hour)
    vgm_start <- vgm(psill = var(aq_hour$AQHI), model = "Sph", range = 10000, nugget = 0.01)
    vgm_fit <- tryCatch(
      fit.variogram(vgm_emp, vgm_start, fit.method = 1),
      error = function(e){
        vgm(psill = var(aq_hour$AQHI), model = "Sph", range = 10000, nugget = 0.01)
      }
    )
  }
  
  # Kriging
  hk_grid_kriging <- hk_grid_template
  hk_grid_kriging$AQHI <- NA_real_
  
  if(nrow(aq_hour) < 2){
    hk_grid_kriging[[1]] <- array(aq_hour$AQHI[1], dim = dim(hk_grid_kriging[[1]]))
  } else {
    kr <- krige(AQHI ~ 1, aq_hour, grid_coords, model = vgm_fit)
    hk_grid_kriging[[1]] <- matrix(kr$var1.pred, nrow = nrow(hk_grid_kriging[[1]]), ncol = ncol(hk_grid_kriging[[1]]), byrow = FALSE)
  }
  
  names(hk_grid_kriging) <- paste0("AQHI_hour_", sprintf("%02d", hour_sel))
  
  # IDW
  hk_grid_idw <- hk_grid_template
  hk_grid_idw$AQHI <- NA_real_
  
  if(nrow(aq_hour) < 2){
    hk_grid_idw[[1]] <- array(aq_hour$AQHI[1], dim = dim(hk_grid_idw[[1]]))
  } else {
    idw_result <- idw(AQHI ~ 1, aq_hour, grid_coords, idp = 2.0)
    hk_grid_idw[[1]] <- matrix(idw_result$var1.pred, nrow = nrow(hk_grid_idw[[1]]), ncol = ncol(hk_grid_idw[[1]]), byrow = FALSE)
  }
  
  names(hk_grid_idw) <- paste0("AQHI_hour_", sprintf("%02d", hour_sel))
  
  # Save rasters
  write_stars(
    hk_grid_kriging,
    paste0("../data/processed/AQHI_kriging_hour_", sprintf("%02d", hour_sel), ".tif"),
    options = "COMPRESS=LZW"
  )
  
  write_stars(
    hk_grid_idw,
    paste0("../data/processed/AQHI_idw_hour_", sprintf("%02d", hour_sel), ".tif"),
    options = "COMPRESS=LZW"
  )
  
  cat(sprintf("  Saved: AQHI_kriging_hour_%02d.tif & AQHI_idw_hour_%02d.tif\n", hour_sel, hour_sel))
}

cat("\nHourly averages complete!\n")
cat("\n=== ALL SPATIAL ANALYSIS COMPLETE ===\n")

