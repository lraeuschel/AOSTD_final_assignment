# ==============================
# Spatial Analysis AQHI
# ==============================

library(dplyr)
library(sf)
library(gstat)
library(stars)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
      
base_path <- "../data/processed/spatial_analysis"

path_kriging_month        <- file.path(base_path, "kriging_fit", "month")
path_kriging_manual_month <- file.path(base_path, "kriging_manual", "month")
path_idw_month            <- file.path(base_path, "idw", "month")

path_kriging_annual       <- file.path(base_path, "kriging_fit", "annual")
path_kriging_manual_annual<- file.path(base_path, "kriging_manual", "annual")
path_idw_annual           <- file.path(base_path, "idw", "annual")

path_kriging_hour         <- file.path(base_path, "kriging_fit", "hour")
path_kriging_manual_hour  <- file.path(base_path, "kriging_manual", "hour")
path_idw_hour             <- file.path(base_path, "idw", "hour")

# ----------------------------
# Load data
# ----------------------------

stations <- st_read("../data/raw/EPD_Current_AQHI/EPD_Current_AQHI.shp")
districts <- st_read("../data/raw/boundaries/hk_boundaries.json") %>%
  st_transform(st_crs(stations))

aq_long <- readRDS("../data/processed/AQHI_hourly_long_all_years_sf.rds")

# ----------------------------
# Grid
# ----------------------------

res <- 500
hk_grid_template <- st_as_stars(st_bbox(districts), dx = res, dy = res)

grid_pts <- st_coordinates(hk_grid_template) %>% as.data.frame()
names(grid_pts) <- c("x", "y")
grid_coords <- st_as_sf(grid_pts, coords = c("x","y"), crs = st_crs(districts))

# ----------------------------
# Tracker
missing_months <- data.frame()
missing_years  <- data.frame()
missing_hours  <- data.frame()

# ============================================================
# MONTHLY
# ============================================================
unique(aq_long$month)

years <- 2021:2025
months <- c("Jan", "Feb", "Mrz", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez")

for(year_sel in years){
  for(month_sel in months){
    
    cat(sprintf("Processing %s %d...\n", month_sel, year_sel))
    
    aq_month <- aq_long %>%
      filter(year == year_sel, month == month_sel) %>%
      group_by(station_name) %>%
      summarise(
        AQHI = mean(AQHI, na.rm = TRUE),
        geometry = st_union(geometry),
        .groups = "drop"
      ) %>%
      st_as_sf()
    
    if(nrow(aq_month) == 0){
      missing_months <- rbind(
        missing_months,
        data.frame(year = year_sel, month = month_sel)
      )
      next
    }
    
    # ---------------- Variogram fitted
    if(nrow(aq_month) < 2 || var(aq_month$AQHI, na.rm = TRUE) == 0){
      vgm_fit <- vgm(psill = 0.01, model = "Sph", range = 10000, nugget = 0.01)
      vgm_manual <- vgm_fit
    } else {
      vgm_emp <- variogram(AQHI ~ 1, aq_month)
      vgm_start <- vgm(psill = var(aq_month$AQHI), model = "Sph", range = 10000, nugget = 0.01)
      vgm_fit <- tryCatch(fit.variogram(vgm_emp, vgm_start), error = function(e) vgm_start)
      
      coords <- st_coordinates(aq_month)
      dists <- dist(coords)
      data_var <- var(aq_month$AQHI, na.rm = TRUE)
      vgm_manual <- vgm(
        psill = 0.7 * data_var,
        model = "Sph",
        range = as.numeric(quantile(dists, 0.5)),
        nugget = 0.3 * data_var
      )
    }
    
    # ---------------- Kriging fitted
    kr_fit <- krige(AQHI ~ 1, aq_month, grid_coords, model = vgm_fit)
    hk_grid_fit <- hk_grid_template
    hk_grid_fit[[1]] <- matrix(kr_fit$var1.pred, nrow = nrow(hk_grid_fit[[1]]), ncol = ncol(hk_grid_fit[[1]]))
    
    # ---------------- Kriging manual
    kr_manual <- krige(AQHI ~ 1, aq_month, grid_coords, model = vgm_manual)
    hk_grid_manual <- hk_grid_template
    hk_grid_manual[[1]] <- matrix(kr_manual$var1.pred, nrow = nrow(hk_grid_manual[[1]]), ncol = ncol(hk_grid_manual[[1]]))
    
    # ---------------- IDW
    idw_res <- idw(AQHI ~ 1, aq_month, grid_coords, idp = 2.0)
    hk_grid_idw <- hk_grid_template
    hk_grid_idw[[1]] <- matrix(idw_res$var1.pred, nrow = nrow(hk_grid_idw[[1]]), ncol = ncol(hk_grid_idw[[1]]))
    
    # ---------------- SAVE
    write_stars(hk_grid_fit, file.path(path_kriging_month, paste0("AQHI_kriging_fit_", year_sel, "_", month_sel, ".tif")))
    write_stars(hk_grid_manual, file.path(path_kriging_manual_month, paste0("AQHI_kriging_manual_", year_sel, "_", month_sel, ".tif")))
    write_stars(hk_grid_idw, file.path(path_idw_month, paste0("AQHI_idw_", year_sel, "_", month_sel, ".tif")))
  }
}

# ============================================================
# ANNUAL
# ============================================================

for(year_sel in years){
  
  cat(sprintf("Processing annual data for year %d...\n", year_sel))
  
  aq_year <- aq_long %>%
    filter(year == year_sel) %>%
    group_by(station_name) %>%
    summarise(
      AQHI = mean(AQHI, na.rm = TRUE),
      geometry = st_union(geometry),
      .groups = "drop"
    ) %>%
    st_as_sf()
  
  if(nrow(aq_year) == 0){
    missing_years <- rbind(missing_years, data.frame(year = year_sel))
    next
  }
  
  # ---------------- Variogram fitted
  if(nrow(aq_year) < 2 || var(aq_year$AQHI, na.rm = TRUE) == 0){
    vgm_fit <- vgm(psill = 0.01, model = "Sph", range = 10000, nugget = 0.01)
    vgm_manual <- vgm_fit
  } else {
    vgm_emp <- variogram(AQHI ~ 1, aq_year)
    vgm_start <- vgm(psill = var(aq_year$AQHI), model = "Sph", range = 10000, nugget = 0.01)
    vgm_fit <- tryCatch(fit.variogram(vgm_emp, vgm_start), error = function(e) vgm_start)
    
    coords <- st_coordinates(aq_year)
    dists <- dist(coords)
    data_var <- var(aq_year$AQHI, na.rm = TRUE)
    vgm_manual <- vgm(psill = 0.7*data_var, model="Sph", range=as.numeric(quantile(dists,0.5)), nugget=0.3*data_var)
  }
  
  # Kriging fitted
  kr_fit <- krige(AQHI ~ 1, aq_year, grid_coords, model = vgm_fit)
  hk_grid_fit <- hk_grid_template
  hk_grid_fit[[1]] <- matrix(kr_fit$var1.pred, nrow=nrow(hk_grid_fit[[1]]), ncol=ncol(hk_grid_fit[[1]]))
  
  # Kriging manual
  kr_manual <- krige(AQHI ~ 1, aq_year, grid_coords, model = vgm_manual)
  hk_grid_manual <- hk_grid_template
  hk_grid_manual[[1]] <- matrix(kr_manual$var1.pred, nrow=nrow(hk_grid_manual[[1]]), ncol=ncol(hk_grid_manual[[1]]))
  
  # IDW
  idw_res <- idw(AQHI ~ 1, aq_year, grid_coords, idp=2.0)
  hk_grid_idw <- hk_grid_template
  hk_grid_idw[[1]] <- matrix(idw_res$var1.pred, nrow=nrow(hk_grid_idw[[1]]), ncol=ncol(hk_grid_idw[[1]]))
  
  # SAVE
  write_stars(hk_grid_fit, file.path(path_kriging_annual, paste0("AQHI_kriging_fit_", year_sel, "_annual.tif")))
  write_stars(hk_grid_manual, file.path(path_kriging_manual_annual, paste0("AQHI_kriging_manual_", year_sel, "_annual.tif")))
  write_stars(hk_grid_idw, file.path(path_idw_annual, paste0("AQHI_idw_", year_sel, "_annual.tif")))
}

# ============================================================
# HOURLY
# ============================================================

for(hour_sel in 1:24){
  
  cat(sprintf("Processing hourly data for hour %02d...\n", hour_sel))
  
  aq_hour <- aq_long %>%
    filter(Hour == hour_sel) %>%
    group_by(station_name) %>%
    summarise(
      AQHI = mean(AQHI, na.rm = TRUE),
      geometry = st_union(geometry),
      .groups = "drop"
    ) %>%
    st_as_sf()
  
  if(nrow(aq_hour) == 0){
    missing_hours <- rbind(missing_hours, data.frame(hour = hour_sel))
    next
  }
  
  # ---------------- Variogram fitted
  if(nrow(aq_hour) < 2 || var(aq_hour$AQHI, na.rm = TRUE) == 0){
    vgm_fit <- vgm(psill=0.01, model="Sph", range=10000, nugget=0.01)
    vgm_manual <- vgm_fit
  } else {
    vgm_emp <- variogram(AQHI ~ 1, aq_hour)
    vgm_start <- vgm(psill = var(aq_hour$AQHI), model = "Sph", range = 10000, nugget = 0.01)
    vgm_fit <- tryCatch(fit.variogram(vgm_emp, vgm_start), error=function(e) vgm_start)
    
    coords <- st_coordinates(aq_hour)
    dists <- dist(coords)
    data_var <- var(aq_hour$AQHI, na.rm = TRUE)
    vgm_manual <- vgm(psill=0.7*data_var, model="Sph", range=as.numeric(quantile(dists,0.5)), nugget=0.3*data_var)
  }
  
  # Kriging fitted
  kr_fit <- krige(AQHI ~ 1, aq_hour, grid_coords, model=vgm_fit)
  hk_grid_fit <- hk_grid_template
  hk_grid_fit[[1]] <- matrix(kr_fit$var1.pred, nrow=nrow(hk_grid_fit[[1]]), ncol=ncol(hk_grid_fit[[1]]))
  
  # Kriging manual
  kr_manual <- krige(AQHI ~ 1, aq_hour, grid_coords, model=vgm_manual)
  hk_grid_manual <- hk_grid_template
  hk_grid_manual[[1]] <- matrix(kr_manual$var1.pred, nrow=nrow(hk_grid_manual[[1]]), ncol=ncol(hk_grid_manual[[1]]))
  
  # IDW
  idw_res <- idw(AQHI ~ 1, aq_hour, grid_coords, idp=2.0)
  hk_grid_idw <- hk_grid_template
  hk_grid_idw[[1]] <- matrix(idw_res$var1.pred, nrow=nrow(hk_grid_idw[[1]]), ncol=ncol(hk_grid_idw[[1]]))
  
  # SAVE
  write_stars(hk_grid_fit, file.path(path_kriging_hour, paste0("AQHI_kriging_fit_hour_", sprintf("%02d", hour_sel), ".tif")))
  write_stars(hk_grid_manual, file.path(path_kriging_manual_hour, paste0("AQHI_kriging_manual_hour_", sprintf("%02d", hour_sel), ".tif")))
  write_stars(hk_grid_idw, file.path(path_idw_hour, paste0("AQHI_idw_hour_", sprintf("%02d", hour_sel), ".tif")))
}

# ============================================================
# SAVE TRACKERS
write.csv(missing_months, file.path(base_path, "missing_months.csv"), row.names=FALSE)
write.csv(missing_years,  file.path(base_path, "missing_years.csv"), row.names=FALSE)
write.csv(missing_hours,  file.path(base_path, "missing_hours.csv"), row.names=FALSE)

cat("=== COMPLETE ===\n")
