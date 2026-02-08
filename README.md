# AQHI Dashboard for Hong Kong

This project provides a Shiny-based dashboard for the analysis and visualization of the Air Quality Health Index (AQHI) in Hong Kong.  
The repository is designed to store raw input data only. All processed datasets and spatial interpolation results are generated locally using the provided R scripts.

---

## Project Structure

The repository follows a structured workflow separating raw data, processed outputs, scripts, and application components.

    project-root/
    ├── data/
    │   ├── raw/                # Raw input data
    │   └── processed/          # Output data
    ├── scripts/
    │   ├── 01_data_import_cleaning.R
    │   └── 02_spatial_analysis.R
    ├── shiny/
    │   └── app.R               # Shiny dashboard application
    └── README.md

---

## Software Requirements

The project was developed using the following software environment:

- R (version 4.5 or higher recommended)  
- RStudio (recommended but not required)  

---

## Required R Packages

### Dashboard Application

The project relies on several R packages for data processing, spatial analysis, and dashboard visualization.  

```r
# Core packages for data manipulation and plotting
install.packages(c("dplyr", "tidyr", "readr", "lubridate", "ggplot2"))

# Spatial packages
install.packages(c("sf", "raster", "stars", "gstat", "leaflet"))

# Dashboard and interactivity
install.packages(c("shiny", "shinyjs", "plotly", "DT", "rstudioapi")) 

```
---

## Raw Data Requirements

All raw input data must be placed in the directory structure shown below. The scripts rely on this exact folder organization.

    data/raw/
    ├── AQHI Data/
    │   ├── 2021/
    │   ├── 2022/
    │   ├── 2023/
    │   ├── 2024/
    │   └── 2025/
    │       └── Monthly CSV files (e.g., 202101_Eng.csv)
    ├── EPD_Current_AQHI/
    │   ├── EPD_Current_AQHI.shp
    │   ├── EPD_Current_AQHI.dbf
    │   ├── EPD_Current_AQHI.shx
    │   ├── EPD_Current_AQHI.prj
    │   ├── EPD_Current_AQHI.cpg
    │   └── EPD_Current_AQHI.xml
    └── boundaries/
        └── hk_boundaries.json

---

## Processing Workflow

The project consists of three sequential processing steps.

### Step 1: Import and Cleaning of Raw Data

Run the script:

    scripts/01_data_import_cleaning.R

This script:

- Imports AQHI observations  
- Harmonizes station metadata  
- Converts the data into spatial format  
- Combines all available years into a unified dataset  

Output file:

    data/processed/AQHI_hourly_long_all_years_sf.rds

---

### Step 2: Spatial Interpolation

Run the script:

    scripts/02_spatial_analysis.R

This script performs spatial interpolation using multiple approaches:

- Ordinary Kriging with variogram fitting  
- Manual Kriging with predefined variogram parameters  
- Inverse Distance Weighting (IDW)  

Interpolations are produced for three temporal aggregation levels:

- Monthly averages  
- Annual averages  
- Hourly averages across all years  

Output directory:

    data/processed/spatial_analysis/

This directory contains raster files and missing data tracking outputs.

---

### Step 3: Running the Dashboard

Before starting the dashboard, ensure that the working directory is set to the project root folder.  
All file paths used in the application are relative to this directory.

#### Option A: Using RStudio (Recommended)

1. Open the project in RStudio.
2. Navigate to:

       shiny/app.R

3. Click the **Run App** button.

#### Option B: Using the R Console

Set the working directory to the project root folder and run:

       shiny::runApp("shiny")

The dashboard automatically loads all processed datasets and spatial raster layers from the processed data directory.


---

## Notes on Processing Time

Spatial interpolation and raster generation can require several minutes depending on computational resources and system performance.

---

## Troubleshooting

### Missing Input Files

Verify that all raw data files are placed correctly in the data/raw/ directory and follow the required naming conventions.

---

### Shapefile Loading Errors

Ensure that all required shapefile components are present:

- .shp  
- .dbf  
- .shx  
- .prj  
- .cpg  

---

## Data Sources

The AQHI data and spatial boundary datasets originate from:

- Hong Kong Environmental Protection Department (EPD)  
- Hong Kong Government Data Portal  

Detailed attribution information is provided within the dashboard.

---

## Reproducibility

The repository is designed to support full reproducibility of all derived datasets and visualizations. All processing steps are documented and implemented using open-source R packages.

---

## Author

Lukas Räuschel, February 2026

