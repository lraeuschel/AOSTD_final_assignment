
# ============================================================
# app.R
# AQHI Dashboard Skeleton
# ============================================================
# ----------------------------
# 0. Libraries
# ----------------------------
library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)   # optional for interactive plots
library(sf)       # for spatial data
library(leaflet)  # for maps
library(DT)       # for data tables
library(shinyjs)  # for disable/enable inputs
library(stars)    # for raster data
library(raster)   # for raster operations

# ----------------------------
# 1. Load pre-processed data
# ----------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

aq_all <- readRDS("../data/processed/AQHI_hourly_long_all_years_sf.rds")
stations <- st_read("../data/raw/EPD_Current_AQHI/EPD_Current_AQHI.shp")
stations <- st_transform(stations, 4326)  # Transform to WGS84 for leaflet

# ----------------------------
# 1. UI
# ----------------------------
ui <- fluidPage(
  useShinyjs(),
  navbarPage(
    title = tagList(
      icon("smog"),
      "AQHI Dashboard"
    ),
    
    # ----------------------------
    # Tab 0: About Project
    # ----------------------------
    tabPanel("About",
      fluidRow(
        column(12,
          h2("Analysis of the Air Quality Health Index (AQHI) in Hong Kong"),
          tags$div(
            style = "margin-bottom: 20px;",
            tags$p(
              "This dashboard was created as part of the course ",
              tags$strong("Analysis of Spatio-Temporal Data - Seminar WiSe 2025/26"), br(),
              "Instructors: Prof. Dr. Edzer Pebesma and Poshan Niraula"
            ),
            tags$div(
              tags$strong("Name: "), "Lukas Räuschel", br(),
              tags$strong("Student Number: "), "536377"
            )
          ),
          hr()
        )
      ),
      
      fluidRow(
        column(12,
          h3(icon("info-circle"), "What is AQHI?"),
          tags$p(
            "The ", tags$strong("Air Quality Health Index (AQHI)"), " is a health risk-based air pollution index developed by Hong Kong's Environmental Protection Department. ",
            "It provides an estimate of the short-term health risk (hospital admission) from air pollution, particularly for heart and respiratory diseases."
          ),
          tags$p(
            tags$strong("Scale: "), "The AQHI is reported on a scale from 1 to 10 and 10+, grouped into five health risk categories: ",
            tags$span(style = "color: #00E400;", "Low (1-3)"), ", ",
            tags$span(style = "color: #FFAA00;", "Moderate (4-6)"), ", ",
            tags$span(style = "color: #FF7E00;", "High (7)"), ", ",
            tags$span(style = "color: #FF0000;", "Very High (8-10)"), ", and ",
            tags$span(style = "color: #8B0000;", "Serious (10+)"), "."
          ),
          tags$p(
            tags$strong("Calculation: "), "The AQHI is calculated from the combined health risk of four air pollutants: ",
            "Ozone (O₃), Nitrogen Dioxide (NO₂), Sulphur Dioxide (SO₂), and Particulate Matter (PM₁₀ or PM₂.₅). ",
            "Each pollutant's contribution is weighted by health risk factors derived from local hospital admission data and air quality measurements. ",
            "The index represents the added health risk (%AR) based on 3-hour moving average concentrations."
          ),
          tags$small(
            "Source: ",
            tags$a("Hong Kong EPD - AQHI FAQs",
                  href = "https://www.aqhi.gov.hk/en/what-is-aqhi/faqs.html",
                  target = "_blank")
          ),
          hr()
        )
      ),
      
      fluidRow(
        column(12,
          h3(icon("laptop-code"), "Dashboard Features"),
          tags$ul(
            tags$li(tags$strong("Trend Analysis Tab: "), "Interactive time series plots showing daily AQHI development with station comparison"),
            tags$li(tags$strong("Hourly Analysis Tab: "), "Diurnal patterns showing average AQHI by hour of day"),
            tags$li(tags$strong("Spatial Analysis Tab: "), "Interactive maps with Kriging and IDW interpolations at monthly, annual, and hourly scales")
          ),
          hr()
        )
      ),
      
      fluidRow(
        column(12,
          h3(icon("chart-bar"), "Dataset Statistics"),
          tags$ul(
            tags$li(
              tags$strong("Total Records: "), 
              format(nrow(aq_all), big.mark = ","), " hourly measurements"
            ),
            tags$li(
              tags$strong("Monitoring Stations: "), 
              n_distinct(aq_all$station_name), " locations across Hong Kong"
            ),
            tags$li(
              tags$strong("Time Period: "), 
              paste0(month.name[lubridate::month(min(aq_all$date, na.rm = TRUE))], " ", 
                     lubridate::year(min(aq_all$date, na.rm = TRUE))), " to ", 
              paste0(month.name[lubridate::month(max(aq_all$date, na.rm = TRUE))], " ", 
                     lubridate::year(max(aq_all$date, na.rm = TRUE)))
            ),
            tags$li(
              tags$strong("Data Completeness: "), 
              paste0(round((1 - sum(is.na(aq_all$AQHI)) / nrow(aq_all)) * 100, 1), "% valid measurements")
            )
          ),
          hr()
        )
      ),
      
      fluidRow(
        column(6,
          h3(icon("database"), "Data Sources"),
          tags$ul(
            tags$li(
              tags$strong("Administrative Boundaries"), br(),
              tags$small("Source: ", tags$a("Open Data Portal of the Government of Hong Kong", 
                                           href = "https://data.gov.hk/en-data/dataset/hk-had-json1-hong-kong-administrative-boundaries",
                                           target = "_blank"), br(),
                        "Content: Administrative District Boundaries of Hong Kong", br(),
                        "Resolution: Vector Polygons")
            ),
            br(),
            tags$li(
              tags$strong("Air Quality Monitoring Stations"), br(),
              tags$small("Source: ", tags$a("Environmental Protection Department (EPD) Hong Kong",
                                           href = "https://www.aqhi.gov.hk/en/monitoring-network/air-quality-monitoring-stations.html",
                                           target = "_blank"), br(),
                        "Content: Location of the Air Quality Monitoring Stations", br(),
                        "Resolution: Point Data (shape file)")
            ),
            br(),
            tags$li(
              tags$strong("Past Hourly Air Quality Measurements"), br(),
              tags$small("Source: ", tags$a("Environmental Protection Department (EPD) Hong Kong",
                                           href = "https://www.aqhi.gov.hk/en/past-data/past-aqhi.html",
                                           target = "_blank"), br(),
                        "Content: Hourly Air Quality Measurements for each month and year from 2021-2025", br(),
                        "Resolution: Spatial (Air Quality Monitoring Stations); Temporal (Hourly, 2021-2025)")
            )
          )
        ),
        
        column(6,
          h3(icon("chart-line"), "Analysis & Methods"),
          tags$ul(
            tags$li(
              tags$strong("Libraries: "), 
              tags$code("dplyr, sf, ggplot2, plotly, leaflet, lubridate, stars, gstat")
            ),
            br(),
            tags$li(
              tags$strong("Data Processing"), br(),
              tags$small("• Import and merge 60 monthly CSV files (2021-2025)", br(),
                        "• Data cleaning and quality checks", br(),
                        "• Spatial join with station locations", br(),
                        "• Calculate temporal features (year, month, hour)")
            ),
            br(),
            # tags$li(
            #   tags$strong("Temporal Analysis"), br(),
            #   tags$small("• Daily aggregation: mean AQHI per station and day", br(),
            #             "• Monthly aggregation: identify seasonal patterns", br(),
            #             "• Hourly patterns: detect diurnal variations", br(),
            #             "• Comparative analysis between stations and overall HK average")
            # ),
            br(),
            tags$li(
              tags$strong("Spatial Interpolation"), br(),
              tags$small(
                tags$strong("Kriging (Geostatistical):"), br(),
                "  • Variogram modeling (spherical model)", br(),
                "  • Considers spatial autocorrelation", br(),
                "  • Optimal spatial prediction with error estimates", br(),
                br(),
                tags$strong("IDW (Deterministic):"), br(),
                "  • Inverse Distance Weighting (power = 2)", br(),
                "  • Simpler distance-based interpolation", br(),
                "  • Faster computation, no variogram needed", br(),
                br(),
                tags$strong("Aggregation Levels:"), br(),
                "  • Monthly: 60 rasters (2021-2025, all months)", br(),
                "  • Annual: 5 rasters (yearly averages)", br(),
                "  • Hourly: 24 rasters (average for each hour across all years)", br(),
                "  • Resolution: 500m grid", br(),
                "  • Total: 178 interpolated rasters (89 Kriging + 89 IDW)"
              )
            )
          )
        )
      )
    ),
    
    # ----------------------------
    # Tab 2: Trend Analysis
    # ----------------------------
    tabPanel("Trend Analysis",
      sidebarLayout(
        sidebarPanel(
          h4("Filter Options"),
          
          # Station selection
          selectInput("trend_station", 
                      "Select Station:", 
                      choices = c("All Stations" = "all", 
                                  sort(unique(aq_all$station_name))),
                      selected = "all"),
          
          # Year selection
          selectInput("trend_year",
                      "Select Year:",
                      choices = sort(unique(aq_all$year)),
                      selected = max(aq_all$year)),
          
          # Month selection
          fluidRow(
            column(8,
              selectInput("trend_month",
                          "Select Month:",
                          choices = sort(unique(aq_all$month)),
                          selected = "Jan")
            ),
            column(4,
              br(),
              checkboxInput("trend_whole_year", "Whole Year", value = FALSE)
            )
          ),

          checkboxInput("trend_compare", "Compare with the average of all stations", value = FALSE),
          br(), br(),
          
          # Station map
          h4("Station Location"),
          leafletOutput("trend_station_map", height = "250px"),
          
          width = 3
        ),
        
        mainPanel(
          h4("AQHI Trend"),
          plotlyOutput("trend_plot", height = "500px"),
          width = 9
        )
      )
    ),
    
    # ----------------------------
    # Tab 3: Hourly Analysis
    # ----------------------------
    tabPanel("Hourly Analysis",
      sidebarLayout(
        sidebarPanel(
          h4("Filter Options"),
          
          # Station selection
          selectInput("hourly_station", 
                      "Select Station:", 
                      choices = c("All Stations" = "all",
                                  sort(unique(aq_all$station_name))),
                      selected = "all"),
          
          # Year selection
          selectInput("hourly_year",
                      "Select Year:",
                      choices = sort(unique(aq_all$year)),
                      selected = max(aq_all$year)),
          
          # Month selection
          fluidRow(
            column(8,
              selectInput("hourly_month",
                          "Select Month:",
                          choices = sort(unique(aq_all$month)),
                          selected = "Jan")
            ),
            column(4,
              br(),
              checkboxInput("hourly_whole_year", "Whole Year", value = FALSE)
            )
          ),
          
          checkboxInput("hourly_compare", "Compare with the average of all stations", value = FALSE),
          br(), br(),
          
          # Station map
          h4("Station Location"),
          leafletOutput("hourly_station_map", height = "250px"),
          
          width = 3
        ),
        
        mainPanel(
          h4("Hourly AQHI Trend"),
          plotlyOutput("hourly_plot", height = "500px"),
          width = 9
        )
      )
    ),
    
    # ----------------------------
    # Tab 4: Spatial Analysis
    # ----------------------------
    tabPanel("Spatial Analysis",
      sidebarLayout(
        sidebarPanel(
          h4("Filter Options"),
          
          # Method selection
          radioButtons("spatial_method",
                      "Interpolation Method:",
                      choices = c("Kriging" = "kriging", "IDW" = "idw"),
                      selected = "kriging"),
          
          hr(),
          
          # Selection type
          radioButtons("spatial_type",
                      "Temporal Selection:",
                      choices = c("Monthly" = "month", 
                                  "Annual" = "annual",
                                  "Hourly Average" = "hour"),
                      selected = "month"),
          
          # Conditional panels based on selection type
          conditionalPanel(
            condition = "input.spatial_type == 'month'",
            selectInput("spatial_year",
                        "Select Year:",
                        choices = 2021:2025,
                        selected = 2024),
            selectInput("spatial_month",
                        "Select Month:",
                        choices = c("Jan", "Feb", "Mrz", "Apr", "Mai", "Jun",
                                   "Jul", "Aug", "Sep", "Okt", "Nov", "Dez"),
                        selected = "Jan")
          ),
          
          conditionalPanel(
            condition = "input.spatial_type == 'annual'",
            selectInput("spatial_year_annual",
                        "Select Year:",
                        choices = 2021:2025,
                        selected = 2024)
          ),
          
          conditionalPanel(
            condition = "input.spatial_type == 'hour'",
            sliderInput("spatial_hour",
                        "Select Hour of Day:",
                        min = 1,
                        max = 24,
                        value = 12,
                        step = 1)
          ),
          
          hr(),
          
          # Legend type selection
          radioButtons("spatial_legend_type",
                      "Legend Type:",
                      choices = c("Categories" = "category", 
                                  "Continuous" = "continuous"),
                      selected = "category"),
          
          h4("Legend"),
          conditionalPanel(
            condition = "input.spatial_legend_type == 'category'",
            tags$div(
              style = "font-size: 12px;",
              "AQHI Color Scale:", br(),
              tags$div(style = "display: flex; align-items: center; margin: 2px 0;",
                tags$div(style = "width: 20px; height: 20px; background: #00E400; margin-right: 5px;"),
                "Low (1-3)"
              ),
              tags$div(style = "display: flex; align-items: center; margin: 2px 0;",
                tags$div(style = "width: 20px; height: 20px; background: #FFFF00; margin-right: 5px;"),
                "Moderate (4-6)"
              ),
              tags$div(style = "display: flex; align-items: center; margin: 2px 0;",
                tags$div(style = "width: 20px; height: 20px; background: #FF7E00; margin-right: 5px;"),
                "High (7)"
              ),
              tags$div(style = "display: flex; align-items: center; margin: 2px 0;",
                tags$div(style = "width: 20px; height: 20px; background: #FF0000; margin-right: 5px;"),
                "Very High (8-10)"
              ),
              tags$div(style = "display: flex; align-items: center; margin: 2px 0;",
                tags$div(style = "width: 20px; height: 20px; background: #8B0000; margin-right: 5px;"),
                "Serious (10+)"
              )
            )
          ),
          conditionalPanel(
            condition = "input.spatial_legend_type == 'continuous'",
            tags$div(
              style = "font-size: 12px;",
              "Continuous AQHI Scale (Min to Max)"
            )
          ),
          
          width = 3
        ),
        
        mainPanel(
          h4(textOutput("spatial_title")),
          leafletOutput("spatial_map", height = "600px"),
          br(),
          p(textOutput("spatial_info")),
          width = 9
        )
      )
    )
  )
)


# ----------------------------
# 2. Server
# ----------------------------
server <- function(input, output, session) {
  
  # Disable/enable month select based on whole year checkbox
  observe({
    if (input$trend_whole_year) {
      shinyjs::disable("trend_month")
    } else {
      shinyjs::enable("trend_month")
    }
  })
  
  # ----------------------------
  # Tab 2: Trend Analysis
  # ----------------------------
  
  # Reactive data for trend analysis
  trend_data <- reactive({
    # Start with daily data
    data <- aq_all
    print(paste("Initial rows:", nrow(data)))
    print(paste("names of columns:", paste(colnames(data), collapse = ", ")))
    print(head(data))
    
    # Filter by year
    if (!is.null(input$trend_year)) {
      data <- data %>% filter(year == input$trend_year)
    }
    print(paste("Filtered by year:", input$trend_year))
    print(paste("summary rows after filtering:", nrow(data)))
    
    # Filter by month (skip if "Whole Year" checkbox is checked)
    if (!is.null(input$trend_month) && !input$trend_whole_year) {
      data <- data %>% filter(month == input$trend_month)
    }
    print(paste("Filtered by month:", input$trend_month))
    print(paste("summary rows after filtering:", nrow(data)))
    
    # Filter by station or aggregate for all HK
    if (input$trend_station == "all") {
      print("Aggregating data for all stations")
      # Aggregate across all stations for each date
      data <- data %>%
        group_by(date, year, month) %>%
        summarise(
          AQHI = mean(AQHI, na.rm = TRUE),
          station_name = "All Stations",
          .groups = "drop"
        )
    } else {
      data <- data %>% filter(station_name == input$trend_station)
    }
    
    data
  })

  hk_avg_data <- reactive({
    data <- aq_all
    if (!is.null(input$trend_year)) {
      data <- data %>% filter(year == input$trend_year)
    }
    if (!is.null(input$trend_month) && !input$trend_whole_year) {
      data <- data %>% filter(month == input$trend_month)
    }

    data %>%
      group_by(date, year, month) %>%
      summarise(
        AQHI = mean(AQHI, na.rm = TRUE),
        station_name = "All Stations",
        .groups = "drop"
      ) %>%
      arrange(date)
  })

  
  output$trend_plot <- renderPlotly({

    main_data <- trend_data() %>% 
      ungroup() %>%
      group_by(date, year, month, station_name) %>%
      summarise(AQHI = mean(AQHI, na.rm = TRUE), .groups = "drop") %>%
      arrange(date)

    if (nrow(main_data) == 0) {
      return(
        plotly_empty(type = "scatter", mode = "lines")
      )
    }

    compare_data <- NULL
    show_compare <- FALSE
    if (input$trend_station != "all" && isTRUE(input$trend_compare)) {
      compare_data <- hk_avg_data()
      show_compare <- TRUE
    }

    # Determine mode and marker visibility
    if (input$trend_whole_year) {
      p <- plot_ly() %>%
        add_trace(
          data = main_data,
          x = ~date,
          y = ~AQHI,
          type = "scatter",
          mode = "lines",
          name = ifelse(input$trend_station == "all", "All Stations", input$trend_station),
          line = list(width = 2, color = "#1f77b4"),
          hovertemplate = "<b>Date:</b> %{x}<br><b>AQHI:</b> %{y:.1f}<extra></extra>"
        )
    } else {
      p <- plot_ly() %>%
        add_trace(
          data = main_data,
          x = ~date,
          y = ~AQHI,
          type = "scatter",
          mode = "lines+markers",
          name = ifelse(input$trend_station == "all", "All Stations", input$trend_station),
          line = list(width = 2, color = "#1f77b4", shape = "spline"),
          marker = list(size = 6, color = "#1f77b4"),
          hovertemplate = "<b>Date:</b> %{x}<br><b>AQHI:</b> %{y:.1f}<extra></extra>"
        )
    }

    if (!is.null(compare_data)) {
      if (input$trend_whole_year) {
        p <- p %>%
          add_trace(
            data = compare_data,
            x = ~date,
            y = ~AQHI,
            type = "scatter",
            mode = "lines",
            name = "All stations (avg)",
            line = list(width = 2, color = "#e57373"),
            hovertemplate = "<b>Date:</b> %{x}<br><b>All stations Avg:</b> %{y:.1f}<extra></extra>"
          )
      } else {
        p <- p %>%
          add_trace(
            data = compare_data,
            x = ~date,
            y = ~AQHI,
            type = "scatter",
            mode = "lines+markers",
            name = "All stations (avg)",
            line = list(width = 2, color = "#e57373", shape = "spline"),
            marker = list(size = 6, color = "#e57373"),
            hovertemplate = "<b>Date:</b> %{x}<br><b>All stations Avg:</b> %{y:.1f}<extra></extra>"
          )
      }
    }

    p <- p %>%
      layout(
        title = paste(
          "AQHI Trend –",
          ifelse(input$trend_station == "all", "All Stations", input$trend_station),
          "(",
          input$trend_year,
          "-",
          input$trend_month,
          ")"
        ),
        xaxis = list(title = "Date", gridcolor = "lightgray", showgrid = TRUE, showticklabels = TRUE),
        yaxis = list(title = "AQHI Value", gridcolor = "lightgray", showgrid = TRUE, showticklabels = TRUE),
        hovermode = "x unified",
        plot_bgcolor = "rgba(240, 240, 240, 0.5)",
        showlegend = show_compare
      )
    
    p
  })


  # ----------------------------
  # Trend data table
  # ----------------------------
  output$trend_table <- DT::renderDT({
    data <- trend_data()
    # Remove geometry column if it exists
    if ("geometry" %in% colnames(data)) {
      data <- data %>% st_drop_geometry()
    }
    data
  }, options = list(
    pageLength = 10,
    scrollX = TRUE
  ))
  # Station map
  output$trend_station_map <- renderLeaflet({
    m <- leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircles(
        data = stations,
        radius = 500,
        color = "#3388ff",
        fillOpacity = 0.4,
        weight = 2,
        label = ~Name
      ) %>%
      setView(lng = 114.17, lat = 22.3, zoom = 9)
    
    if (input$trend_station != "all") {
      # Highlight selected station in red
      station_data <- stations %>% filter(Name == input$trend_station)
      
      if (nrow(station_data) > 0) {
        coords <- st_coordinates(station_data)
        m <- m %>%
          addCircles(
            data = station_data,
            radius = 700,
            color = "#d32f2f",
            fillOpacity = 0.7,
            weight = 3,
            label = ~Name
          ) %>%
          setView(lng = coords[1], lat = coords[2], zoom = 9)
      }
    }
    
    m
  })

  output$trend_debug <- renderPrint({
    data <- trend_data()
    
    cat("Rows:", nrow(data), "\n")
    cat("Columns:\n")
    print(colnames(data))
    
    cat("\nDate range:\n")
    print(range(data$date, na.rm = TRUE))
    
    cat("\nStation:\n")
    print(unique(data$station_name))
  })


  
  output$trend_summary <- renderPrint({
    data <- trend_data()
    
    if (nrow(data) == 0) {
      cat("No data available for selected filters\n")
      return()
    }
    
    cat("=== Summary Statistics ===\n")
    cat("Station:", ifelse(input$trend_station == "all", "All Stations", input$trend_station), "\n")
    cat("Period:", min(data$date, na.rm = TRUE), "to", max(data$date, na.rm = TRUE), "\n")
    cat("Number of observations:", nrow(data), "\n\n")
    
    cat("AQHI Statistics:\n")
    print(summary(data$AQHI))
    
    cat("\n\nBy Year:\n")
    year_summary <- data %>%
      group_by(year) %>%
      summarise(
        Mean = round(mean(AQHI, na.rm = TRUE), 2),
        Median = round(median(AQHI, na.rm = TRUE), 2),
        Min = round(min(AQHI, na.rm = TRUE), 2),
        Max = round(max(AQHI, na.rm = TRUE), 2)
      )
    print(as.data.frame(year_summary))
  })
  
  # ----------------------------
  # Tab 3: Hourly Analysis
  # ----------------------------
  
  # Disable/enable month select based on whole year checkbox
  observe({
    if (input$hourly_whole_year) {
      shinyjs::disable("hourly_month")
    } else {
      shinyjs::enable("hourly_month")
    }
  })
  
  hourly_data <- reactive({
    data <- aq_all %>%
      st_drop_geometry()
    
    if (!is.null(input$hourly_year)) {
      data <- data %>% filter(year == input$hourly_year)
    }
    
    if (!is.null(input$hourly_month) && !input$hourly_whole_year) {
      data <- data %>% filter(month == input$hourly_month)
    }
    
    if (input$hourly_station == "all") {
      data <- data %>%
        group_by(Hour) %>%
        summarise(
          AQHI = mean(AQHI, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      data <- data %>%
        filter(station_name == input$hourly_station) %>%
        group_by(Hour) %>%
        summarise(
          AQHI = mean(AQHI, na.rm = TRUE),
          .groups = "drop"
        )
    }
    
    data %>% arrange(Hour)
  })
  
  hk_hourly_avg_data <- reactive({
    data <- aq_all %>%
      st_drop_geometry()
    
    if (!is.null(input$hourly_year)) {
      data <- data %>% filter(year == input$hourly_year)
    }
    
    if (!is.null(input$hourly_month) && !input$hourly_whole_year) {
      data <- data %>% filter(month == input$hourly_month)
    }
    
    data %>%
      group_by(Hour) %>%
      summarise(
        AQHI = mean(AQHI, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(Hour)
  })
  
  output$hourly_plot <- renderPlotly({
    data <- hourly_data()
    
    if (nrow(data) == 0) {
      return(plotly_empty(type = "scatter", mode = "lines+markers"))
    }
    
    compare_data <- NULL
    show_compare <- FALSE
    if (input$hourly_station != "all" && isTRUE(input$hourly_compare)) {
      compare_data <- hk_hourly_avg_data()
      show_compare <- TRUE
    }
    
    p <- plot_ly() %>%
      add_trace(
        data = data,
        x = ~Hour,
        y = ~AQHI,
        type = "scatter",
        mode = "lines+markers",
        name = ifelse(input$hourly_station == "all", "All Stations", input$hourly_station),
        line = list(width = 2, color = "#1f77b4"),
        marker = list(size = 8, color = "#1f77b4"),
        hovertemplate = "<b>Hour:</b> %{x}:00<br><b>Avg AQHI:</b> %{y:.1f}<extra></extra>"
      )
    
    if (!is.null(compare_data)) {
      p <- p %>%
        add_trace(
          data = compare_data,
          x = ~Hour,
          y = ~AQHI,
          type = "scatter",
          mode = "lines+markers",
          name = "All stations (avg)",
          line = list(width = 2, color = "#e57373"),
          marker = list(size = 8, color = "#e57373"),
          hovertemplate = "<b>Hour:</b> %{x}:00<br><b>All stations Avg:</b> %{y:.1f}<extra></extra>"
        )
    }
    
    p <- p %>%
      layout(
        title = paste(
          "Average Hourly AQHI Profile –",
          ifelse(input$hourly_station == "all", "All Stations", input$hourly_station),
          "(",
          input$hourly_year,
          "-",
          input$hourly_month,
          ")"
        ),
        xaxis = list(
          title = "Hour of Day",
          tickmode = "linear",
          tick0 = 0,
          dtick = 1,
          gridcolor = "lightgray",
          showgrid = TRUE
        ),
        yaxis = list(title = "AQHI Value", gridcolor = "lightgray", showgrid = TRUE),
        hovermode = "x unified",
        plot_bgcolor = "rgba(240, 240, 240, 0.5)",
        showlegend = show_compare
      )
    
    p
  })
  
  output$hourly_station_map <- renderLeaflet({
    m <- leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircles(
        data = stations,
        radius = 500,
        color = "#3388ff",
        fillOpacity = 0.4,
        weight = 2,
        label = ~Name
      ) %>%
      setView(lng = 114.17, lat = 22.3, zoom = 9)
    
    if (input$hourly_station != "all") {
      station_data <- stations %>% filter(Name == input$hourly_station)
      
      if (nrow(station_data) > 0) {
        coords <- st_coordinates(station_data)
        m <- m %>%
          addCircles(
            data = station_data,
            radius = 700,
            color = "#d32f2f",
            fillOpacity = 0.7,
            weight = 3,
            label = ~Name
          ) %>%
          setView(lng = coords[1], lat = coords[2], zoom = 9)
      }
    }
    
    m
  })
  
  # ----------------------------
  # Tab 4: Spatial Analysis
  # ----------------------------
  
  # Reactive expression to load the raster
  spatial_raster <- reactive({
    method <- input$spatial_method
    type <- input$spatial_type
    
    # Build file path based on selections
    base_path <- "../data/processed/spatial_analysis"
    
    if (type == "month") {
      year <- input$spatial_year
      month <- input$spatial_month
      filename <- paste0("AQHI_", method, "_", year, "_", month, ".tif")
      filepath <- file.path(base_path, method, "month", filename)
    } else if (type == "annual") {
      year <- input$spatial_year_annual
      filename <- paste0("AQHI_", method, "_", year, "_annual.tif")
      filepath <- file.path(base_path, method, "annual", filename)
    } else if (type == "hour") {
      hour <- sprintf("%02d", input$spatial_hour)
      filename <- paste0("AQHI_", method, "_hour_", hour, ".tif")
      filepath <- file.path(base_path, method, "hour", filename)
    }
    
    # Check if file exists
    if (!file.exists(filepath)) {
      return(NULL)
    }
    
    # Load raster
    tryCatch({
      r <- read_stars(filepath)
      r
    }, error = function(e) {
      NULL
    })
  })
  
  # Title output
  output$spatial_title <- renderText({
    method_name <- ifelse(input$spatial_method == "kriging", "Kriging", "IDW")
    
    if (input$spatial_type == "month") {
      paste0("Spatial Interpolation (", method_name, ") - ", 
             input$spatial_month, " ", input$spatial_year)
    } else if (input$spatial_type == "annual") {
      paste0("Spatial Interpolation (", method_name, ") - Annual Average ", 
             input$spatial_year_annual)
    } else {
      paste0("Spatial Interpolation (", method_name, ") - Hour ", 
             input$spatial_hour, ":00 (All Years Average)")
    }
  })
  
  # Info output
  output$spatial_info <- renderText({
    r <- spatial_raster()
    if (is.null(r)) {
      return("⚠ Raster file not found. Please check if the spatial analysis has been run.")
    }
    
    # Extract values and compute statistics
    values <- as.vector(r[[1]])
    values <- values[!is.na(values)]
    
    if (length(values) == 0) {
      return("⚠ No valid data in raster.")
    }
    
    paste0("Min: ", round(min(values), 2), " | ",
           "Max: ", round(max(values), 2), " | ",
           "Mean: ", round(mean(values), 2))
  })
  
  # Map output
  output$spatial_map <- renderLeaflet({
    r <- spatial_raster()
    
    # Base map
    m <- leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 114.17, lat = 22.3, zoom = 11)
    
    if (is.null(r)) {
      return(m)
    }
    
    # Convert stars to raster for leaflet
    r_raster <- as(r, "Raster")
    
    # Get actual data range
    values <- as.vector(r[[1]])
    values <- values[!is.na(values)]
    
    if (length(values) == 0) {
      return(m)
    }
    
    min_val <- min(values)
    max_val <- max(values)
    
    # Define color palette based on legend type
    if (input$spatial_legend_type == "category") {
      # Fixed categorical palette
      pal <- colorNumeric(
        palette = c("#00E400", "#FFFF00", "#FF7E00", "#FF0000", "#8B0000"),
        domain = c(0, 11),
        na.color = "transparent"
      )
      legend_values <- c(0, 11)
      legend_title <- "AQHI"
    } else {
      # Continuous palette scaled to actual data range
      pal <- colorNumeric(
        palette = c("#00E400", "#FFFF00", "#FF7E00", "#FF0000", "#8B0000"),
        domain = c(min_val, max_val),
        na.color = "transparent"
      )
      legend_values <- c(min_val, max_val)
      legend_title <- paste0("AQHI (", round(min_val, 1), "-", round(max_val, 1), ")")
    }
    
    # Add raster layer
    m <- m %>%
      addRasterImage(r_raster, colors = pal, opacity = 0.7) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = legend_values,
        title = legend_title,
        opacity = 0.7
      ) %>%
      addCircleMarkers(
        data = stations,
        radius = 4,
        color = "black",
        fillColor = "white",
        fillOpacity = 0.8,
        weight = 2,
        label = ~Name
      )
    
    m
  })
  
}

# ----------------------------
# 3. Run App
# ----------------------------
shinyApp(ui = ui, server = server)