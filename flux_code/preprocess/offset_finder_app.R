library(shiny)
library(plotly)
library(dplyr)
library(lubridate)
library(readr)

# Prepare flux data for both March and October 2022
prepare_flux_data <- function() {
  # Load tree flux data
  tree_flux <- read_csv("field_notes/blueflux compiled tree fluxes_additional.csv") %>%
    filter(analyzer == "Picarro") %>%
    mutate(
      date_parsed = mdy(date),
      datetime_str = paste(format(date_parsed, "%Y-%m-%d"), start_time),
      start_datetime = ymd_hms(datetime_str, tz = "America/New_York"),
      end_datetime_str = paste(format(date_parsed, "%Y-%m-%d"), end_time),
      end_datetime = ymd_hms(end_datetime_str, tz = "America/New_York"),
      dataset = "Tree",
      start_time_display = start_time,
      end_time_display = end_time,
      component_display = component,
      surface_type = component,
      measurement_id = index
    ) %>%
    filter(
      !is.na(start_datetime),
      ((date_parsed >= as.Date("2022-03-18") & date_parsed <= as.Date("2022-03-24")) |
         (date_parsed >= as.Date("2022-10-17") & date_parsed <= as.Date("2022-10-25")))
    )
  
  # Load soil/water flux data
  soil_water_flux <- read_csv("/Users/jongewirtzman/My Drive/Research/Blueflux/blueflux-ground/field_notes/BlueFlux Dataset_soils_water.csv") %>%
    filter(`Gas Analyzer` == "Picarro") %>%
    mutate(
      date_parsed = mdy(Date),
      start_time_clean = `Flux Start Time`,
      end_time_clean = `Flux End Time`,
      datetime_str = paste(format(date_parsed, "%Y-%m-%d"), start_time_clean),
      start_datetime = ymd_hms(datetime_str, tz = "America/New_York"),
      end_datetime_str = paste(format(date_parsed, "%Y-%m-%d"), end_time_clean),
      end_datetime = ymd_hms(end_datetime_str, tz = "America/New_York"),
      dataset = "Soil/Water",
      start_time_display = start_time_clean,
      end_time_display = end_time_clean,
      component_display = Surface,
      surface_type = Surface,
      measurement_id = paste(Plot, `Collar Notes`, sep = "_"),
      plot_info = Plot
    ) %>%
    filter(
      !is.na(start_datetime),
      ((date_parsed >= as.Date("2022-03-18") & date_parsed <= as.Date("2022-03-24")) |
         (date_parsed >= as.Date("2022-10-17") & date_parsed <= as.Date("2022-10-25")))
    )
  
  # Combine datasets with common column structure
  combined_flux <- bind_rows(
    tree_flux %>% 
      mutate(
        species = if("species" %in% names(.)) species else NA_character_,
        plot_info = if("plot" %in% names(.)) plot else NA_character_
      ) %>%
      select(
        dataset, date_parsed, start_datetime, end_datetime,
        start_time_display, end_time_display, component_display,
        surface_type, measurement_id, species, plot_info
      ),
    soil_water_flux %>% 
      mutate(
        species = NA_character_  # Soil/water doesn't have species
      ) %>%
      select(
        dataset, date_parsed, start_datetime, end_datetime,
        start_time_display, end_time_display, component_display,
        surface_type, measurement_id, species, plot_info
      )
  ) %>%
    arrange(start_datetime)
  
  cat("Data loading summary:\n")
  cat("Tree flux events:", nrow(tree_flux), "\n")
  cat("Soil/Water flux events:", nrow(soil_water_flux), "\n")
  cat("Combined total:", nrow(combined_flux), "\n")
  
  return(combined_flux)
}

# Generate date choices dynamically
generate_date_choices <- function() {
  march_dates <- seq.Date(from = as.Date("2022-03-18"), to = as.Date("2022-03-24"), by = "day")
  october_dates <- seq.Date(from = as.Date("2022-10-17"), to = as.Date("2022-10-25"), by = "day")
  
  all_dates <- c(march_dates, october_dates)
  choices <- setNames(as.character(all_dates), format(all_dates, "%B %d, %Y"))
  
  return(choices)
}

# UI
ui <- fluidPage(
  titlePanel("Picarro CH4 Alignment - March & October 2022"),
  
  fluidRow(
    column(3,
           wellPanel(
             h4("Time Offset Controls"),
             
             sliderInput("offset_minutes", 
                         "Coarse (minutes):",
                         min = -720, max = 720,
                         value = 0, step = 10),
             
             numericInput("offset_seconds",
                          "Fine (seconds):",
                          value = 0, step = 5),
             
             hr(),
             
             selectInput("focus_date",
                         "Select Date:",
                         choices = generate_date_choices(),
                         selected = "2022-03-18"),
             
             hr(),
             
             h4("Zoom Controls"),
             
             numericInput("start_hour",
                          "Start Hour (24h):",
                          value = 6, min = 0, max = 23, step = 1),
             
             numericInput("end_hour",
                          "End Hour (24h):",
                          value = 18, min = 0, max = 23, step = 1),
             
             checkboxInput("auto_zoom",
                           "Auto-zoom to flux events",
                           value = FALSE),
             
             hr(),
             
             h4("Dataset Controls"),
             
             checkboxGroupInput("show_datasets",
                                "Show flux events from:",
                                choices = c("Tree measurements" = "Tree",
                                            "Soil/Water measurements" = "Soil/Water"),
                                selected = c("Tree", "Soil/Water")),
             
             hr(),
             
             h4("Current Offset:"),
             verbatimTextOutput("current_offset"),
             
             br(),
             
             actionButton("save_offset", "Save This Offset", 
                          class = "btn-success btn-block")
           )
    ),
    
    column(9,
           tabsetPanel(
             tabPanel("Daily Alignment",
                      h4(textOutput("date_title")),
                      plotlyOutput("daily_plot", height = "500px"),
                      br(),
                      h5("Flux Events This Day:"),
                      tableOutput("daily_flux_table")
             ),
             
             tabPanel("Period Overview", 
                      plotlyOutput("overview_plot", height = "600px")),
             
             tabPanel("Data Summary",
                      h4("All Flux Events by Date"),
                      DT::dataTableOutput("all_flux_table"))
           )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values to store data
  values <- reactiveValues()
  
  # Load data when app starts
  observe({
    # Load both flux datasets
    values$flux_data <- prepare_flux_data()
    
    # Debug: Print flux data info
    cat("Combined flux data loaded:", nrow(values$flux_data), "rows\n")
    cat("Tree events:", sum(values$flux_data$dataset == "Tree"), "\n")
    cat("Soil/Water events:", sum(values$flux_data$dataset == "Soil/Water"), "\n")
    
    # Print date range summary
    date_summary <- values$flux_data %>%
      group_by(date_parsed) %>%
      summarise(count = n(), .groups = "drop")
    cat("Events by date:\n")
    print(date_summary)
    
    # Load Picarro data from existing analyzer_data list
    if(exists("analyzer_data", envir = .GlobalEnv) && "Picarro" %in% names(analyzer_data)) {
      values$picarro_data <- analyzer_data[["Picarro"]] %>%
        filter(
          (year(POSIX.time) == 2022 & month(POSIX.time) == 3 & day(POSIX.time) >= 18 & day(POSIX.time) <= 24) |
            (year(POSIX.time) == 2022 & month(POSIX.time) == 10 & day(POSIX.time) >= 17 & day(POSIX.time) <= 25)
        )
      cat("Picarro data loaded:", nrow(values$picarro_data), "rows\n")
      cat("Date range:", as.character(range(values$picarro_data$POSIX.time)), "\n")
    } else {
      cat("ERROR: analyzer_data not found or doesn't contain 'Picarro'\n")
      if(exists("analyzer_data", envir = .GlobalEnv)) {
        cat("Available analyzer keys:", names(analyzer_data), "\n")
      } else {
        cat("analyzer_data object not found in global environment\n")
      }
    }
  })
  
  # Calculate total offset
  total_offset_seconds <- reactive({
    (input$offset_minutes * 60) + input$offset_seconds
  })
  
  # Apply offset to Picarro data
  adjusted_picarro <- reactive({
    req(values$picarro_data)
    
    values$picarro_data %>%
      mutate(POSIX.time_adjusted = POSIX.time + total_offset_seconds())
  })
  
  # Filter for selected day with time zoom
  daily_picarro <- reactive({
    req(adjusted_picarro())
    target_date <- as.Date(input$focus_date)
    
    daily_data <- adjusted_picarro() %>%
      filter(as.Date(POSIX.time_adjusted) == target_date)
    
    # Apply time zoom if specified
    if(isFALSE(input$auto_zoom) && nrow(daily_data) > 0) {
      start_time <- as.POSIXct(paste(target_date, paste0(input$start_hour, ":00:00")))
      end_time <- as.POSIXct(paste(target_date, paste0(input$end_hour, ":59:59")))
      
      daily_data <- daily_data %>%
        filter(POSIX.time_adjusted >= start_time, 
               POSIX.time_adjusted <= end_time)
    }
    
    daily_data
  })
  
  daily_flux <- reactive({
    req(values$flux_data)
    target_date <- as.Date(input$focus_date)
    
    flux_day <- values$flux_data %>%
      filter(as.Date(start_datetime) == target_date) %>%
      filter(dataset %in% input$show_datasets)  # Filter by selected datasets
    
    # Auto-zoom to flux events if enabled
    if(isTRUE(input$auto_zoom) && nrow(flux_day) > 0) {
      # Find time range of flux events with 1-hour padding
      flux_start <- min(flux_day$start_datetime) - hours(1)
      flux_end <- max(flux_day$end_datetime) + hours(1)
      
      # Update the hour inputs (for display purposes)
      updateNumericInput(session, "start_hour", value = hour(flux_start))
      updateNumericInput(session, "end_hour", value = hour(flux_end))
    }
    
    flux_day
  })
  
  # Date title
  output$date_title <- renderText({
    format(as.Date(input$focus_date), "%A, %B %d, %Y")
  })
  
  # Daily plot
  output$daily_plot <- renderPlotly({
    req(daily_picarro())
    
    picarro_day <- daily_picarro()
    flux_day <- daily_flux()
    
    if(nrow(picarro_day) == 0) {
      return(plot_ly() %>% 
               add_text(text = "No Picarro data for this date", 
                        x = 0.5, y = 0.5) %>%
               layout(xaxis = list(showgrid = FALSE), 
                      yaxis = list(showgrid = FALSE)))
    }
    
    # Base plot with CH4 data
    p <- plot_ly(picarro_day) %>%
      add_lines(x = ~POSIX.time_adjusted, 
                y = ~CH4dry_ppb,
                name = "CH4 (ppb)",
                line = list(color = "#2E86AB", width = 2),
                hovertemplate = "<b>Time:</b> %{x}<br><b>CH4:</b> %{y:.1f} ppb<extra></extra>") %>%
      layout(
        xaxis = list(title = "Time of Day", 
                     tickformat = "%H:%M"),
        yaxis = list(title = "CH4 (ppb)"),
        hovermode = "x unified"
      )
    
    # Add flux start times as colored vertical lines
    if(nrow(flux_day) > 0) {
      y_range <- range(picarro_day$CH4dry_ppb, na.rm = TRUE)
      y_padding <- diff(y_range) * 0.1
      
      # Use different colors for different datasets
      colors <- c("Tree" = "#E63946", "Soil/Water" = "#06D6A0")
      
      for(i in 1:nrow(flux_day)) {
        dataset_type <- flux_day$dataset[i]
        line_color <- colors[dataset_type]
        
        p <- p %>%
          add_segments(
            x = flux_day$start_datetime[i],
            xend = flux_day$start_datetime[i],
            y = y_range[1] - y_padding,
            yend = y_range[2] + y_padding,
            line = list(color = line_color, width = 3),
            name = if(i == 1 || !dataset_type %in% flux_day$dataset[1:(i-1)]) {
              paste(dataset_type, "Flux")
            } else "",
            showlegend = (i == 1 || !dataset_type %in% flux_day$dataset[1:(i-1)]),
            hovertemplate = paste0("<b>", dataset_type, " Flux:</b> ", flux_day$measurement_id[i], 
                                   "<br><b>Component:</b> ", flux_day$component_display[i],
                                   if(!is.na(flux_day$species[i])) paste0("<br><b>Species:</b> ", flux_day$species[i]) else "",
                                   if(!is.na(flux_day$plot_info[i])) paste0("<br><b>Plot:</b> ", flux_day$plot_info[i]) else "",
                                   "<br><b>Time:</b> %{x}<extra></extra>")
          )
      }
    }
    
    p
  })
  
  # Overview plot
  output$overview_plot <- renderPlotly({
    req(adjusted_picarro(), values$flux_data)
    
    overview_picarro <- adjusted_picarro()
    overview_flux <- values$flux_data
    
    if(nrow(overview_picarro) == 0) {
      return(plot_ly() %>% add_text(text = "No Picarro data available"))
    }
    
    # Create plot
    p <- plot_ly() %>%
      add_lines(data = overview_picarro,
                x = ~POSIX.time_adjusted,
                y = ~CH4dry_ppb,
                name = "CH4 (ppb)",
                line = list(color = "#2E86AB", width = 1)) %>%
      layout(
        title = "CH4 Data - March & October 2022 Overview",
        xaxis = list(title = "Date & Time"),
        yaxis = list(title = "CH4 (ppb)")
      )
    
    # Add all flux events with different colors
    if(nrow(overview_flux) > 0) {
      y_range <- range(overview_picarro$CH4dry_ppb, na.rm = TRUE)
      colors <- c("Tree" = "#E63946", "Soil/Water" = "#06D6A0")
      
      # Filter flux by selected datasets
      overview_flux_filtered <- overview_flux %>%
        filter(dataset %in% input$show_datasets)
      
      if(nrow(overview_flux_filtered) > 0) {
        for(dataset_type in unique(overview_flux_filtered$dataset)) {
          dataset_events <- overview_flux_filtered %>% filter(dataset == dataset_type)
          
          p <- p %>%
            add_segments(data = dataset_events,
                         x = ~start_datetime,
                         xend = ~start_datetime,
                         y = y_range[1],
                         yend = y_range[2],
                         line = list(color = colors[dataset_type], width = 2),
                         name = paste(dataset_type, "Events"),
                         showlegend = TRUE)
        }
      }
    }
    
    p
  })
  
  # Current offset display
  output$current_offset <- renderText({
    offset_sec <- total_offset_seconds()
    offset_min <- round(offset_sec / 60, 2)
    paste0(offset_sec, " seconds\n(", offset_min, " minutes)")
  })
  
  # Daily flux table
  output$daily_flux_table <- renderTable({
    flux_day <- daily_flux()
    
    if(nrow(flux_day) > 0) {
      table_data <- flux_day %>%
        select(
          Dataset = dataset,
          ID = measurement_id,
          Start = start_time_display,
          End = end_time_display,
          Type = component_display
        ) %>%
        arrange(Start)
      
      # Add additional columns if available
      if("species" %in% names(flux_day) && any(!is.na(flux_day$species))) {
        table_data$Species <- flux_day$species
      }
      
      if("plot_info" %in% names(flux_day) && any(!is.na(flux_day$plot_info))) {
        table_data$Plot <- flux_day$plot_info
      }
      
      table_data
    } else {
      data.frame(Message = "No flux measurements this day")
    }
  }, striped = TRUE, hover = TRUE)
  
  # All flux events table
  output$all_flux_table <- DT::renderDataTable({
    req(values$flux_data)
    
    values$flux_data %>%
      select(
        Date = date_parsed,
        Dataset = dataset,
        ID = measurement_id,
        Start = start_time_display,
        End = end_time_display,
        Type = component_display,
        Species = species,
        Plot = plot_info
      ) %>%
      arrange(Date, Start)
  }, options = list(pageLength = 25, scrollX = TRUE))
  
  # Save offset action
  observeEvent(input$save_offset, {
    offset <- total_offset_seconds()
    showModal(modalDialog(
      title = "Offset Saved!",
      paste0("Current offset of ", offset, " seconds has been noted.\n\n",
             "Apply this offset to your full dataset:\n",
             "picarro_corrected <- picarro_data %>%\n",
             "  mutate(POSIX.time_corrected = POSIX.time + ", offset, ")"),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
  })
}

# Launch the app
shinyApp(ui = ui, server = server)

# SETUP INSTRUCTIONS:
# 
# 1. Make sure analyzer_data[["Picarro"]] is loaded in your R session
# 
# 2. Verify file paths are correct
# 
# 3. Run the app
# 
# DATE RANGES INCLUDED:
# - March 18-24, 2022
# - October 17-25, 2022
# 
# FEATURES:
# - ±12 hours offset range
# - Time zoom controls
# - Two datasets: Tree (red) & Soil/Water (green) 
# - Dataset toggle controls
# - Daily and overview tabs
# - Data summary table with all events