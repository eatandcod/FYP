library(shiny)
library(tidyverse)
library(data.table)
library(ggmap)
library(gganimate)
library(shinyWidgets)

# Define the function to get fire data outside of server function
get_fire_data <- function(main_url, map_key, source, area, start_date, end_date) {
  fire_data_list <- list()  
  current_date <- as.Date(start_date)
  
  while (current_date <= as.Date(end_date)) {
    interval_end <- min(current_date + 9, as.Date(end_date))
    url <- sprintf("%s/%s/%s/%s/%d/%s", main_url, map_key, source, area, 10, format(current_date, "%Y-%m-%d"))
    tryCatch({
      data <- data.table::fread(url)
      if (nrow(data) == 0) {
        # If no data is returned, create an empty data table with the correct types
        data <- data.table(
          latitude = numeric(),
          longitude = numeric(),
          bright_ti4 = numeric(),
          scan = numeric(),
          track = numeric(),
          acq_date = IDate(character()),
          acq_time = integer(),
          satellite = character(),
          instrument = character(),
          confidence = character(),
          version = character(),
          bright_ti5 = numeric(),
          frp = numeric(),
          daynight = character()
        )
      }
      fire_data_list[[length(fire_data_list) + 1]] <- data
    }, error = function(e) {
      message(sprintf("Error retrieving data for date: %s", format(current_date, "%Y-%m-%d")))
    })
    current_date <- interval_end + 1
  }
  
  # Combine all data frames in the list into one data frame with matching types
  fire_data <- rbindlist(fire_data_list, fill = TRUE, use.names = TRUE)
  return(fire_data)
}


# Define UI
ui <- fluidPage(
  titlePanel("Active Fire Map Viewer"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dateRange",
                     "Select Date Range:",
                     start = Sys.Date() - 30, end = Sys.Date()
      ),
      actionButton("update", "Update Map")
    ),
    mainPanel(
      uiOutput("animatedMap")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$update, {
    # Define area
    xmin <- 11.4
    ymin <- 36.6
    xmax <- 18.7
    ymax <- 42
    area_coords <- c(xmin, ymin, xmax, ymax)
    area <- paste(area_coords, collapse = ",")
    
    main_url <- "https://firms.modaps.eosdis.nasa.gov/api/area/csv"
    map_key <- "0ca7e809b22eafa273116a3832ce032d" # Use the provided map key
    source <- "VIIRS_SNPP_NRT"
    start_date <- input$dateRange[1]
    end_date <- input$dateRange[2]
    
    # Call the get_fire_data function with the correct arguments
    fire_data <- get_fire_data(main_url, map_key, source, area, start_date, end_date)
    
    # Check if the fire_data object has any rows
    if(nrow(fire_data) > 0) {
      # Process the fire_data and create the animated map as before
      # ... rest of the code to create the animated map ...
    } else {
      output$animatedMap <- renderUI({
        h4("No fire data available for the selected period.")
      })
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)