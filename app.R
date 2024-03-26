library(shiny)
library(ggmap)
library(gganimate) 
library(gifski)
library(data.table) 
library(forecast)

# Define the user interface for the app
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
            body {
              background: url('merge/fire1.jpg') no-repeat center center fixed;
              background-size: cover; /* Cover the entire page */
              font-family: 'Arial', sans-serif;
            }
            .navbar, .sidebar .well {
              background-color: #ffffff;
              border-radius: 0;
            }
            .navbar {
              box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
              margin-bottom: 20px;
            }
            .navbar-default .navbar-brand {
              color: #337ab7;
              font-size: 24px;
              font-weight: bold;
              margin-left: 20px;
            }
            .navbar-default {
              border-color: #fff;
              background-color: #fff;
            }
            .btn-action {
              background-color: #4CAF50;
              border-color: #4CAF50;
              color: #ffffff;
            }
            .btn-action:hover {
              background-color: #45a049;
              border-color: #45a049;
            }
            .well {
              box-shadow: none;
              border: 1px solid #ddd;
            }
            #fireGIF_container {
              border: 2px solid #337ab7;
              background-color: #ffffff;
              padding: 10px;
              box-shadow: 0 2px 4px rgba(0,0,0,0.2);
            }
            .control-label {
              font-weight: bold;
            }
            .sidebar .form-group {
              background-color: #e9ecef; /* Add a light gray background */
              padding: 8px 10px;
              border-radius: 4px;
              margin-bottom: 10px; /* Add some space at the bottom */
            }
            .sidebar .btn {
              width: 100%;
              margin-top: 10px;
            }
            /* Additional styling for the date inputs */
            .sidebar .shiny-date-input {
              background-color: #f7f7f7; /* Lighter grey background for date inputs */
              border: 3px solid #cccccc; /* Slightly darker border for the date inputs */
              border-radius: 4px; /* Rounded borders for the date inputs */
              padding: 4px; /* Padding inside the date input boxes */
            }
        ")),
    tags$link(rel="stylesheet", type="text/css", href="bootstrap.min.css")
  ),
  
  # App title in the navbar
  tags$nav(class="navbar navbar-default",
           tags$div(class="container-fluid",
                    tags$div(class="navbar-header",
                             tags$a(class="navbar-brand", href="#", "Active Fire Maps with R")
                    )
           )
  ),
  
  sidebarLayout(
    sidebarPanel(
      # Input: Specify start and end date
      dateInput("start_date", "Start Date:", value = Sys.Date() - 10),
      dateInput("end_date", "End Date:", value = Sys.Date()),
      # Input: Button to generate map
      actionButton("goButton", "Generate Map", class = "btn-action"),
      # Add this inside your sidebarLayout's sidebarPanel in the ui object
      downloadButton("downloadData", "Download Data", class = "btn-action")
      
    ),
    mainPanel( 
      textOutput("totalFires"),
      # Output: Display the generated fire map GIF inside a container with blue border
      tags$div(id = "fireGIF_container",
               imageOutput("fireGIF", height = "auto") # Set auto height for responsive GIF display
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive value to store the fire data
  fire_data <- reactiveVal()
  
  # Function to fetch fire data
  get_fire_data <- function(main_url, map_key, source, area, start_date, end_date) {
    day_range <- as.integer(as.Date(end_date) - as.Date(start_date)) + 1
    url <- paste0(main_url, "/", map_key, "/", source, "/", area, "/", day_range, "/", start_date)
    return(fread(url))
  }
  
  # Observe the click of the 'Generate Map' button
  observeEvent(input$goButton, {
    if (difftime(input$end_date, input$start_date, units = "days") > 10) {
      showModal(modalDialog(
        title = "Error",
        "The date range should not exceed 10 days.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    # Fetch the data from the API and store it in 'fire_data'
    fire_data(get_fire_data(
      main_url = "https://firms.modaps.eosdis.nasa.gov/api/area/csv",
      map_key = "0ca7e809b22eafa273116a3832ce032d", # Replace with your actual map key
      source = "VIIRS_SNPP_NRT",
      area = "world",
      start_date = format(input$start_date, "%Y-%m-%d"),
      end_date = format(input$end_date, "%Y-%m-%d")
    ))
  }) 
  output$totalFires <- renderText({
    paste("Total number of fires detected:", nrow(fire_data()))
  })
  
  # Render the GIF image
  output$fireGIF <- renderImage({
    req(fire_data())
    
    data <- fire_data()
    
    if (nrow(data) == 0) {
      stop("No fire data available for this date range.")
    }
    
    # Convert temperatures from Kelvin to Celsius
    data$temperature <- data$bright_ti5 - 273.15
    
    # Define the file name for the GIF
    gif_file <- tempfile(fileext = ".gif")
    data$acq_date <- as.Date(data$acq_date, format = "%Y-%m-%d")
    
    # Create a ggplot object with fire data
    p <- ggplot(data, aes(x = longitude, y = latitude, color = temperature)) +
      geom_point() +
      scale_color_gradient(low = "yellow", high = "red") +
      labs(title = "Active Fires", x = "Longitude", y = "Latitude") +
      theme_minimal() +
      transition_time(acq_date) +
      labs(subtitle = 'Date: {format(frame_time, "%Y-%m-%d")}')
    
    
    # Animate the ggplot object and save as GIF
    anim <- animate(p, nframes = 100, fps = 10, width = 800, height = 600, renderer = gifski_renderer())
    anim_save(gif_file, animation = anim)
    
    # Return the path to the generated GIF for rendering in UI
    list(src = gif_file, contentType = "image/gif", alt = "Fire Map Animation")
  }, deleteFile = TRUE)
  
  # Download handler to allow users to download the data as a CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("nasa_fire_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(fire_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)

