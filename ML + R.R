library(shinydashboard)
library(plotly)
library(leaflet)
library(DT)
library(ggplot2)
library(readr) # If needed for reading data
library(dplyr)
library(rgl) # For 3D plots
library(shinythemes)
library(shinyWidgets)
library(leaflet.extras) 
library(shinyjs) 
library(beepr)
library(httr)
library(jsonlite) 
library(zoo)


# Function for calculating fire spread rate
rothermel_model <- function(IR, phi_w, phi_s, rho_b, epsilon, Q_ig) {
  (IR * (1 + phi_w + phi_s)) / (rho_b * epsilon * Q_ig)
}

# Assuming 'df' is loaded here (update path as necessary)
df <- read_csv("data/Brazilian-fire-dataset.csv")


# UI definition
library(shinydashboard)

# UI definition
ui <- dashboardPage(
  dashboardHeader(title = "Combined App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Fire spread rate", tabName = "dashboard", icon = icon("fire")),
      menuItem("Parameters", tabName = "parameters", icon = icon("sliders")),
      menuItem("Choropleth Map", tabName = "choroplethMap", icon = icon("globe")),
      menuItem("Forest Fire Analysis", tabName = "forestFireAnalysis", icon = icon("tree")),
      menuItem("Real-time Data", tabName = "activeFireMaps", icon = icon("satellite-dish")),
      menuItem("Fire Prediction in Kyrgyzstan", tabName = "kyrgyzstanPrediction", icon = icon("fire-alt"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Control Panel", status = "primary", solidHeader = TRUE, width = 12,
                    actionButton("startBtn", "Start", icon = icon("play"), class = "btn-success"),
                    actionButton("stopBtn", "Stop", icon = icon("stop"), class = "btn-danger"),
                    textOutput("spreadRateOutput")
                )
              ),
              fluidRow(
                box(title = "Factor Influence on Spread Rate", status = "info", solidHeader = TRUE, width = 6,
                    plotlyOutput("factorPlot")
                ),
                box(title = "Fire Spread Visualization", status = "warning", solidHeader = TRUE, width = 6,
                    rglwidgetOutput("plot3D")
                )
              )
      ),
      tabItem(tabName = "parameters",
              sidebarPanel(
                sliderInput("IR", "Reaction Intensity", min = 0, max = 10000, value = 2000),
                sliderInput("phi_w", "Wind Factor", min = 0, max = 1, value = 0.4),
                sliderInput("phi_s", "Slope Factor", min = 0, max = 1, value = 0.3),
                sliderInput("rho_b", "Bulk Density", min = 0, max = 100, value = 40),
                sliderInput("epsilon", "Effective Heating Number", min = 0, max = 1, step = 0.01, value = 0.01),
                sliderInput("Q_ig", "Heat of Pre-Ignition", min = 0, max = 10000, value = 2500)
              )
      ),
      tabItem(tabName = "choroplethMap",
              h2("Interactive Choropleth Map"),
              mainPanel(leafletOutput("map"))
      ),
      tabItem(tabName = "forestFireAnalysis",
              h2("Forest Fire Analysis"),
              selectInput("selectedState", "Select State:", choices = unique(df$State)),
              selectInput("selectedYear", "Select Year:", choices = unique(df$Year)),
              tabsetPanel(
                tabPanel("Trend Analysis", plotlyOutput("trendPlot")), 
                tabPanel("State Comparison", plotlyOutput("stateComparisonPlot")),
                tabPanel("Data Table", DT::dataTableOutput("fireTable"))
              )
      ),  
      sidebarLayout(
        sidebarPanel(
          dateInput("start_date", "Start Date:", value = Sys.Date() - 10),
          dateInput("end_date", "End Date:", value = Sys.Date()),
          actionButton("goButton", "Generate Map", class = "btn-action"),
          br(),  # Adds a line break for space
          downloadButton("downloadData", "Download Data", class = "btn-action")
        ),
      tabItem(tabName = "kyrgyzstanPrediction",
              h2("Fire Prediction in Kyrgyzstan"),
              mainPanel( 
                textOutput("totalFires"),
                plotOutput("fireForecast"),
                tags$div(id = "fireGIF_container",
                         imageOutput("fireGIF", height = "auto")
      ),
      tabItem(tabName = "activeFireMaps",
              titlePanel("Active Fire Maps with R"),
              sidebarLayout(
                sidebarPanel(
                  dateInput("start_date", "Start Date:", value = Sys.Date() - 10),
                  dateInput("end_date", "End Date:", value = Sys.Date()),
                  actionButton("goButton", "Generate Map"), 
                  downloadButton("downloadData", "Download Data", class = "btn-primary")
                ),
                mainPanel(  
                  textOutput("arimaOrder"),
                  plotOutput("fireForecast"),
                  imageOutput("fireGIF")
                )
              )
      )
    )
  )
)




# Server logic
server <- function(input, output, session) {
  running <- reactiveVal(FALSE)
  
  observeEvent(input$startBtn, { running(TRUE) })
  observeEvent(input$stopBtn, { running(FALSE) })
  
  spreadRate <- reactive({
    if(running()) {
      rothermel_model(input$IR, input$phi_w, input$phi_s, input$rho_b, input$epsilon, input$Q_ig)
    } else {
      NULL
    }
  })
  
  output$spreadRateOutput <- renderText({
    rate <- spreadRate()
    if (!is.null(rate)) {
      paste("Spread Rate:", round(rate, 2), "units")
    } else {
      "Spread Rate: Not running"
    }
  })
  
  output$plot3D <- renderRglwidget({
    rate <- spreadRate()
    if (!is.null(rate)) {
      clear3d()
      x <- seq(-10, 10, length.out = 100)
      y <- seq(-10, 10, length.out = 100)
      z <- outer(x, y, function(x, y) { rate * exp(-0.1*(x^2 + y^2)) })
      surface3d(x, y, z, color = rainbow(10000))
      rglwidget()
    }
  })
  
  output$factorPlot <- renderPlotly({
    rate <- spreadRate()
    if (!is.null(rate)) {
      data <- data.frame(
        FactorValue = c(input$IR, input$phi_w * 100, input$phi_s * 100),
        SpreadRate = rep(rate, 3),
        FactorType = c("Reaction Intensity", "Wind Factor", "Slope Factor")
      )
      plot_ly(data, x = ~FactorValue, y = ~SpreadRate, type = 'scatter', mode = 'markers',
              marker = list(size = 10), color = ~FactorType, colors = "Set1") %>%
        layout(title = "Factors Influencing Spread Rate",
               xaxis = list(title = "Factor Value"),
               yaxis = list(title = "Spread Rate"))
    }
  })
  
  # Update the heatmap based on running status and spread rate changes
  spreadRate <- reactive({
    if(running()) {
      rothermel_model(input$IR, input$phi_w, input$phi_s, input$rho_b, input$epsilon, input$Q_ig)
    } else {
      0  # Default to 0 if not running
    }
  }) 
  output$map <- renderLeaflet({
    req(spreadRate())  # ensure that spreadRate is available
    
    # Simulate the 'df' with 'lat' and 'long' columns
    df_simulated <- data.frame(
      lat = runif(100, -23.5, -22.9),  # random latitudes in a plausible range
      long = runif(100, -46.6, -46.4),  # random longitudes in a plausible range
      intensity = runif(100, min = 0, max = spreadRate())  # intensity based on spread rate
    )
    
    # Create a leaflet map with a black tile layer
    leaflet(df_simulated) %>%
      addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png",
               options = tileOptions(minZoom = 1, maxZoom = 18, attribution = "")) %>%
      addHeatmap(
        lng = ~long, lat = ~lat, intensity = ~intensity,
        blur = 20, max = 1, radius = 15,
        gradient = c(low = "yellow", high = "red")  # color gradient from low to high intensity
      )
  }) 
  high_spread_rate_threshold <- 20  # Adjust this threshold as needed
  
  # Observe the spread rate and trigger an alarm if necessary
  observe({
    spread_rate_value <- spreadRate() # Function to get the current spread rate
    if(!is.null(spread_rate_value) && spread_rate_value > high_spread_rate_threshold) {
      # Use shinyjs to run custom JavaScript for sound
      beep(sound = "shotgun")
      
      # Show a danger message using Shiny's built-in notification system
      showNotification("Danger! High spread rate detected.", type = "error")
    }
  })
  
  
  # Update select inputs for forest fire analysis
  df <- read_csv("data/Brazilian-fire-dataset.csv")
  
  # Update the choices for the select input for Year based on the selected State
  # Update the choices for the select input for Year based on the selected State
  observeEvent(input$selectedState, {
    state_specific_years <- unique(df$Year[df$State == input$selectedState])
    updateSelectInput(session, "selectedYear", choices = state_specific_years)
  }, ignoreNULL = FALSE)
  
  # Trend Plot
  output$trendPlot <- renderPlotly({
    req(input$selectedState, input$selectedYear)  # Require that these inputs are not NULL
    
    filtered_df <- df %>%
      filter(State == input$selectedState, Year == as.numeric(input$selectedYear)) %>%
      mutate(Month = factor(Month, levels = c("January", "February", "March", "April", "May", "June", 
                                              "July", "August", "September", "October", "November", "December")))
    
    p <- ggplot(filtered_df, aes(x = Month, y = `Number of Fires`, group = 1)) +
      geom_line(color = "steelblue") +
      geom_point(color = "darkorange", size = 3) +
      theme_minimal() +
      labs(title = paste("Fire Trends in", input$selectedState, input$selectedYear),
           x = "Month", y = "Number of Fires") +
      scale_x_discrete(limits = levels(filtered_df$Month)) # Ensure months are in order
    
    # Convert to plotly object
    ggplotly(p) %>%
      layout(hovermode = 'closest')  # Enable hover for the closest data point
  }) 
  selected_data <- reactive({
    req(input$selectedState, input$selectedYear)
    filtered_df <- df %>%
      filter(State == input$selectedState, Year == as.numeric(input$selectedYear))
    return(filtered_df)
  })
  
  # Render the data table
  output$fireTable <- DT::renderDataTable({
    DT::datatable(selected_data(), options = list(pageLength = 5))
  }) 
  output$stateComparisonPlot <- renderPlotly({
    req(input$selectedYear)  # Ensure that the selectedYear input is not NULL
    
    # Filter the data for the selected year across all states
    yearly_data <- df %>%
      filter(Year == as.numeric(input$selectedYear))
    
    # Create a plot comparing the number of fires across states
    p <- ggplot(yearly_data, aes(x = State, y = `Number of Fires`, fill = State)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = paste("Comparison of Fire Occurrences in", input$selectedYear),
           x = "State", y = "Number of Fires")
    
    ggplotly(p)
  })
  
  
  
  # Data Table
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
  
  output$fireForecast <- renderPlot({
    req(fire_data())
    daily_fire_count <- aggregate(fire_data()[, "acq_date"], by=list(Date=fire_data()$acq_date), FUN=length)
    colnames(daily_fire_count) <- c("Date", "Count")
    
    # Make sure 'Date' is in the correct format
    daily_fire_count$Date <- as.Date(daily_fire_count$Date)
    
    # Use ts() function with frequency that represents your data, e.g. 365 for daily data
    fire_ts <- ts(daily_fire_count$Count, start=c(year(daily_fire_count$Date[1]), month(daily_fire_count$Date[1])), frequency=365)
    
    # Fit an ARIMA model
    fit <- auto.arima(fire_ts)
    
    # Forecast the next period
    future <- forecast(fit, h=10)
    
    # Plot the forecast
    autoplot(future) + 
      xlab("Date") + 
      ylab("Predicted Fire Count") +
      ggtitle("30-Day Fire Forecast") +
      theme(plot.title = element_text(hjust = 0.5))
  }) 
  # Inside your server <- function(input, output, session) { ... }
  output$arimaOrder <- renderText({
    req(fire_data())  # Ensure that the fire data is available
    
    tryCatch({
      daily_fire_count <- aggregate(fire_data()[, "acq_date"], by=list(Date=fire_data()$acq_date), FUN=length)
      colnames(daily_fire_count) <- c("Date", "Count")
      
      daily_fire_count$Date <- as.Date(daily_fire_count$Date)
      
      fire_ts <- ts(daily_fire_count$Count, start=c(year(daily_fire_count$Date[1]), month(daily_fire_count$Date[1])), frequency=365)
      
      fit <- auto.arima(fire_ts)
      p <- fit$order[1]
      d <- fit$order[2]
      q <- fit$order[3]
      order <- paste("ARIMA(", p, ",", d, ",", q, ")", sep = "")
      order  # Display ARIMA order as a text output
    }, error = function(e) {
      paste("Error fitting ARIMA model:", e$message)
    })
  })
  
  
  
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
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("nasa_fire_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(fire_data(), file, row.names = FALSE)
    }
  ) 
  initial_fire_data_kyrgyzstan <- read.csv("C:/Users/azra.nisar/Downloads/viirs-snpp_2021_Kyrgyzstan.csv", stringsAsFactors = FALSE)
  
  # Convert dates in the data to Date type and remove rows where acq_date is NA
  initial_fire_data_kyrgyzstan$acq_date <- as.Date(initial_fire_data_kyrgyzstan$acq_date)
  fire_data_kyrgyzstan <- na.omit(initial_fire_data_kyrgyzstan)
  
  # Reactive expression to filter the fire data based on input dates for Kyrgyzstan
  filtered_fire_data_kyrgyzstan <- reactive({
    req(input$start_date, input$end_date)
    filtered_data <- fire_data_kyrgyzstan %>%
      filter(acq_date >= as.Date(input$start_date) & acq_date <= as.Date(input$end_date))
    return(filtered_data)
  })
  
  # Output for total number of fires in Kyrgyzstan
  output$totalFiresKyrgyzstan <- renderText({
    paste("Total number of fires detected in Kyrgyzstan:", nrow(filtered_fire_data_kyrgyzstan()))
  })
  
  # Output for fire forecast plot in Kyrgyzstan
  output$kyrgyzstanForecast <- renderPlot({
    req(filtered_fire_data_kyrgyzstan())
    daily_fire_count <- aggregate(filtered_fire_data_kyrgyzstan()[, "acq_date"], 
                                  by = list(Date = filtered_fire_data_kyrgyzstan()$acq_date), 
                                  FUN = length)
    colnames(daily_fire_count) <- c("Date", "Count")
    
    # Convert to a ts object assuming daily data
    fire_ts <- ts(daily_fire_count$Count, frequency = 365)
    
    # Fit an ARIMA model
    fit <- auto.arima(fire_ts)
    
    # Forecast the next period
    future <- forecast(fit, h = 30)
    
    # Plot the forecast
    plot(future)
  })
  
  # Output for animated fire GIF in Kyrgyzstan (if you have such functionality)
  output$kyrgyzstanFireGIF <- renderImage({
    req(filtered_fire_data_kyrgyzstan())
    # ... your code for creating and saving the GIF ...
    # Ensure to return the path to the generated GIF
  }, deleteFile = TRUE)
  
}

shinyApp(ui, server)
