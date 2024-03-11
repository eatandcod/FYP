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

# Function for calculating fire spread rate
rothermel_model <- function(IR, phi_w, phi_s, rho_b, epsilon, Q_ig) {
  (IR * (1 + phi_w + phi_s)) / (rho_b * epsilon * Q_ig)
}

# Assuming 'df' is loaded here (update path as necessary)
df <- read_csv("data/Brazilian-fire-dataset.csv")


# UI definition
ui <- dashboardPage( 
   
  dashboardHeader(title = "Combined App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Fire spread rate", tabName = "dashboard", icon = icon("fire")),
      menuItem("Parameters", icon = icon("sliders"),
               sliderInput("IR", "Reaction Intensity", min = 0, max = 10000, value = 2000),
               sliderInput("phi_w", "Wind Factor", min = 0, max = 1, value = 0.4),
               sliderInput("phi_s", "Slope Factor", min = 0, max = 1, value = 0.3),
               sliderInput("rho_b", "Bulk Density", min = 0, max = 100, value = 40),
               sliderInput("epsilon", "Effective Heating Number", min = 0, max = 1, step = 0.01, value = 0.01),
               sliderInput("Q_ig", "Heat of Pre-Ignition", min = 0, max = 10000, value = 2500)
      ),
      menuItem("Choropleth Map", tabName = "choroplethMap", icon = icon("globe")),
      menuItem("Forest Fire Analysis", tabName = "forestFireAnalysis", icon = icon("tree")), 
      menuItem("Real-time Data", tabName = "realtimeData", icon = icon("satellite-dish"))
      
    )
  ),
  dashboardBody( 
    useShinyjs(),
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
                tabPanel("Data Table", DT::dataTableOutput("fireTable"))
              )
      )
    ), 
    tabItem(tabName = "realtimeData",
            h2("Real-time Fire Data"),
            leafletOutput("realtimeMap")
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
  
  
  # Data Table
  output$fireTable <- DT::renderDataTable({
    req(input$selectedState)  # Require that this input is not NULL
    
    # Filter 'df' for the selected state and render it
    df %>% filter(State == input$selectedState)
  })
} 
observe({
  # This can be triggered by an input or run when the app is launched
  api_data <- reactiveVal()
  
  # Call the API at regular intervals
  autoInvalidate <- reactiveTimer(60000) # to call API every 60 seconds
  
  observe({
    autoInvalidate() # This reactive expression is invalidated every 60 seconds
    # API call, update api_data
    res <- httr::GET("https://api.example.com/data")
    # Error handling for the API call
    if(httr::status_code(res) == 200) {
      api_content <- httr::content(res, as = "text", encoding = "UTF-8")
      # Assuming the API returns JSON
      api_json <- jsonlite::fromJSON(api_content)
      api_data(api_json)
    } else {
      # Error handling here
      api_data(NULL)
    }
  })
  
  output$apiDataDisplay <- renderUI({
    # Render the API data in the UI, could be a table or a plot
    # Check if api_data() is not NULL and then proceed
    api_dat <- api_data()
    if(!is.null(api_dat)) {
      # Convert the data to a dataframe or a format suitable for rendering in the UI
      # Return a UI element such as a plot or table created with the data
    }
  })
}) 
# This observe block could be used to periodically call an API
library(httr)
library(jsonlite)
library(leaflet)

# Example placeholder for the FIRMS API URL, adjust according to the actual API documentation
api_url <- paste0("https://firms.modaps.eosdis.nasa.gov/api/endpoint?api_key=", "0ca7e809b22eafa273116a3832ce032d")

# Function to fetch data
fetchData <- function() {
  response <- GET(api_url)
  if(status_code(response) == 200) {
    content <- content(response, "text")
    data <- fromJSON(content)
    return(data)
  } else {
    return(NULL)
  }
}

# In your Shiny server function, call this fetchData() function periodically
output$realtimeMap <- renderLeaflet({
  data <- fetchData()
  if(!is.null(data)) {
    # Assuming 'data' contains latitude and longitude
    leaflet() %>% 
      addTiles() %>%
      addMarkers(lng = data$longitude, lat = data$latitude)
  }
})




shinyApp(ui, server)
