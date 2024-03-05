library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(htmlwidgets)
library(RColorBrewer)

# Define the Rothermel model function
rothermel_model <- function(IR, phi_w, phi_s, rho_b, epsilon, Q_ig) {
  (IR * (1 + phi_w + phi_s)) / (rho_b * epsilon * Q_ig)
}

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap"),
    tags$style(HTML("
      body, h1, h2, h3, h4, h5, h6, .navbar .navbar-brand {
        font-family: 'Roboto', sans-serif;
      }
      .shiny-app-title {
        color: #ffffff;
        background-color: #333333;
        text-align: center;
        padding: 20px;
        font-size: 2em;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 2px;
      }
      .sidebar {
        background-color: #252526; /* Dark gray */
        padding: 20px;
        color: #ffffff;
        box-shadow: 0px 0px 8px yellow; /* Box shadow for inputs */
      }
      .sidebar .control-label {
        color: #ffffff;
      }
      .sidebar .form-control {
        background-color: #333333;
        border: none;
        color: #ffffff;
        box-shadow: 0px 0px 8px yellow; /* Box shadow for inputs */
      }
      .btn {
        border-radius: 30px;
        text-transform: uppercase;
        font-weight: 700;
        margin: 5px;
      }
      .btn-start {
        background-color: #21ba45; /* Green */
        color: #ffffff;
      }
      .btn-stop {
        background-color: #db2828; /* Red */
        color: #ffffff;
      }
      .leaflet-container {
        border-radius: 8px;
        margin-top: 20px;
      }
      .shiny-input-container {
        margin-bottom: 15px;
      }
      .leaflet-container {
        height: 600px;
      }
    "))
  ),
  div(class = "shiny-app-title", "Interactive Fire Spread Rate Choropleth Map"),
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      numericInput("IR", "Reaction Intensity", value = 2000),
      numericInput("phi_w", "Wind Factor", value = 0.4),
      numericInput("phi_s", "Slope Factor", value = 0.3),
      numericInput("rho_b", "Bulk Density", value = 40),
      numericInput("epsilon", "Effective Heating Number", value = 0.01),
      numericInput("Q_ig", "Heat of Pre-Ignition", value = 2500),
      actionButton("startBtn", "Start", class = "btn btn-start"),
      actionButton("stopBtn", "Stop", class = "btn btn-stop"),
      textOutput("spreadRateOutput")
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

server <- function(input, output, session) {
  running <- reactiveVal(FALSE)
  
  observeEvent(input$startBtn, {
    running(TRUE)
  })
  
  observeEvent(input$stopBtn, {
    running(FALSE)
  })
  
  observe({
    if (running()) {
      invalidateLater(5000, session)  # Update every 5 seconds
      
      # Update the input values randomly
      updateNumericInput(session, "IR", value = sample(1000:10000, 1))
      updateNumericInput(session, "phi_w", value = runif(1, 0, 1))
      updateNumericInput(session, "phi_s", value = runif(1, 0, 1))
      updateNumericInput(session, "rho_b", value = sample(10:100, 1))
      updateNumericInput(session, "epsilon", value = runif(1, 0.01, 0.1))
      updateNumericInput(session, "Q_ig", value = sample(2500:8000, 1))
      
      # Recalculate spread rate and update map
      spread_rate <- rothermel_model(input$IR, input$phi_w, input$phi_s, input$rho_b, input$epsilon, input$Q_ig)
      output$spreadRateOutput <- renderText({
        paste("Spread Rate: ", round(spread_rate, 2), "units")
      })
      
      # Update the map with new spread rate
      output$map <- renderLeaflet({
        data <- data.frame(lat = runif(10, -23, -22), long = runif(10, 150, 151))
        data$spread_rate <- rnorm(10, mean = spread_rate, sd = 5)
        
        # Use color palette that reflects fire colors 
        pal <- colorNumeric(palette = "RdYlBu", domain = NULL)
        
        NW.pal <- colorRampPalette(colors=c("red4", "yellow2"))
        
        
        leaflet(data) %>%
          addProviderTiles(providers$CartoDB.DarkMatter) %>%
          addHeatmap(lng = ~long, lat = ~lat, intensity = ~spread_rate, blur = 20, max = 0.05, radius = 15, 
                     gradient = NW.pal(5)) %>%
          addLegend("bottomright", pal = pal, values = ~spread_rate, title = "Spread Rate", labFormat = labelFormat(suffix = " units"))
        
      })
    }
  })
}

shinyApp(ui = ui, server = server)