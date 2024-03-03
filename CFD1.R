library(shiny)
library(rgl)
library(shinythemes)  
library(shinydashboard) 
library(shinyWidgets) 
library(plotly)
rothermel_model <- function(IR, phi_w, phi_s, rho_b, epsilon, Q_ig) {
  # IR: Reaction Intensity (kW/m^2)
  # phi_w: Wind factor
  # phi_s: Slope factor
  # rho_b: Bulk Density (kg/m^3)
  # epsilon: Effective Heating Number
  # Q_ig: Heat of Pre-Ignition (kJ/kg)
  
  # Calculate the propagating flux ratio
  xi <- IR / (rho_b * Q_ig)
  
  # Calculate the dimensionless coefficients
  phi_Ew <- phi_w # Wind coefficient
  phi_Es <- phi_s # Slope coefficient
  
  # Calculate the rate of spread
  ROS <- xi * (1 + phi_Ew + phi_Es) / epsilon
  
  return(ROS)
}

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Interactive Fire Spread Rate Map"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Settings", icon = icon("sliders"), 
               sliderInput("IR", "Reaction Intensity", min = 0, max = 10000, value = 2000),
               sliderInput("phi_w", "Wind Factor", min = 0, max = 1, value = 0.4),
               sliderInput("phi_s", "Slope Factor", min = 0, max = 1, value = 0.3),
               sliderInput("rho_b", "Bulk Density", min = 0, max = 100, value = 40),
               sliderInput("epsilon", "Effective Heating Number", min = 0, max = 1, step = 0.01, value = 0.01),
               sliderInput("Q_ig", "Heat of Pre-Ignition", min = 0, max = 10000, value = 2500)
      )
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
      )
    )
  )
)





# Define server logic
server <- function(input, output, session) {
  running <- reactiveVal(FALSE) 
  
  # Calculate spread rate using reactive expression
  spreadRate <- reactive({
    if(running()) {
      rothermel_model(input$IR, input$phi_w, input$phi_s, input$rho_b, input$epsilon, input$Q_ig)
    } else {
      NULL  # Return NULL when not running
    }
  })
  
  observeEvent(input$startBtn, { running(TRUE) })
  observeEvent(input$stopBtn, { running(FALSE) })
  
  output$spreadRateOutput <- renderText({
    rate <- spreadRate()
    if (!is.null(rate)) {
      paste("Spread Rate:", round(rate, 2), "units")
    } else {
      "Spread Rate: Not running"
    }
  })
  
  # Render the 3D plot
  output$plot3D <- renderRglwidget({
    rate <- spreadRate()
    if (!is.null(rate)) {
      clear3d()  # Clear any previous RGL scenes
      
      # Create some sample 3D data
      x <- seq(-10, 10, length.out = 100)
      y <- seq(-10, 10, length.out = 100)
      z <- outer(x, y, function(x, y) { rate * exp(-0.1*(x^2 + y^2)) })
      
      # Generate the plot
      surface3d(x, y, z, color = rainbow(10000))
      
      # Return the widget
      rglwidget() 
      
    }
  }) 
  output$factorPlot <- renderPlotly({
    rate <- spreadRate()
    if (!is.null(rate)) {
      data <- data.frame(
        FactorValue = c(input$IR, input$phi_w * 100, input$phi_s * 100),  # Scaled for visibility
        SpreadRate = rep(rate, 3),
        FactorType = c("Reaction Intensity", "Wind Factor", "Slope Factor")
      )
      
      p <- plot_ly(data, x = ~FactorValue, y = ~SpreadRate, type = 'scatter', mode = 'markers',
                   marker = list(size = 10), color = ~FactorType, colors = "Set1") %>%
        layout(title = "Factors Influencing Spread Rate",
               xaxis = list(title = "Factor Value"),
               yaxis = list(title = "Spread Rate"))
      return(p)
    }
  })
  

}

# Run the application 
shinyApp(ui, server)
