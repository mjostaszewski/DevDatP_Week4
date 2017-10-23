library(shiny)
library(leaflet)

# Define UI for application that explores the air quality dataset
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Air quality sensor data for 2017"),
  
  # Sidebar with two drop-down menu's (sensor and measurement) and visualization choice (radio button)
  sidebarLayout(
    sidebarPanel(
      # Only three major sensors selected
      selectInput(inputId = "sensor_type", 
                  label = "A. Choose a sensor type:",
                  choices = c("PM2.5 - Local Conditions", "Ozone", "Sulfur dioxide")),
      # This will be a dynamic drop down menu, with values changing depending on the sensor type
      # as thre are different measures for different sensor types 
      uiOutput("choose_measure"),
      div(style="display: inline-block;vertical-align:top; width: 10px;",h5(tags$b("C."))),
      div(style="display: inline-block;vertical-align:top; width: 10px;",br()),
      div(style="display: inline-block;vertical-align:top; width: 100px;",actionButton("draw", tags$b("Plot data points"))),
      br(),br(),
      radioButtons(inputId = "select_zoomoutput",
                   label = "D. Choose output:",
                   choices = c("Sensor location", "Linear model")),
      br(),
      h4("Comment:"),
      textOutput("comment"),
      br(),
      h4("User guide:"),
      p("This is an interactive explorer of US 2017 air quality data, obtained from ",
        a(href = "https://www.epa.gov/outdoor-air-quality-data", "EPA website.")),
      p("1. Choose sensor (A) and measure type (B)"),
      p("2. Press 'Plot data points' (C) to visualize selected subset"),
      p("3. Select a group of points of the graph (point and drag)"),
      p("4a. If 'Sensor location' is selected (D), geographical location of measuring sensors will be displayed"),
      p("4b. If 'Linear model' is selected (D), linear trend for selected points will be displayed")
  ),
    
  # Show a plot of the generated distribution and, if needed, a map.
  mainPanel(
    plotOutput("measures_time",
                 brush = brushOpts(id = "focus_points", resetOnNew = TRUE)),
    br(),
    leafletOutput("map_of_sensors")
    )
  )
))
