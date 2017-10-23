library(shiny)
library(lubridate)
library(dplyr)
library(ggplot2)
library(leaflet)
# Read in the dataset

# download.file("https://aqs.epa.gov/aqsweb/airdata/annual_conc_by_monitor_2017.zip",
#               destfile = "annual_conc_by_monitor_2017.zip")
# unzip("annual_conc_by_monitor_2017.zip")
# anc_sub <- tbl_df(read.csv("annual_conc_by_monitor_2017.csv")) %>% 
#   filter(Parameter.Name %in% c("PM2.5 - Local Conditions", "Ozone", "Sulfur dioxide")) %>%
#   mutate(X1st.Max.DateTime = ymd_hm(X1st.Max.DateTime))

# !!! Load the preprocessed dataset. This solution reduces load on shinyapps.io. 
# The file was generated with the code above
load(file = "source.Rdata")
# Define server logic
shinyServer(function(input, output) {
  # renderUI allows to define a dynamic drop-down menu, depending on the value of the "sensor_type" input
  output$choose_measure <- renderUI({
    range <- anc_sub %>%
      filter(Parameter.Name == input$sensor_type) %>%
      pull(Metric.Used)
    selectInput("selected_measure", "B. Choose a measure:", unique(range))
  })
  
  # plot a blank graph
  observeEvent(input$selected_measure, {
    output$measures_time <- renderPlot({
      NULL
    })
    output$comment <- renderText({"Press C. Plot data points"})
  })
  
  # When the button is clicked, draw the graph for the selected sensor and measure type
  observeEvent(input$draw, {
    output$measures_time <- renderPlot({
      points <- anc_sub %>%
        filter(Parameter.Name == input$sensor_type & Metric.Used == input$selected_measure)
      points <- points %>% mutate(colour = "black", size = 1,7)
      ggplot(data = points, aes(x = X1st.Max.DateTime, y = X1st.Max.Value)) + 
        geom_point(colour = points$colour, size = points$size) +
        labs(x = "Time", y = paste("Sensor readout: ", as.character(pull(points, "Units.of.Measure"))[1])) + 
        ggtitle(paste("Readout of", input$sensor_type)) +
        geom_jitter(height = 0.1*(sd(points$X1st.Max.Value)))
    })
    output$comment <- renderText({"Select points on the graph and the outupt type"})
  })

  # Observer for brush selection of points in the graph  
  observeEvent(input$focus_points, {
    points <- anc_sub %>%
      filter(Parameter.Name == input$sensor_type & Metric.Used == input$selected_measure)
    points <- points %>% mutate(colour = "black", size = 1,7)
    pfilter = points$X1st.Max.DateTime <= input$focus_points$xmax & 
      points$X1st.Max.DateTime >= input$focus_points$xmin &
      points$X1st.Max.Value <= input$focus_points$ymax & 
      points$X1st.Max.Value >= input$focus_points$ymin
    # For later highlighting the selected data points
    points[pfilter,]$colour = "blue"
    points[pfilter,]$size = 3
    
    # Condition the action based on the radio button status
    choice <- input$select_zoomoutput
    
    if(choice == "Sensor location") {
      # Generate leaflet map
      these_points <- points %>%
        filter(X1st.Max.DateTime <= input$focus_points$xmax & X1st.Max.DateTime >= input$focus_points$xmin) %>%
        filter(X1st.Max.Value <= input$focus_points$ymax & X1st.Max.Value >= input$focus_points$ymin)
      xys = unique(data.frame(these_points[,c("Latitude", "Longitude")]))
      output$map_of_sensors <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addMarkers(data = xys, clusterOptions = markerClusterOptions())
      })
      output$measures_time <- renderPlot({
        ggplot(data = points, aes(x = X1st.Max.DateTime, y = X1st.Max.Value)) + 
          geom_point() +
          labs(x = "Time", y = paste("Sensor readout: ", as.character(pull(points, "Units.of.Measure"))[1])) +
          geom_jitter(height = 0.1*(sd(points$X1st.Max.Value)), colour = points$colour, size = points$size) +
          ggtitle(paste("Readout of", input$sensor_type, ", location selected points shown on the map"))
      })
      output$comment <- renderText({paste(nrow(xys), 
                                          "locations for", sum(pfilter), 
                                          "points selected and shown on map")})
    } else {
      # Generate linear model output
      output$measures_time <- renderPlot({
        ggplot(data = points, aes(x = X1st.Max.DateTime, y = X1st.Max.Value)) + 
          geom_point() +
          geom_smooth(data = points[pfilter,], aes(x = X1st.Max.DateTime, y = X1st.Max.Value), method = "lm") + 
          labs(x = "Time", y = paste("Sensor readout: ", as.character(pull(points, "Units.of.Measure"))[1])) +
          geom_jitter(height = 0.1*(sd(points$X1st.Max.Value)), colour = points$colour, size = points$size) +
          ggtitle(paste("Readout of", input$sensor_type, ", linear model for selected points"))
      })
      output$comment <- renderText({paste(sum(pfilter), "points selected, linear model drawn")})
    }
  })
})
