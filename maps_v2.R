# Define UI
ui <- fluidPage(
  titlePanel("Interactive Map with Year Dropdown"),
  selectInput("year_selector", "Select Year:", choices = unique(bigger_3$year)),
  leafletOutput("map")
)

# Define server logic
server <- function(input, output) {
  output$map <- renderLeaflet({
    filtered_data <- bigger_3 %>% filter(year == input$year_selector)
    leaflet(data = filtered_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        popup = ~paste(Province, " ,", Region),
        clusterOptions = markerClusterOptions())
  })
}

# Run the application
shinyApp(ui, server)
