# Define UI
ui <- fluidPage(
  titlePanel("Interactive Map with Year Dropdown"),
  selectInput("year_selector", "Select Year:", choices = sort(bigger_3$year)),
  selectInput("gdp_industry_selector", "Select Industry:", choices = sort(bigger_3$`GDP: Industry`)),
  selectInput("province_selector", "Select Province:", choices = c("All Provinces", sort(bigger_3$Province))),
  selectInput("city_selector", "Select City:", choices = c("None", sort(bigger_3$Region))),
  leafletOutput("map")
)

# Define server logic
server <- function(input, output) {
  output$map <- renderLeaflet({
    filtered_data <- bigger_3

    if (input$province_selector != "All Provinces") {
      filtered_data <- filtered_data |>
        filter(Province == input$province_selector)
    }

    if (input$city_selector != "None") {
      filtered_data <- filtered_data |>
        filter(Region == input$city_selector)
    }

    filtered_data <- filtered_data |>
      filter(year == input$year_selector,
             `GDP: Industry` == input$gdp_industry_selector)
    leaflet(data = filtered_data) |>
      addTiles() |>
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        popup = ~paste(
          "Region: ", Region,
          " , Province: ", Province
          # Figure out how to add other column data other than Region, Province, and year
          # " , Administrative Division Code: ", `Debt Ratio(%)`
          ),
        clusterOptions = markerClusterOptions())
  })
}

# Run the application
shinyApp(ui, server)
