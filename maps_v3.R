
library(readr)
library(leaflet)

smaller_app <- read_csv("data/emissions_lat_long.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Map with Year Dropdown (Carbon Emissions)"),
  selectInput("year_selector", "Select Year:", choices = sort(smaller_app$year), selected = 2016),
  # selectInput("province_selector", "Select Province:", choices = c("All Provinces", sort(smaller_app$Province))),
  leafletOutput("map")
)

# Define server logic
server <- function(input, output) {
  output$map <- renderLeaflet({
    filtered_data <- smaller_app

    # if (input$province_selector != "All Provinces") {
    #   filtered_data <- filtered_data |>
    #     filter(Province == input$province_selector)
    # }

    filtered_data <- filtered_data |>
      filter(year == input$year_selector)

    leaflet(data = filtered_data) |>
      addTiles() |>
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        popup = ~paste(
          "<b>Region:</b> ", Region,
          # ", <b>Province:</b> ", Province,
          ", <b>Year:</b> ", year,
          "<br><b>Administrative Division Code:</b> ", Statistical_Division_Code,
          "<br><b>Carbon Emissions</b>: ", `Carbon Emissions`
          ),
        clusterOptions = markerClusterOptions(minZoom = 0, maxZoom = 20)
        )
  })
}

# Run the application
shinyApp(ui, server)
