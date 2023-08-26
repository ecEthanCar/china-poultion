
library(readr)
library(leaflet)

smaller_app <- read_csv("data/carbon_emissions_est_prop_gdp_(2016-2019).csv")

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
          "<br><b>Carbon Emissions</b>: ", `Carbon Emissions`,
          "<br><b>Debt Ratio</b>: ", `Debt Ratio(%)`, "%",
          "<br><b>Annual Real Estate Investment:</b> ",
            Annual_Real_Estate_Investment,
            ifelse(is.na(Annual_Real_Estate_Investment), "", " RBM mn"),
          "<br><b>Annual Average Property Price:</b> ",
            Annual_Avg_Property_Price,
            ifelse(is.na(Annual_Avg_Property_Price), "", " RMB/sq m")
          ),
        clusterOptions = markerClusterOptions(minZoom = 0, maxZoom = 20)
        )
  })
}

# Run the application
shinyApp(ui, server)
