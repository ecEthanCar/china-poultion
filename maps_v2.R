
library(readr)
library(leaflet)
# bigger_4 <- read_csv("data/estate_property_debt_gdp_location(2016-2022).csv")
# View(estate_property_debt_gdp_location_2016_2022_)

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Map On China's Economics"),
  selectInput("year_selector", "Select Year:", choices = sort(bigger_4$year), selected = 2016),
  selectInput("gdp_industry_selector", "Select Industry:", choices = sort(bigger_4$`GDP: Industry`),
  selected = "Agriculture, Forestry, Animal Husbandry and Fishery (Incl. Services)"),
  selectInput("province_selector", "Select Province:", choices = c("All Provinces", sort(bigger_4$Province))),
  # selectInput("city_selector", "Select City:", choices = c("None", sort(bigger_4$Region))),
  leafletOutput("map")
)

# Define server logic
server <- function(input, output) {
  output$map <- renderLeaflet({
    filtered_data <- bigger_12
#    filtered_data <- smaller_2


    if (input$province_selector != "All Provinces") {
      filtered_data <- filtered_data |>
        filter(Province == input$province_selector)
    }

    # if (input$city_selector != "None") {
    #   filtered_data <- filtered_data |>
    #     filter(Region == input$city_selector)
    # }

    filtered_data <- filtered_data |>
      filter(year == input$year_selector,
             `GDP: Industry` == input$gdp_industry_selector)
    leaflet(data = filtered_data) |>
      addTiles() |>
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        popup = ~paste0(
          "<b>Year:</b> ", year,
          "<br><b>Region:</b> ", Region,
          ", <b>Province:</b> ", Province,
          "<br><b>Coordinates:</b> (", Latitude, ", ", Longitude, ")",
          "<br><b>Administrative Division Code:</b> ", Statistical_Division_Code,
          "<br><b>GDP</b>: ", GDP, ifelse(is.na(GDP), "", " RBM bn"),
#          "<br><b>Carbon Emissions: <br>",
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
