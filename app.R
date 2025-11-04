library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(rnaturalearth)
library(rnaturalearthdata)

# Load world map (not needed for leaflet, but kept if useful later)
world <- ne_countries(scale = "medium", returnclass = "sf")

# Load and prepare your conflict data
conflicts <- read_csv("GEDEvent_v25_1.csv") %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  mutate(type_of_violence = as.numeric(type_of_violence))

# UI
ui <- fluidPage(
  titlePanel("Conflicts by type over the years"),
  tags$h4("Based on the UCDP Georeferenced Event Dataset (GED), Global Version 25.1", 
          style = "color: #666; font-weight: normal; margin-top: -10px;"),
  
  # Interactive leaflet map
  leafletOutput("mapPlot", height = "600px"),
  
  # Controls below
  wellPanel(
    fluidRow(
      column(6,
             sliderInput("year", "Select Year:",
                         min = min(conflicts$year, na.rm = TRUE),
                         max = max(conflicts$year, na.rm = TRUE),
                         value = min(conflicts$year, na.rm = TRUE),
                         step = 1,
                         sep = "")
      ),
      column(6,
             checkboxGroupInput("violence_type", "Type of Violence:",
                                choices = list(
                                  "1: State-based" = 1,
                                  "2: Non-state" = 2,
                                  "3: One-sided" = 3
                                ),
                                inline = TRUE,
                                selected = c(1, 2, 3))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  filtered_data <- reactive({
    data <- conflicts %>%
      filter(year == input$year,
             type_of_violence %in% as.numeric(input$violence_type))  
    print("Filtered types:")
    print(unique(data$type_of_violence))
    data
  })
  
  output$mapPlot <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 0, lat = 10, zoom = 2)
  })
  
  observe({
    # 🗺️ Update map
    leafletProxy("mapPlot", data = filtered_data()) %>%
      clearMarkers() %>%
      addCircleMarkers(
        data = filtered_data(),  # ⬅️ make sure data is explicitly passed here
        lng = ~longitude,
        lat = ~latitude,
        radius = ~scales::rescale(best, to = c(3, 6)),
        color = ~case_when(
          type_of_violence == "1" ~ "#B22222CC",
          type_of_violence == "2" ~ "#4682B4CC",
          type_of_violence == "3" ~ "#00CD66CC",
          TRUE ~ "black"
        ),
        stroke = FALSE,
        fillOpacity = 0.8,
        popup = ~paste0(
          "<strong>Year:</strong> ", year, "<br/>",
          "<strong>Type:</strong> ", type_of_violence, "<br/>",
          "<strong>Best Estimate of Deaths:</strong> ", best
        )
      )
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
