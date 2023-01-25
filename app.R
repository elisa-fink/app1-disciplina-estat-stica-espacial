library(tidyverse)
library(lubridate)
library(RSocrata)
library(leaflet)
library(sf)

years_ago <- today() - years(2)
crash_url <- glue::glue("https://data.cityofchicago.org/Transportation/Traffic-Crashes-Crashes/85ca-t3if?$where=CRASH_DATE > '{years_ago}'")
crash_raw <- as_tibble(read.socrata(crash_url))

crash <- crash_raw %>%
  arrange(desc(crash_date)) %>%
  transmute(
    injuries = if_else(injuries_total > 0, "Com feridos", "Sem feridos"),
    damage,
    posted_speed_limit,
    weather_condition,
    crash_date,
    crash_month,
    crash_day_of_week,
    crash_hour,
    street_name,
    street_no,
    street_direction,
    lat = latitude,
    lng = longitude
  ) %>%
  na.omit()

set.seed(2)
crash_sample<- sample_frac(crash, size = 0.01, replace = FALSE)


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Acidentes em Chicago"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "injuries",
                        label = "Feridos:",
                        choices = c("Sem feridos", "Com feridos")),
            sliderInput(inputId = "speed",
                        label = "Quanta velocidade acima do limite (em mph):",
                        min = 0, max = 70, step = 5, value = c(0, 70)),
            radioButtons("damage",
                        "Dano causado:",
                        choices = list("$500 OR LESS", "$501 - $1,500", "OVER $1,500")),
            radioButtons("weather_condition",
                        "Condição do tempo:",
                        choices = levels(as.factor(crash_sample$weather_condition)))
            
        ),

        # Show a plot 
        mainPanel(
           leafletOutput(outputId = "mapa")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    

  output$mapa = renderLeaflet({
    
    x = input$injuries
    color = ""
    if (x == "Sem feridos"){
      x<- "none"
      color = "teal"
    }
    else {
      x<-"injuries"
      color = "darkorange"
    }
    
  
    leaflet(crash_sample %>%
              filter(
                injuries == input$injuries,
                damage == input$damage,
                weather_condition == input$weather_condition,
                posted_speed_limit >= input$speed[1],
                posted_speed_limit <= input$speed[2]
              )) %>% 
      addTiles() %>%
      setView(lng = -87.68, lat = 41.85, zoom = 10) %>%
      addCircleMarkers(lng=~lng,
                       lat=~lat,
                       radius = 2,
                       color = color,
                       stroke = FALSE, fillOpacity = 0.8)
                 
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
