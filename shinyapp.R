#load libraries
library(shiny)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(leaflet)
library(here)

#wrangle data (TO BE REPLACED BY READING IN CSV)
damdata <- read_csv(here("damdata.csv"))


leaflet(options = leafletOptions(minZoom = 4, maxZoom = 9)) %>%
  addTiles() %>%
  setView(lat = 19.35, lng = 76.2, zoom = 6) %>%
  setMaxBounds(lat1 = 10, lng1 = 62.2,
               lat2 = 29, lng2 = 90.2)


# User interface
ui <- fluidPage(
  titlePanel(title = "Dam dashboard"),
  tabsetPanel(
    tabPanel("Map", 
             mainPanel(
               textOutput("info"),
               leafletOutput("map"))
             ),
    
    tabPanel("Plot", 
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput(inputId = "damtype",
                                    label = "Dam use",
                                    choices = levels(damdata$purpose),
                                    selected = levels(damdata$purpose)),
                 dateRangeInput(inputId = "dates",
                                label = "Date range",
                                start = as_date("2015-01-01"), end = as_date("2020-12-01"),
                                min = as_date("2020-01-01"), max = as_date("2020-12-01"))),
               mainPanel(plotOutput("plot"))
             )),
             
    tabPanel("Sources",
             mainPanel(
               uiOutput("link"), uiOutput("link2")
             )
    )
  )
)

    
    
    
# Server function
server <- function(input, output){

  
  damreact <- reactive({
    damdata %>%
      filter(purpose %in% input$damtype, 
             date <= input$dates[2], date >= input$dates[1])
  })
  
  
  output$info <- renderText({
    "Maharashtra Dams"
  })
  
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 4, maxZoom = 9)) %>%
      addTiles() %>%
      setView(lat = 19.35, lng = 76.2, zoom = 6) %>%
      setMaxBounds(lat1 = 10, lng1 = 62.2,
                   lat2 = 29, lng2 = 90.2)
  })
  
  
  
  output$plot <- renderPlot({
  
      ggplot(damreact(),
             aes(x = date, y = storage_bcm, color = purpose,
                 size = effective_storage_capacity_109m3)) +
      geom_point(alpha = 0.6) +
      theme_minimal() +
      labs(y = bquote("Water storage"~(10^9~m^3)), color = "Use",
           size = bquote("Effective storage capacity"~(10^9~m^3)),
           caption = "Data are at the monthly level.") +
      theme(axis.title.x = element_blank()) +
      guides(color = guide_legend(order = 1),
             size = guide_legend(order = 2)) +
      scale_color_manual(breaks = c("Hydroelectricity",
                                    "Irrigation",
                                    "Irrigation & Hydroelectricity",
                                    "Irrigation & Water supply"),
                         values = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3"))
  })

  
  
  output$link <- renderUI({
    tags$a(href = "https://d1z8le3pdnub92.cloudfront.net/app/0.0.49/#/reports/important-dams", 
           target = "_blank", 
           "Water Resources Department. Government of Maharashtra, India.")
  })
  
  
  output$link2 <- renderUI({
    tags$a(href="https://en.wikipedia.org/wiki/List_of_dams_and_reservoirs_in_Maharashtra", 
           target = "_blank",
           "Wikipedia: List of dams and reservoirs in Maharashtra.")
  })
}


# Creates app
shinyApp(ui = ui, server = server)