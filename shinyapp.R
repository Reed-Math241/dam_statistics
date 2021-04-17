#load libraries
library(shiny)
library(tidyverse)
library(lubridate)
library(leaflet)
library(here)

#load data
damdata <- read_csv(here("damdata.csv")) %>%
  mutate(purpose = fct_relevel(purpose, c("Hydroelectricity",
                                          "Irrigation",
                                          "Irrigation & Hydroelectricity",
                                          "Irrigation & Water supply")))

damspat <- read_csv(here("damspats.csv")) %>%
  mutate(purpose = fct_relevel(purpose, c("Hydroelectricity",
                                          "Irrigation",
                                          "Irrigation & Hydroelectricity",
                                          "Irrigation & Water supply")))


#testing out plots
content <- paste("<b>", damspat$reservoir_name, "</b></br>",
                 "River:", damspat$river, "</br>",
                 "Purpose:", damspat$purpose, "</br>",
                 "Effective storage capacity:", damspat$effect_sto_cap_bcm, "BCM")


leaflet(options = leafletOptions(minZoom = 3, maxZoom = 9)) %>%
  addTiles() %>%
  setView(lat = 19.2, lng = 76.1, zoom = 7) %>%
  setMaxBounds(lat1 = 10, lng1 = 62.2,
               lat2 = 29, lng2 = 90.2) %>%
  addCircleMarkers(data = damspat,
                   lat = ~lat, lng = ~long,
                   stroke = FALSE, fillOpacity = 0.65,
                   radius = ~effect_sto_cap_bcm*10, popup = content)



# User interface
ui <- fluidPage(
  titlePanel(title = "Dam dashboard"),
  tabsetPanel(
    tabPanel("Map", 
             mainPanel(
               textOutput("title"),
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
                                start = as_date("2015-01-01"), end = as_date("2015-12-01"),
                                min = as_date("2015-01-01"), max = as_date("2020-12-01")),
                 textOutput("dateinfo")),
               mainPanel(
                 h3("Water storage over time"),
                 plotOutput("plot"))
             )),
    
    tabPanel("Stats",
             mainPanel(
               p("This is where we will do our spatial stats.")
             )),
             
    tabPanel("Info",
             mainPanel(
               uiOutput("credit"), uiOutput("link"), uiOutput("link2")
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
  
  
  output$title <- renderText({
    "Maharashtra Dams"
  })
  
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 3, maxZoom = 9)) %>%
      addTiles() %>%
      setView(lat = 19.2, lng = 76.1, zoom = 7) %>%
      setMaxBounds(lat1 = 10, lng1 = 62.2,
                   lat2 = 29, lng2 = 90.2) %>%
      addCircleMarkers(data = damspat,
                       lat = ~lat, lng = ~long,
                       stroke = FALSE, fillOpacity = 0.65,
                       radius = ~effect_sto_cap_bcm*10, popup = content)
  })
  
  
  
  output$dateinfo <- renderText({
    "The data are at the monthly level and range from January 2015 to December 2020. 
    We recommend subsetting by year and/or dam use to avoid overplotting."
  })
  

  
  output$plot <- renderPlot({
  
      ggplot(damreact(),
             aes(x = date, y = storage_bcm, color = purpose,
                 size = effective_storage_capacity_109m3)) +
      geom_point(alpha = 0.6) +
      theme_minimal() +
      labs(y = bquote("Water storage"~(10^9~m^3)), color = "Use",
           size = bquote("Effective storage capacity"~(10^9~m^3))) +
      theme(axis.title.x = element_blank()) +
      guides(color = guide_legend(order = 1),
             size = guide_legend(order = 2)) +
      scale_color_manual(breaks = c("Hydroelectricity",
                                    "Irrigation",
                                    "Irrigation & Hydroelectricity",
                                    "Irrigation & Water supply"),
                         values = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3"))
  })

  
  output$credit <- renderUI({
    auth <- "Authors:"
    ad <- "Aditya Gadkari"
    la <- "Lauren Rabe"
    skip <- " "
    sc <- "Sources:"
    HTML(paste(auth, ad, la, skip, sc, sep = "</br>"))
  })
  
  
  output$link <- renderUI({
    tags$a(href = "https://indiawris.gov.in/wris/#/", 
           target = "_blank", 
           "India Water Resources Information System")
  })
  
  
  output$link2 <- renderUI({
    tags$a(href="https://en.wikipedia.org/wiki/List_of_dams_and_reservoirs_in_Maharashtra", 
           target = "_blank",
           "Wikipedia: List of dams and reservoirs in Maharashtra")
  })
}


# Creates app
shinyApp(ui = ui, server = server)