#load libraries
library(shiny)
library(tidyverse)
library(lubridate)
library(leaflet)
library(here)
library(leaflet.extras)
library(shinyWidgets)
library(leafpop)

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
                                          "Irrigation & Water supply")),
         district = as_factor(district))

load(here("popmaps.RData")) #this is called "maps" in the environment
#because that's what it was called in leafpop.Rmd and somehow R knows


#create static pieces
content <- paste("<b>", damspat$reservoir_name, "</b></br>",
                 "River:", damspat$river, "</br>",
                 "Purpose:", damspat$purpose, "</br>",
                 "Effective storage capacity:", damspat$effective_storage_capacity_109m3, "BCM")


leaflet(options = leafletOptions(minZoom = 3, maxZoom = 9)) %>%
  addProviderTiles(providers$CartoDB.VoyagerLabelsUnder) %>%
  setView(lat = 19.2, lng = 76.1, zoom = 7) %>%
  setMaxBounds(lat1 = 10, lng1 = 62.2,
               lat2 = 29, lng2 = 90.2) %>%
  addCircleMarkers(data = damspat,
                   lat = ~lat, lng = ~long,
                   stroke = FALSE, fillOpacity = 0.65,
                   radius = ~effective_storage_capacity_109m3*10, popup = content)



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
                 
                 pickerInput(inputId = "damdist",
                             label = "District",
                             choices = unique(damdata$district),
                             selected = unique(damdata$district),
                             options = list(`actions-box` = TRUE,
                                            size = 10,
                                            `selected-text-format` = "count > 3"
                                            ),
                             multiple = TRUE),
                 
                 dateRangeInput(inputId = "dates",
                                label = "Date range",
                                start = as_date("2015-01-01"), end = as_date("2016-12-01"),
                                min = as_date("2015-01-01"), max = as_date("2020-12-01")),
                 
                 textOutput("dateinfo")),
               
               mainPanel(
                 h3("Water storage over time"),
                 plotOutput("plot"))
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
             district %in% input$damdist,
             date <= input$dates[2], date >= input$dates[1])
  })
  
  
  output$title <- renderText({
    "Maharashtra Dams"
  })
  
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 3, maxZoom = 9)) %>%
      addTiles() %>%
      setView(lat = 19.1, lng = 75.8, zoom = 7) %>%
      setMaxBounds(lat1 = 10, lng1 = 62.2,
                   lat2 = 29, lng2 = 90.2) %>%
      addCircleMarkers(data = damspat,
                       lat = ~lat, lng = ~long,
                       stroke = FALSE, fillOpacity = 0.65,
                       radius = ~effective_storage_capacity_109m3*10,
                       group = "dams",
                       label = unique(damspat$reservoir_name)) %>%
      addPopupGraphs(maps, group = "dams", width = 400, height = 300)
    
      
  })
  
  
  
  output$dateinfo <- renderText({
    "The data are at the monthly level and range from January 2015 to December 2020. 
    We recommend subsetting the data to avoid overplotting."
  })
  

  
  output$plot <- renderPlot({
  
      ggplot(damreact(),
             aes(x = date, y = storage_bcm, color = purpose, group = reservoir_name)) +
      geom_point(alpha = 0.6,
                 size = damreact()$effective_storage_capacity_109m3*2) +
      geom_line(alpha = 0.2, color = "black") +
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