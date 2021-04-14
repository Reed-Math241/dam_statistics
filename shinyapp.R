#load libraries
library(shiny)
library(tidyverse)

#generate data
test_dat <- data.frame(x = c(5, 2, 3),
                       y = c(7, 4, 6),
                       dam = c("Hydroelectricity",
                               "Irrigation", "Water Supply"))


# User interface
ui <- fluidPage(
  titlePanel(title = "Dam dashboard"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "purpose", 
                         label = "Dam purpose", 
                         choices = c("Hydroelectricity",
                                     "Irrigation", "Water Supply"),
                         selected = c("Hydroelectricity",
                                      "Irrigation", "Water Supply")
                         )),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)


# Server function
server <- function(input, output){
  
  test_dat2 <- reactive({
    test_dat %>%
      filter(dam %in% input$purpose)
  })
  
  
  
  output$plot <- renderPlot({
    
    ggplot(test_dat2(), aes(x = x, y = y, 
                            color = dam)) +
      geom_point(size = 2) +
      scale_color_manual(breaks = c("Hydroelectricity",
                                    "Irrigation", "Water Supply"),
                         values = c("#00ba38", "#c77cff", "#619cff"))
  })
}


# Creates app
shinyApp(ui = ui, server = server)