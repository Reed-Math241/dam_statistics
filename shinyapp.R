#load libraries
library(shiny)
library(tidyverse)

#generate data
purpose <- c("Hydroelectricity", "Irrigation", "Water Supply")

test_dat <- data.frame(x = c(5, 2, 3),
                       y = c(7, 4, 6),
                       dam = c("Hydroelectricity",
                               "Irrigation", "Water Supply"))

plot <- ggplot(test_dat, aes(x = x, y = y, color = dam)) +
  geom_point(size = 2)


# User interface
ui <- fluidPage(
  titlePanel(title = "Dam dashboard"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "purpose", 
                         label = h3("Dam purpose"), 
                         choices = list("Hydroelectricity" = 1,
                                        "Irrigation" = 2, 
                                        "Water Supply" = 3),
                         selected = 1:3),
      
      
      hr(),
      fluidRow(column(3, verbatimTextOutput("value")))
    ),
    mainPanel(
      plotOutput(outputId = "plot")
    )
  )
)


# Server function
server <- function(input, output){
  output$plot <- renderPlot({
    test_dat <- reactive({data.frame(x = c(5, 2, 3),
                           y = c(7, 4, 6),
                           dam = c("Hydroelectricity",
                                   "Irrigation", "Water Supply"))
    })
    
    ggplot(test_dat(), aes(x = x, y = y, 
                         color = .data[[input$dam]])) +
      geom_point(size = 2)
  })
}


# Creates app
shinyApp(ui = ui, server = server)