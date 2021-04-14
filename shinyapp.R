#load libraries
library(shiny)
library(tidyverse)

#generate data
test_dat <- data.frame(x = c(5, 2, 3),
                       y = c(7, 4, 6),
                       dam = c("Hydroelectricity",
                               "Irrigation", "Water Supply"))

dam2015fin2 <- dam2015fin %>%
  drop_na() %>%
  mutate(purpose = str_replace(purpose, "  ", " & "),
         purpose = fct_relevel(purpose, c("Hydroelectricity",
                                          "Irrigation",
                                          "Irrigation & Hydroelectricity",
                                          "Irrigation & Water supply")))


# User interface
ui <- fluidPage(
  titlePanel(title = "Dam dashboard"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "damtype",
                         label = "Dam use",
                         choices = levels(dam2015fin2$purpose),
                         selected = levels(dam2015fin2$purpose))),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)


# Server function
server <- function(input, output){

  
  dam2015react <- reactive({
    dam2015fin2 %>%
      mutate(eff_sto_cap_106m3 = effective_storage_capacity_103m3/1000) %>%
      group_by(reservoir_name) %>%
      filter(purpose %in% input$damtype)
  })
  
  
  output$plot <- renderPlot({
  
      ggplot(dam2015react(),
             aes(x = date, y = value, color = purpose,
                 size = eff_sto_cap_106m3)) +
      geom_point(alpha = 0.6) +
      theme_minimal() +
      labs(y = bquote("Water storage"~(10^6~m^3)), color = "Use",
           size = bquote("Effective storage capacity"~(10^6~m^3))) +
      theme(axis.title.x = element_blank()) +
      guides(color = guide_legend(order = 1),
             size = guide_legend(order = 2)) +
      scale_color_manual(breaks = c("Hydroelectricity",
                                    "Irrigation",
                                    "Irrigation & Hydroelectricity",
                                    "Irrigation & Water supply"),
                         values = c("#f8766d", "#00bfc4", "#7cae00", "#c77cff"))
  })
}


# Creates app
shinyApp(ui = ui, server = server)