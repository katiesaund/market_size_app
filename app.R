library(shiny)
library(dplyr)
library(ggplot2)
library(ggforce)
ui <- fluidPage(
  numericInput(inputId = "TAM", 
            label = "Estimate the Total Addressable Market (ARR in millions)",
            value = 100, 
            min = 0,
            max = 1000000000,
            step = 10), 
  # sliderInput(inputId = "TAM", 
  #             label = "Estimate the Total Addressable Market (ARR)",  
  #             value = 10000,  #default value
  #             min = 1,
  #             max = 100000),
  sliderInput(inputId = "SAM_perc", 
              label = "Estimate how much of the TAM is servicable", 
              value = 10, 
              min = 0, 
              max = 100), 
  sliderInput(inputId = "SOM_perc", 
              label = "Estimate how much of the SAM is obtainable", 
              value = 10, 
              min = 0, 
              max = 100), 
  plotOutput("circ")
)

server <- function(input, output) {
  output$circ <- renderPlot({
    SAM_size <- input$TAM * input$SAM_perc / 100 
    SOM_size <- SAM_size * input$SOM_perc / 100
    
    market_size <- as.data.frame(matrix(c(input$TAM, SAM_size, SOM_size), ncol = 1))
    market_size <- market_size %>% mutate("market_share" = c("TAM", "SAM", "SOM"))  
    colnames(market_size) <- c("ARR", "Market")                             
    market_size <- market_size %>% 
      mutate("area" = ARR) %>% 
      mutate("radius" = sqrt(area / 3.14)) %>% 
      # mutate("Millions" = ARR / 1000000) %>% 
      mutate("label_position" = radius * c(1.9, 1.7, 1))
    
    market_size$Market <- factor(market_size$Market, levels = c("TAM", "SAM", "SOM"))
    
    market_size %>%   
      ggplot() + 
      geom_circle(aes(x0 = 0, 
                      y0 = radius, 
                      r = radius, 
                      fill = Market), 
                  show.legend = TRUE) + 
      coord_fixed() + 
      theme_void() + 
      scale_fill_manual(values = c("#FFEDA0", "#FEB24C", "#F03B20")) + 
      geom_text(aes(x = 0,
                    y = label_position, 
                    label = paste0(ARR, "M")))
  })
}

shinyApp(ui = ui, server = server)

