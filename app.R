library(shiny)
library(dplyr)
library(ggplot2)
library(ggforce)

ui <- fluidPage(
  fluidRow(
    column(12, tags$h1("Estimate the market size for a product or company"))),
  fluidRow(
    column(4, 
           tags$h3("Enter your assumptions"),
           numericInput(inputId = "unit_arr", 
                       label = "Annual recurring revenue per unit in real dollars", 
                       value = 100, 
                       min = 0, 
                       max = 100000000, 
                       step = 1000), 
           numericInput(inputId = "units_per_customer", 
                       label = "Units per customer", 
                       value = 1, 
                       min = 1, 
                       max = 10000000, 
                       step = 100), 
           numericInput(inputId = "num_cust", 
                       label = "Number of customers", 
                       value = 10000, 
                       min = 1, 
                       max = 100000000000, 
                       step = 1000), 
           numericInput(inputId = "cagr", 
                        label = "Industry growth rate (CAGR) in %", 
                        value = 5, 
                        min = 0, 
                        max = 1000, 
                        step = 1),
           sliderInput(inputId = "SAM_perc", 
                       label = "Estimate what percent of the TAM is servicable", 
                       value = 10, 
                       min = 0, 
                       max = 100), 
           sliderInput(inputId = "SOM_perc", 
                       label = "Estimate what percent of the SAM is obtainable", 
                       value = 10, 
                       min = 0, 
                       max = 100)), 
    column(8, 
           tags$h3("Estimated ARR in 5 years"),
           plotOutput("circ"))), 
  fluidRow(
    column(12),
    tags$p("Want to request a feature? Open an issue on ",
           tags$a(href = "https://github.com/katiesaund/market_size_app", "Github.")))
)

server <- function(input, output) {
  output$circ <- renderPlot({
    unit_arr_millions <- input$unit_arr / 1000000
    TAM_size <- 
      unit_arr_millions * 
      input$units_per_customer * 
      input$num_cust *
      ((1 + (input$cagr / 100)) ^ 5)
    SAM_size <- TAM_size * input$SAM_perc / 100 
    SOM_size <- SAM_size * input$SOM_perc / 100
    
    market_size <- as.data.frame(matrix(c(TAM_size, SAM_size, SOM_size), ncol = 1))
    market_size <- market_size %>% mutate("market_share" = c("TAM", "SAM", "SOM"))  
    colnames(market_size) <- c("ARR", "Market")                             
    market_size <- market_size %>% 
      mutate("area" = ARR) %>% 
      mutate("radius" = sqrt(area / 3.14)) %>% 
      mutate("label_position" = radius * c(1.9, 1.7, 1))
    
    market_size$Market <- factor(market_size$Market, levels = c("TAM", "SAM", "SOM"))
    
    
    
    market_size <- market_size %>% 
      mutate("mod_ARR" = ARR) %>% 
      mutate("Best_Denomination" = "M")
    
    for (i in 1:3){
      if (market_size$ARR[i] > 999) {
        market_size$Best_Denomination[i] <- "B"
        market_size$mod_ARR[i] <- market_size$mod_ARR[i]  / 1000
      } else if (market_size$ARR[i] < 1 ) {
        market_size$Best_Denomination[i] <- "K"
        market_size$mod_ARR[i] <- market_size$mod_ARR[i] * 1000
      }
    }
    
    print(market_size)
    
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
                    label = paste0(round(mod_ARR, 1), Best_Denomination)))
  })
}

shinyApp(ui = ui, server = server)