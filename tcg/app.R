#' ---
#' title: "TCGPlayer_Shiny"
#' author: "Darren Shoemaker"
#' ---

# Libraries ----

# Compatibility 

library(here)

# Shiny libraries

library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(htmlwidgets)
library(DT)
library(bslib)
library(thematic)
library(bsicons)
library(fontawesome)
library(htmltools)
library(shinycssloaders)

# Function libraries (dependencies)

library(ggplot2)
library(ggstatsplot)
library(ggpubr)
library(ggsci)
library(ggrepel)
library(patchwork)

library(dplyr)

# Load helper functions 

thematic_shiny()

here::here()

# Define UI for application that draws a histogram

ui <- bslib::page_fluid(
  
  theme = bs_theme(version = 5, bootswatch = 'zephyr'),
  
  useShinyjs(),
  
  page_navbar(title = 'TCGPlayer Inventory Summarizer by Chardar!',
              selected = 'Home',
              
              nav_panel(
                title = 'Home',
                fillable = T,
                
                page_fillable(
                  
                  card(
                   
                    fileInput('upload', "Upload exported TCGPlayer Inventory", accept = c('.csv'))
                    
                  ),
                  
                  plotOutput('products'),
                  
                  plotOutput('price'),
                  
                  plotOutput('value')
                )
              ),
              
              nav_panel(title = 'About'),
              
              bslib::nav_item(input_dark_mode(id = 'dark_mode', mode = 'light'))
    
  )
)

# Define server logic required to draw a histogram

server <- function(input, output) {

    data <- reactive({
      req(input$upload)
      
      ext <- tools::file_ext(input$upload$name)
      switch(ext,
             csv = vroom::vroom(input$upload$datapath, delim = ','),
             validate("Invalid file extension. Only .csv files are accepted."))
    })
    
    products <- reactive({
      req(input$upload)
      
      a <- data() %>% 
        filter(`Total Quantity` != 0) %>% 
        summarise(Count = sum(`Total Quantity`), .by = `Product Line`) %>% 
        ggplot() +
        geom_bar(aes(x = Count, y = `Product Line`, fill = `Product Line`), stat = 'identity') +
        geom_label(aes(x = Count, y = `Product Line`, label = Count)) +
        labs(y = 'Product Line') +
        theme(axis.title.x = element_blank()) +
        theme_bw() +
        scale_fill_frontiers() 
      
      b <- data() %>% 
        filter(`Total Quantity` != 0) %>% 
        summarise(Count = sum(`Total Quantity`), .by = `Product Line`) %>% 
        ggplot() +
        geom_bar(aes(x = '', y = Count, fill = `Product Line`), stat = 'identity', width = 1) +
        coord_polar('y', start = 0) +
        labs(y = NULL, x = NULL) +
        theme(axis.text.y = element_blank(),
              axis.title.y = element_blank()) +
        theme_bw() +
        scale_fill_frontiers() 
      
      a + b + plot_layout(nrow = 1, ncol = 2, guides = 'collect')
     
      })
    
    price <- reactive({
      req(input$upload)
      
      a <- data() %>% 
        filter(`Total Quantity` != 0) %>% 
        summarise(Price = sum(`TCG Low Price`, na.rm = T), .by = `Product Line`) %>% 
        na.omit() %>% 
        ggplot() +
        geom_bar(aes(x = Price, y = `Product Line`, fill = `Product Line`), stat = 'identity') +
        geom_label(aes(x = Price, y = `Product Line`, label = Price)) +
        labs(y = 'Product Line') +
        theme(axis.title.x = element_blank()) +
        theme_bw() +
        scale_fill_frontiers() 
      
      b <- data() %>% 
        filter(`Total Quantity` != 0) %>% 
        summarise(Price = sum(`TCG Low Price`, na.rm = T), .by = `Product Line`) %>%
        na.omit() %>% 
        ggplot() +
        geom_bar(aes(x = '', y = Price, fill = `Product Line`), stat = 'identity', width = 1) +
        coord_polar('y', start = 0) +
        labs(y = NULL, x = NULL) +
        theme(axis.text.y = element_blank(),
              axis.title.y = element_blank()) +
        theme_bw() +
        scale_fill_frontiers() 
      
      a + b + plot_layout(nrow = 1, ncol = 2, guides = 'collect')
      
    })
    
    value <- reactive({
      req(input$upload)
      
      a <- data() %>% 
        filter(`Total Quantity` != 0) %>% 
        mutate(Rate = case_when(`TCG Low Price` > 50 ~ 'High End',
                                            between(`TCG Low Price`, 5, 50) ~ 'Decent',
                                            between(`TCG Low Price`, 1, 5) ~ 'Trade',
                                            between(`TCG Low Price`, 0.5, 1) ~ 'Playable',
                                            `TCG Low Price` < 0.5 ~ 'Bulk')) %>% 
        summarise(Price = sum(`TCG Low Price`, na.rm = T), .by = `Rate`) %>% 
        na.omit() %>% 
        ggplot() +
        geom_bar(aes(x = Price, y = Rate, fill = Rate), stat = 'identity') +
        geom_label(aes(x = Price, y = Rate, label = Price)) +
        labs(y = 'Product Line') +
        theme(axis.title.x = element_blank()) +
        theme_bw() +
        scale_fill_frontiers() 
      
      b <- data() %>% 
        filter(`Total Quantity` != 0) %>% 
        mutate(Rate = case_when(`TCG Low Price` > 50 ~ 'High End',
                                between(`TCG Low Price`, 5, 50) ~ 'Decent',
                                between(`TCG Low Price`, 1, 5) ~ 'Trade',
                                between(`TCG Low Price`, 0.5, 1) ~ 'Playable',
                                `TCG Low Price` < 0.5 ~ 'Bulk')) %>% 
        summarise(Price = sum(`TCG Low Price`, na.rm = T), .by = `Rate`) %>%
        na.omit() %>% 
        ggplot() +
        geom_bar(aes(x = '', y = Price, fill = `Rate`), stat = 'identity', width = 1) +
        coord_polar('y', start = 0) +
        labs(y = NULL, x = NULL) +
        theme(axis.text.y = element_blank(),
              axis.title.y = element_blank()) +
        theme_bw() +
        scale_fill_frontiers() 
      
      a + b + plot_layout(nrow = 1, ncol = 2, guides = 'collect')
      
    })
    
    output$products <- renderPlot({
      products()
    })
    
    output$price <- renderPlot({
      price()
    })
    
    output$value <- renderPlot({
      value()
    })

}

# Run the application 
shinyApp(ui = ui, server = server)