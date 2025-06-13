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

# Define UI ----

ui <- bslib::page_fluid(
  
  theme = bs_theme(version = 5, bootswatch = 'zephyr'),
  
  useShinyjs(),
  
  page_navbar(title = 'TCGPlayer Inventory Summarizer by Chardar!',
              selected = 'Home',
              
              nav_panel(
                title = 'Home',
                fillable = T,
                
                layout_sidebar(fillable = T,
                               fill = T,
                               width = '30%',
                               
                               sidebar = sidebar(
                                 
                                 materialSwitch('bulk', label = tags$span(
                                   'Include Bulk?', 
                                   tooltip(
                                     bsicons::bs_icon('question-circle'),
                                     'Selecting yes will include cards with TCG Low Prices below 50 cents.',
                                     placement = 'right', options = list(template = '<div class="tooltip" role="tooltip"><div class="tooltip-arrow"></div><div class="tooltip-inner" style="text-align: left"></div></div>')
                                   )
                                 ),
                                 status = 'primary')
                              
                               ),
                  
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

# Server ----

server <- function(input, output) {

    data <- reactive({
      req(input$upload)
      
      if(!input$bulk) {
        ext <- tools::file_ext(input$upload$name)
        switch(ext,
               csv = vroom::vroom(input$upload$datapath, delim = ','),
               validate("Invalid file extension. Only .csv files are accepted.")) %>% 
          filter(`Total Quantity` != 0,
                 `TCG Low Price` > 0.50)
      } else {
      
      ext <- tools::file_ext(input$upload$name)
      switch(ext,
             csv = vroom::vroom(input$upload$datapath, delim = ','),
             validate("Invalid file extension. Only .csv files are accepted.")) %>% 
        filter(`Total Quantity` != 0)
      }
    })
    
    products <- reactive({
      req(input$upload)
      
      a <- data() %>% 
        summarise(Count = sum(`Total Quantity`), .by = `Product Line`) %>% 
        ggplot() +
        geom_bar(aes(x = Count, y = `Product Line`, fill = `Product Line`), stat = 'identity') +
        geom_label(aes(x = Count, y = `Product Line`, label = Count)) +
        labs(y = 'Product Line')  +
        theme_bw() +
        scale_fill_frontiers() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text = element_text(size = 14, face = 'bold'),
              legend.text = element_text(size = 14))
      
      b <- data() %>% 
        summarise(Count = sum(`Total Quantity`), .by = `Product Line`) %>% 
        ggplot() +
        geom_bar(aes(x = '', y = Count, fill = `Product Line`), stat = 'identity', width = 1) +
        coord_polar('y', start = 0) +
        labs(y = NULL, x = NULL)  +
        geom_label_repel(aes(x = '', y = Count, label = `Product Line`)) +
        theme_bw() +
        scale_fill_frontiers() +
        theme(axis.text = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks = element_blank(),
              legend.text = element_text(size = 14))
      
      a + b + plot_layout(nrow = 1, ncol = 2, guides = 'collect')
     
      })
    
    price <- reactive({
      req(input$upload)
      
      a <- data() %>% 
        summarise(Price = sum(`TCG Low Price`, na.rm = T), .by = `Product Line`) %>% 
        na.omit() %>% 
        ggplot() +
        geom_bar(aes(x = Price, y = `Product Line`, fill = `Product Line`), stat = 'identity') +
        geom_label(aes(x = Price, y = `Product Line`, label = Price)) +
        labs(y = 'Product Line') +
        theme_bw() +
        scale_fill_frontiers() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text = element_text(size = 14, face = 'bold'),
              legend.text = element_text(size = 14))
      
      b <- data() %>% 
        summarise(Price = sum(`TCG Low Price`, na.rm = T), .by = `Product Line`) %>%
        na.omit() %>% 
        ggplot() +
        geom_bar(aes(x = '', y = Price, fill = `Product Line`), stat = 'identity', width = 1) +
        coord_polar('y', start = 0) +
        labs(y = NULL, x = NULL) +
        theme_bw() +
        scale_fill_frontiers() +
        theme(axis.text = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks = element_blank(),
              legend.text = element_text(size = 14))
      
      a + b + plot_layout(nrow = 1, ncol = 2, guides = 'collect')
      
    })
    
    value <- reactive({
      req(input$upload)
      
      a <- data() %>%
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
        theme_bw() +
        scale_fill_frontiers() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text = element_text(size = 14, face = 'bold'),
              legend.text = element_text(size = 14))
      
      b <- data() %>% 
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
        theme_bw() +
        scale_fill_frontiers() +
        theme(axis.text = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks = element_blank(),
              legend.text = element_text(size = 14))
      
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