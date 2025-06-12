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
library(ggpubr)
library(ggsci)
library(ggrepel)

library(dplyr)

# Load helper functions 

thematic_shiny()

here::here()

# Define UI for application that draws a histogram

ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram

server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
