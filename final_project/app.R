# Load necessary libraries

library(shiny)
library(igraph)
library(tidyverse)

# Load custom function doc

source("functions.R")

# From here onwards the shiny app starts

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("State Network"),

    # Show a plot of the generated distribution
    mainPanel(
              sliderInput("scale",
                   "Size of Nodes:",
                   min = 0,
                   max = 200,
                   value = 100), 
              plotOutput("netwPlot"), 
        )
)

# Define server logic required to network plot
server <- function(input, output) {

    output$netwPlot <- renderPlot({
        plot_network(scale_factor = (input$scale / 100))
    }, height = 500, width = 750)
}

# Run the application 
shinyApp(ui = ui, server = server)
