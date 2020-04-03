#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(igraph)
library(tidyverse)

source("functions.R")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("State Network"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("scale",
                        "Size of Nodes:",
                        min = 0,
                        max = 200,
                        value = 100)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("netwPlot")
        )
    )
)

# Define server logic required to network plot
server <- function(input, output) {

    output$netwPlot <- renderPlot({
        plot_network(scale_factor = (input$scale / 100))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
