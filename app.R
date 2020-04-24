# Load necessary libraries

library(shiny)
library(igraph)
library(tidyverse)

# Load custom function doc 

source("code_files/functions.R")

# From here onwards the shiny app starts

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("State Network"),

    # Show a plot of the generated distribution
    mainPanel(
              plotOutput("netwPlot"),
              selectInput("variable", "Position States By:",
                          c("Racial Similarity" = "RaceDif",
                            "Distance between State Capitals" = "Distance",
                            "Migration" = "ACS_Migration",
                            "Flights" = "IncomingFlights",
                            "Trade (Imports)" = "Imports",
                            "Political Ideology Differences" = "IdeologyDif",
                            "Religious Differences" = "ReligDif")),
              selectInput("node_size", "States Sized by:",
                          c("Population" = "pop",
                            "Number of State Borders" = "borders"
                            )),
              selectInput("borders_only", "Limit Relationships to Bordering States?",
                          c("Yes" = TRUE,
                            "No" = FALSE
                          )),
              sliderInput("scale",
                          "Size of Nodes:",
                          min = 0,
                          max = 200,
                          value = 100)
        )
)

# Define server logic required to network plot
server <- function(input, output) {

    # Load data 
    
    read_data()
    
    # Get plotting
    
    output$netwPlot <- renderPlot({
        make_plot_master(edges_by = input$variable,
                         nodes_by = input$node_size,
                         borders_only = input$borders_only,
                         scale_factor = (input$scale / 100))
    }, height = 900, width = 1500)
}

# Run the application 
shinyApp(ui = ui, server = server)
