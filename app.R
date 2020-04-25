# Load necessary libraries

library(shiny)
library(shinythemes)
library(igraph)
library(vembedr)
library(tidyverse)

# Load custom function doc 

source("code_files/functions.R")

# From here onwards the shiny app starts

ui <- fluidPage(fluidPage(theme = shinytheme("flatly"),
    navbarPage(
        "Reimagining the United States: Geography based on State Relations",
    tabPanel("Interactive Map",
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
                        "Number of State Borders" = "borders",
                        "Median Age" = "age",
                        "Proportion of Urban Population" = "urban",
                        "Proportion of Rural Population" = "rural",
                        "Proportion of White Population" = "white",
                        "Proportion of Black/African American Population" = "black",
                        "Proportion of Hispanic/Latino American Population" = "hispanic"
                        )),
          selectInput("only_border", "Limit Relationships to Bordering States?",
                      c("Yes" = TRUE,
                        "No" = FALSE
                      )),
          sliderInput("scale",
                      "Size of Nodes:",
                      min = 0,
                      max = 200,
                      value = 100),
          sliderInput("edge_proportion",
                      "Proportion of Edges to include:",
                      min = 0,
                      max = 100,
                      value = 100)
    ),
    tabPanel("About",
    ),
    tabPanel("Walkthrough Video",
    )
))
)

# Define server logic required to network plot
server <- function(input, output) {

    read_data()
    
    output$netwPlot <- renderPlot({
        make_plot_master(edges_by = input$variable,
                         nodes_by = input$node_size,
                         only_border = input$only_border,
                         edge_proportion = (input$edge_proportion / 100),
                         scale_factor = (input$scale / 100))
                        }, height = 900, width = 1500)
    
    # output$video <- renderUI({embed_url("https://youtu.be/tMWxByiB5yY")})
    
}

# Run the application 
shinyApp(ui = ui, server = server)
