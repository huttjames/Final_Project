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
                          
                          # Adding the favicon using code from Bridger Gordons project for the HTML
                          
                          HTML('<script> document.title = "Reshaping the United States"; </script>'),
                          tags$head(tags$link(rel="shortcut icon", href="icon.ico")),
    navbarPage(
        "Reshaping the United States: Geography based on State Relations",
    tabPanel("Interactive Map",
      sidebarLayout(
          sidebarPanel(
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
          mainPanel(
              plotOutput("netwPlot")
          )
      )
    ),
    tabPanel("About",
             sidebarLayout(
                 sidebarPanel(
                     htmlOutput("about_me"), 
                     img(src="headshot.jpg",width="100%"), 
                     htmlOutput("bioinfo"),
                     width = 3),
                 mainPanel(
                     htmlOutput("proj_info")
                 )
             )
    ),
    tabPanel("Walkthrough Video",
             uiOutput("video")
    )
))
)

# Define server logic required to network plot
server <- function(input, output) {

    read_data()
    
    # Code for interactive map 
    
    output$netwPlot <- renderPlot({
        make_plot_master(edges_by = input$variable,
                         nodes_by = input$node_size,
                         only_border = input$only_border,
                         edge_proportion = (input$edge_proportion / 100),
                         scale_factor = (input$scale / 100))
                        }, width = 900, height = 900, execOnResize = TRUE)
    
    # Code for about page
    
    output$about_me <- renderUI({HTML("<b> About the Developer </b> <br/><br/>")})
    
    output$bioinfo <- renderUI({HTML(
    "<br/>
    <b> Name:</b> James Hutt 
    <br/>
    <b>Email:</b> jhutt@g.harvard,edu
    <br/><br/>
    <b>Bio:</b> James is studying at Harvard as a Special Student in the Graduate School of Arts and Sciences until May 2020. James' interests are centred around how technology impacts out societies, cultures and politics. During his time at Harvard James took CS50 as well as GOV 1005 (which this project is a part of) but refuses to pick a side in the long running battle over which is better. 
    <br/><br/>
    <a href='https://github.com/huttjames/'> GitHub <a/>
    <br/>
    <a href='https://www.linkedin.com/in/james-w-hutt/'> LinkedIn <a/><br/><br/>
    <a href='https://github.com/huttjames/Final_Project'> Repo for this project <a/>
    <br/>
    "
                                     )})
    
    urlrepo <- a("this repo", href = "https://github.com/huttjames/Final_Project")
    
    output$repo <- renderUI({
        tagList("My code for this Shiny app can be found at", urlrepo)
        })
    
    output$proj_info <- renderUI({
        HTML("<br> <h2> About This Project:</h2>
                            To Do 
                             
         <br> <br>")
    })
    
    # Code for Walkthrough video 
    
        output$video <- renderUI({embed_url("https://youtu.be/vsEeqdtgrqQ")})
    
}

# Run the application 
shinyApp(ui = ui, server = server)
