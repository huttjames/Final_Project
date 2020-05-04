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
    tabPanel("Curated Insights",
             htmlOutput("CI_1"), 
             HTML('<center><img src="flights.gif" width="700"></center>'),
             htmlOutput("CI_2"), 
             HTML('<center><img src="race.gif" width="700"></center>'),
             htmlOutput("CI_3"), 
             HTML('<center><img src="religion.gif" width="700"></center>'),
             htmlOutput("CI_4"), 
             HTML('<center><img src="political.gif" width="700"></center>'),
             htmlOutput("CI_5"), 
             HTML('<center><img src="distance.gif" width="700"></center>'),
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
    "<br>
    <b> Name:</b> James Hutt 
    <br>
    <b>Email:</b> jhutt@g.harvard,edu
    <br><br>
    <b>Bio:</b> James is studying at Harvard as a Special Student in the Graduate School of Arts and Sciences until May 2020. 
    James' interests are centred around how technology impacts out societies, cultures and politics. 
    During his time at Harvard James took CS50 as well as GOV 1005 (which this project is a part of) but refuses to pick a side in the long running battle over which is better. 
    <br><br>
    <a href='https://github.com/huttjames/'> GitHub <a/>
    <br>
    <a href='https://www.linkedin.com/in/james-w-hutt/'> LinkedIn <a/><br/><br/>
    <a href='https://github.com/huttjames/Final_Project'> Repo for this project <a/>
    <br><br>
    <b> Thanks: </b> James would like to express particular thanks to Preceptor David Kane and TF Mitchell Kilborn, 
    without who's teaching, support and help little of this project would have been possible. 
    "
                                     )})
    
    urlrepo <- a("this repo", href = "https://github.com/huttjames/Final_Project")
    
    output$repo <- renderUI({
        tagList("My code for this Shiny app can be found at", urlrepo)
        })
    
    output$proj_info <- renderUI({
        HTML("<h2><b> About This Project</b></h2>
              <h3><b> Introduction</b></h3>
              <p> The map of the state outlines is one of the most recognisable images in all of geography and politics. 
              But, with states increasingly defined by their composition rather than location, simply thinking of which states are next to each other spatially may be insufficient for understanding how they relate.
              This project reshapes the US,mapping states according to flight paths, migration, and political, religious and racial homogeneity. </p>
              <br>
              <p> Finally, the East and West Coast can be together, like they always wanted.</p>
              <h3><b> Sources of Data</b></h3>
              <ul>
                  <li>
                      <a href='https://ippsr.msu.edu/public-policy/state-networks'> State Networks - MSU <a/><br>
                      A dataset covering all 2550 directional pairs of states, including DC, with data split into 
                      Distance,Travel,Migration; Economic; Political; Policy; Demographic data. 
                      <br>
                      There are 114 variables available for each pair, described in the 
                      <a href='https://ippsr.msu.edu/sites/default/files/state_networks/state_networks_codebook.pdf'> Code Book <a/>
                      <br> 
                      The data is available without an API as a .csv or .xlsx file. 
                      <br> 
                      Hat tip to <a href='https://tinyletter.com/data-is-plural/letters/data-is-plural-2019-09-18-edition'> Data is Plural <a/> who featured this data set on their blog which first alerted me to its existence. 
                  </li><br>
                  <li>
                      <a href='https://data.census.gov/cedsci/'> US Census - 2010 <a/><br>
                      The dataset from the 2010 national census. 
                      Available using the <a href='https://walkerke.github.io/tidycensus/articles/basic-usage.html#searching-for-variables'> Tidycensus <a/>
                      The data is, if anything, too abundant with 3346 variables available for each state. 
                  </li>
              </ul>
              
          
              
              <br>
              <h3><b> Explananation of Variables</b></h3>
              <h4><b> Variables to Position the States </b></h4>
              <ul>
                  <li>
                      <b>Racial Similarity</b><br>
                      <em>Data Source</em>: MSU 
                      <br>
                      <em>Description</em>: RaceDif is the total absolute value of differences in each racial group between two states, where the possible racial groups are: Latinx, White, Black, Asian and Native American.
                      <br>
                      <em>Transformation</em>: The variable 1 / RaceDif is used for plotting, so racially similar states, which have a smaller value of RaceDif are closer in space.
                  </li>
                  <li>
                      <b>Distance Between State Capitals</b><br>
                      <em>Data Source</em>: MSU 
                      <br>
                      <em>Description</em>: Haversine distance between State 1 and State 2 Capitals in kilometers. Calculated using the “geosphere” package.
                      <br>
                      <em>Transformation</em>: The variable 1 / Distance^1.3 is used for plotting, so geographically closer capitals are plotted more closely. The power is included to increase dispersion. 
                  </li>
                  <li>
                      <b>Migration</b><br>
                      <em>Data Source</em>: MSU 
                      <br>
                      <em>Description</em>: ACS_Migration is the number of people migrating from State 2 to State 1 in one year, 2017. Population and
                                            ACS_Migration variables Collected from U.S. Census American Community
                                            Survey
                      <br>
                      <em>Transformation</em>: The variable log(ACS_Migration + 1) is plotted, so that states with more migration are plotted more closely together. 
                      There are two lines connecting each state representing migration in each direction. 
                      The log transform decreases dispersion, preventing only the largest values being observable. 
                      The +1 shift accounts for 0 values in linear space, which then have 0 value following the log transform. 
                  </li>
                  <li>
                      <b>Flights</b><br>
                      <em>Data Source</em>: MSU 
                      <br>
                      <em>Description</em>: IncomingFlights shows the number of Flights from State 2 with destination in State 1. From Bureau of Transportation
                                            Statistics (BTS) Origin and Destination Survey, DB1B Coupon (10% sample of
                                            airline tickets from reporting carriers). 2019.
                      <br>
                      <em>Transformation</em>: The variable log(IncomingFlights + 1) is plotted, so that states with more flights are plotted more closely together. 
                      There are two lines connecting each state representing flights in each direction. 
                      The log transform decreases dispersion, preventing only the largest values being observable. 
                      The +1 shift accounts for 0 values in linear space, which then have 0 value following the log transform.
                  </li>
                  <li>
                      <b>Trade (Imports)</b><br>
                      <em>Data Source</em>: MSU 
                      <br>
                      <em>Description</em>: Imports is the aggregated value of trade from State 2 to State 1 in one year. 2017 BTS
                                            Commodity Flow Survey. BTS provide the following information: 'The CFS is a shipper survey of approximately 100,000 establishments from
                                            the industries of mining, manufacturing, wholesale trade, auxiliaries (i.e.
                                            warehouses and distribution centers), and select retail and service trade
                                            industries that ship commodities. Data requested by the CFS includes the
                                            type of commodities shipped, their origin and destination, their value and
                                            weight, and mode(s) of transport. The CFS provides a comprehensive
                                            multimodal picture of national freight flows and represents the only publicly
                                            available source of data for the highway mode.'
                      <br>
                      <em>Transformation</em>: The variable log(Imports + 1) is plotted, so that states with more trade are plotted more closely together. 
                      There are two lines connecting each state representing imports in each direction. 
                      The log transform decreases dispersion, preventing only the largest values being observable. 
                      The +1 shift accounts for 0 values in linear space, which then have 0 value following the log transform. 
                  </li>
                  <li>
                      <b>Political Ideology Differences</b><br>
                      <em>Data Source</em>: MSU 
                      <br>
                      <em>Description</em>: IdeologyDif is the difference between the two states ideology, From Correlates of State Policy Project. 'Yearly measure,
                                            giving the proportion of liberal identifiers minus the proportion of conservative
                                            identifiers in each state. A positive score indicates a more liberal state citizenry.'
                      <br>
                      <em>Transformation</em>: The variable 1 / (abs(IdeologyDif) + 0.001) is plotted to ensure that states with more similar ideologies are plotted more closely together. 
                      The abs term ignores the difference of direction of difference. 
                      The + 0.001 transformation removes the concern around 0 values. 
                  </li>
                  <li>
                      <b>Religious Differences</b><br>
                      <em>Data Source</em>: MSU 
                      <br>
                      <em>Description</em>: ReligDif is the Total absolute value of differences in each of the following religious groups:
                                            Evangelicals, Mainline Protestants, Black Protestants, Catholics, Mormons,
                                            Jews, Muslims, Buddhists, Hindus, and Nones.
                      <br>
                      <em>Transformation</em>: The variable 1 / (ReligDif + 0.001) is plotted to ensure that states with more similar ideologies are plotted more closely together. 
                      The abs term ignores the difference of direction of difference. 
                      The + 0.001 transformation removes the concern around 0 values.
                  </li>
              </ul>
              
              <h4><b> Variables to Size the States (Nodes)</b></h4>
              <ul>
                  <li>
                      <b>Population</b><br>
                      <em>Data Source</em>: MSU 
                      <br>
                      <em>Description</em>: Population in 2017 accoring to the State Networks data set.
                      <br>
                      <em>Transformation</em>: Population / 500,000 is put on a log base 3 scale. 
                      This reduces dispersion between the very large and very small states to make nodes easier to observe. 
                      The division (which is a linear shift in log space) means that the range of node sizes is appropriate at 100% magnification. 
                  </li>
                  <li>
                      <b>Number of State Borders</b><br>
                      <em>Data Source</em>: MSU 
                      <br>
                      <em>Description</em>: Sum of the number of state pairs which share a border for the state in question. 
                      <br>
                      <em>Transformation</em>: The MSU dataset presents a binary Border variable for each state pair. 
                      These are summed by state for plotting. Sizes are defined as 2 * number of borders. 
                  </li>
                  <li>
                      <b>Median Age</b><br>
                      <em>Data Source</em>: US Census 2010 
                      <br>
                      <em>Description</em>: Variable P013001 from the US Census. Median Age by state. 
                      <br>
                      <em>Transformation</em>: Nodes are sized according to P013001 - 25, to improve dispersion and maintain node sizes within an easily visible range.  
                  </li>
                  <li>
                      <b>Proportion of ... Population</b><br>
                      <em>Data Source</em>: US Census 2010 
                      <br>
                      <em>Description</em>: This variable represents the ratio of 
                      P002005 (rural), P002002 (urban), P003002 (white), P003003 (black) and P004003 (hispanic) populations to 
                      P001001 (total) populations according to the 2010 census. 
                      <br>
                      <em>Transformation</em>: Size is equal to the proportion * 20, to give nodes in the size 0 to 20 for 0 to 100% respectively. 
                  </li>
              </ul>
                             
         <br> <br>")
    })
    
    # Code for curated insights
    output$CI_1 <- renderUI({HTML("<h2> 1) Delaware is the odd one out when it comes to flights </h2>
                                  All the states form a giant component even when a very low fraction of edges are considered. Delaware has no flights into it at all. <br/>")})
    
    output$CI_2 <- renderUI({HTML("<h2> 2) New England sticks out as racially distinct. </h2>
                                  New York and New Jersey don't quite fit with any of the states around them. If you squint this almost looks like the map we are used to, just rotated 90% <br/>")})
    
    output$CI_3 <- renderUI({HTML("<h2> 3) Utah is a strange place. That'll be the Mormons I guess.  </h2>
                                  Utah is the clear outlier religiously. Also, and more surprisingly, the youngest state which is clearly visible here. As we plot fewer and few edges the states hold together relatively well apart from Utah.  <br/>")})
    
    output$CI_4 <- renderUI({HTML("<h2> 4) Ideological differences - the coasts flee the centre </h2>
                                  We all know the coasts are distinct, but this shows us quite how much so. It's not true to say this is all a rural-urban divide however... <br/>")})
    
    output$CI_5 <- renderUI({HTML("<h2> 5) Bringing distance back </h2>
                                  To end on a familiar note I plot by distance between state capitals. Comforting to see that Alaska and Hawaii are far from the centre as they should be.  <br/>")})
    
    # Code for Walkthrough video 
    
        output$video <- renderUI({embed_url("https://youtu.be/vsEeqdtgrqQ")})
    
}

# Run the application 
shinyApp(ui = ui, server = server)
