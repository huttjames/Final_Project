
render_netw_plot <- function(scale_factor = 1, edges_by = "RaceDif", nodes_by = "pop"){
  
  # Set vertex size argument 
  
  vertex_size_by <- case_when(nodes_by == "pop" ~ (3 * V(prep$network)$pop),
                              nodes_by == "borders" ~ (2 * V(prep$network)$total_borders),
                              nodes_by == "age" ~ (V(prep$network)$median_age - 25),
                              nodes_by == "urban" ~ (20 * V(prep$network)$prop_urban),
                              nodes_by == "rural" ~ (20 * V(prep$network)$prop_rural),
                              nodes_by == "white" ~ (20 * V(prep$network)$prop_white),
                              nodes_by == "black" ~ (20 * V(prep$network)$prop_black),
                              nodes_by == "hispanic" ~ (20 * V(prep$network)$prop_hisp),
                              TRUE ~ (3 * V(prep$network)$pop))
  
  # Prepare variables for inclusion in the title of the chart 
  
  name_vertex <- case_when(nodes_by == "pop" ~ "Sized by Population",
                              nodes_by == "borders" ~ "Sized by Number of Borders",
                              nodes_by == "age" ~ "Sized by Median Age",
                              nodes_by == "urban" ~ "Sized by Urban Population Fraction",
                              nodes_by == "rural" ~ "Sized by Rural Population Fraction",
                              nodes_by == "white" ~ "Sized by White Population Fraction ",
                              nodes_by == "black" ~ "Sized by Black/African American Pop Fraction",
                              nodes_by == "hispanic" ~ "Sized by Hispanic/Latina Pop Fraction",
                              TRUE ~ "")
  
  name_edge <- case_when(edges_by == "RaceDif" ~ "Sum of Absolute Racial Differences",
                         edges_by == "Distance" ~ "Distance Between State Capitals",
                         edges_by == "ACS_Migration" ~ "Migration in Each Direction",
                         edges_by == "IncomingFlights" ~ "Number of Incoming Flights",
                         edges_by == "Imports" ~ "Amount of Imports",
                         edges_by == "IdeologyDif" ~ "Magnitude of Ideological Differences",
                         edges_by == "ReligDif" ~ "Sum of Absolute Religious Differences",
                         TRUE ~ "Sum of Absolute Racial Differences")
  
  # Prep must exist for this function to work, but this function will only be
  # called from within the master function which produces prep
  # We use this object to plot the network graph. Only the size of
  # nodes is being changed each time this function is called.
  
  plot.igraph(prep$network, 
              layout= prep$l, 
              rescale = FALSE,
              vertex.color = "gray",
              vertex.frame.color = "blue", 
              vertex.label.dist = 0,
              vertex.label.cex = 1,
              main=paste("US States Separated by", name_edge, "\n", name_vertex),
              frame = F,
              vertex.size = vertex_size_by * scale_factor)
  
  
  
}
