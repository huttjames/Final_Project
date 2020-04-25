
render_netw_plot <- function(scale_factor = 1, nodes_by = "pop"){
  
  # Set vertex size argument 
  
  vertex_size_by <- case_when(nodes_by == "pop" ~ (3 * V(prep$network)$pop),
                              nodes_by == "borders" ~ (2 * V(prep$network)$total_borders),
                              nodes_by == "age" ~ (V(prep$network)$median_age - 30),
                              nodes_by == "urban" ~ (20 * V(prep$network)$prop_urban),
                              nodes_by == "rural" ~ (20 * V(prep$network)$prop_rural),
                              nodes_by == "white" ~ (20 * V(prep$network)$prop_white),
                              nodes_by == "black" ~ (20 * V(prep$network)$prop_black),
                              nodes_by == "hispanic" ~ (20 * V(prep$network)$prop_hisp),
                              TRUE ~ (3 * V(prep$network)$pop))
  
  # if(nodes_by == "pop"){
  #   vertex_size_by <- (3 * V(prep$network)$pop)
  # }
  # else{
  #   vertex_size_by <- (2 * V(prep$network)$total_borders)
  # }
  # 
  
  # Prep must exist for this function to work, but this function will only be
  # called from within the master function which produces prep
  
  # At this stage we have the prep object, either just made now, or made
  # earlier. We use this object to plot the network graph. Only the size of
  # nodes is being changed each time this function is called.
  
  plot.igraph(prep$network, 
              layout= prep$l, 
              rescale = FALSE,
              vertex.color = "gray",
              vertex.frame.color = "blue", 
              vertex.label.dist = 0,
              vertex.label.cex = 1,
              main="US States Separated By TO DO: \n Sized by TODO",
              frame = F,
              vertex.size = vertex_size_by * scale_factor)
  
  
  
}
