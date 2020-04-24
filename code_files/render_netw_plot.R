
render_netw_plot <- function(scale_factor = 1, nodes_by = "pop"){
  
  # Set vertex size argument 
  
  if(nodes_by == "pop"){
    vertex_size_by <- (3 * V(prep$network)$pop)
  }
  else{
    vertex_size_by <- (2 * V(prep$network)$total_borders)
  }
  
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
