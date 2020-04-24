# Since I can only return one object from a function in R all of the outputs are
# returned as a list at the end and the elements of the list are accessed by a
# later function. This function creates the layout for a network if called. 

prepare_plot <- function(data = trimmed_x, nodes_by = "borders"){
  
  # Modify x_state_data to drop states without borders if necessary
  
  x_state_data <- x_state_data %>% 
    filter(State1 %in% c(trimmed_x$State1, trimmed_x$State2))
  
  # Assign this network to an object
  
  network <- graph_from_data_frame(trimmed_x,
                                   directed = FALSE,
                                   vertices = x_state_data) %>%
    set_edge_attr("weight", value = trimmed_x$inverse_racedif)
  
  # Assign the layout to an object to be returned
  
  l <- layout_with_fr(network)
  l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
  
  # Assign the degree of nodes to an object to be returned
  
  deg <- degree(network, mode = "all")
  
  # Return a list of outputs for use in the next function
  
  return(list(network = network, l = l, deg = deg))
  
}
