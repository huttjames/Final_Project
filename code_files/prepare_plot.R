# Since I can only return one object from a function in R all of the outputs are
# returned as a list at the end and the elements of the list are accessed by a
# later function. This function creates the layout for a network if called. 

prepare_plot <- function(data = trimmed_x,
                         edges_by = "RaceDif", 
                         edge_proportion = 1){
  
  # Modify x_state_data to drop states without borders if necessary
  
  x_state_data <- x_state_data %>% 
    filter(State1 %in% c(trimmed_x$State1, trimmed_x$State2))
  
  # Assign this network to an object
  
  network <<- graph_from_data_frame(trimmed_x,
                                   directed = FALSE,
                                   vertices = x_state_data) %>%
    set_vertex_attr("pop", index = x_state_data$State1, value = x_state_data$logpop) %>%
    set_vertex_attr("borders", index = x_state_data$State1, value = x_state_data$total_borders) 
  
  # Assign the values required to the edge_variable
  
  edge_variable <- case_when(edges_by == "RaceDif" ~ trimmed_x$inverse_racedif,
                             edges_by == "Distance" ~ trimmed_x$inverse_distance_sq,
                             edges_by == "ACS_Migration" ~ trimmed_x$inverse_migration,
                             edges_by == "IncomingFlights" ~ trimmed_x$IncomingFlights,
                             edges_by == "Imports" ~ trimmed_x$inverse_imports,
                             edges_by == "IdeologyDif" ~ trimmed_x$inverse_ideologydif,
                             edges_by == "ReligDif" ~ trimmed_x$inverse_religdif,
                             TRUE ~ trimmed_x$inverse_racedif)
  
  # Update the edge weights based on the variable selected
  
  network <<- network %>% 
    set_edge_attr("weight", value = edge_variable) 
  
  # Set the quantile 
  
  cutoff <<- quantile(E(network)$weight, (1 - edge_proportion))
  
  # Filter for only edges above a certain threshold value 
  
  network_filtered <<- network %>%
    delete.edges(which(E(network)$weight <= cutoff))
    
  
  # Assign the layout to an object to be returned
  
  l <- layout_with_fr(network_filtered)
  l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
  
  # Assign the degree of nodes to an object to be returned
  
  deg <- degree(network_filtered, mode = "all")
  
  # Return a list of outputs for use in the next function
  
  return(list(network = network_filtered, l = l, deg = deg))
  
}
