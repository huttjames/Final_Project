# Global variable to determine if this is the first plot 

first_plot <- TRUE

# This function reads in the data file and prepares it to plot

prepare_plot <- function(data_source = "raw-data/statenetworks.csv"){
  
  # Print first_plot
  print(first_plot)
  
  #Load the data into x
  
  x <- read_csv(data_source, 
                na = c("", "NA"))
  
  # Mutate the data
  
  x <- x %>%
    select(State1, State2, Border, RaceDif, State1_Pop, State2_Pop) %>% 
    mutate(logpop = log(State1_Pop / 500000, 3)) %>% 
    mutate(s1_larger = ifelse(State1_Pop >= State2_Pop, TRUE, FALSE)) %>%
    mutate(inverse_racedif = 1 / RaceDif)
  
  # Repeat the same process but filtering only for edges where state 1 is larger.
  # This should mean each pair of vertices is only joined by 1 edge. Since the
  # weights were symmetric this should have no effect.
  
  edge_list_unique <- x %>%
    filter(Border == 1) %>%
    filter(s1_larger == TRUE)
  
  state_data <- x %>%
    filter(State1 %in% c(edge_list_unique$State1, edge_list_unique$State2)) %>%
    select(State1, State1_Pop, logpop) %>%
    distinct()
  
  network_inv_race_unique <- graph_from_data_frame(edge_list_unique, directed = FALSE, vertices = state_data) %>%
    set_edge_attr("weight", value = edge_list_unique$inverse_racedif)
  
  l <- layout_with_fr(network_inv_race_unique)
  l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
  
  # Sized by population is commented out so only the sized by degree code is
  # running
  
  # plot.igraph(network_inv_race_unique, 
  #             layout= l, 
  #             rescale = FALSE,
  #             vertex.color = "gray",
  #             vertex.frame.color = "blue", 
  #             vertex.label.dist = 0,
  #             vertex.label.cex = 0.4,
  #             main="US States Separated By Racial Differences Across Bordering States:\n Sized by Population",
  #             frame = F,
  #             vertex.size = 5 * V(network_inv_race_unique)$logpop)
  
  # Reproduce plot but with degree causing sizing 
  
  deg <- degree(network_inv_race_unique, mode = "all")
  
  # Return a list of outputs
  
  return(list(network = network_inv_race_unique, l = l, deg = deg))
  
}

# This function takes in the data from prepare plot and plots it

plot_network <- function(data_source = "raw-data/statenetworks.csv", 
                         scale_factor = 1){
  
  # Check if first plot, if not then use the previous layout
  if(first_plot){prep <<- prepare_plot()
    first_plot <<- FALSE
  }

  plot.igraph(prep$network, 
              layout= prep$l, 
              rescale = FALSE,
              vertex.color = "gray",
              vertex.frame.color = "blue", 
              vertex.label.dist = 0,
              vertex.label.cex = 0.4,
              main="US States Separated By Racial Differences Across Bordering States: \n Sized by Number of State Borders",
              frame = F,
              vertex.size = (3 * prep$deg) * scale_factor)
  
  
}

