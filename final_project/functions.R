# When we first load this app we call the function.R file and initialise with an
# indicator variable that shows this is the first time we have built a plot

first_plot <- TRUE

# Next I define a function called prepare plot. It will be run later in the code
# only if this is the first plot. If it is not the first plot I will use the
# outputs which have already been stored. Since I can only return one object
# from a function in R all of the outputs are returned as a list at the end and
# the elements of the list are accessed by a later function

prepare_plot <- function(data_source = "raw-data/statenetworks.csv"){
  
  #Load the data into x
  
  x <- read_csv(data_source, 
                na = c("", "NA"))
  
  # Mutate the data to get the relevant variables and sensible relative scaling
  
  x <- x %>%
    select(State1, State2, Border, RaceDif, State1_Pop, State2_Pop) %>% 
    mutate(logpop = log(State1_Pop / 500000, 3)) %>% 
    mutate(s1_larger = ifelse(State1_Pop >= State2_Pop, TRUE, FALSE)) %>%
    mutate(inverse_racedif = 1 / RaceDif)
  
  # Filtering only for edges where state 1 is larger. This should mean each pair
  # of vertices is only joined by 1 edge. Since the weights were symmetric this
  # should have no effect.
  
  edge_list_unique <- x %>%
    filter(Border == 1) %>%
    filter(s1_larger == TRUE)
  
  # Use this edge list to filter down the rows to distinct data entries only
  
  state_data <- x %>%
    filter(State1 %in% c(edge_list_unique$State1, edge_list_unique$State2)) %>%
    select(State1, State1_Pop, logpop) %>%
    distinct()
  
  # Assign this network to an object
  
  network_inv_race_unique <- graph_from_data_frame(edge_list_unique, directed = FALSE, vertices = state_data) %>%
    set_edge_attr("weight", value = edge_list_unique$inverse_racedif)
  
  # Assign the layout to an object to be returned
  
  l <- layout_with_fr(network_inv_race_unique)
  l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
  
  # Assign the degree of nodes to an object to be returned
  
  deg <- degree(network_inv_race_unique, mode = "all")
  
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
  

  
  # Return a list of outputs for use in the next function
  
  return(list(network = network_inv_race_unique, l = l, deg = deg))
  
}

# This function, plot_network, is called by the app. It does 2 things. Firstly
# it checks whether the data has been transformed into a graph object. If it
# has, then it uses these values stored in the global variable prep to render
# the plot. If it hasn't been manipulated already this function calls the first
# function to produce the object prep

plot_network <- function(scale_factor = 1){
  
  # Check if first plot. If this is the first plot then first_plot = TRUE from
  # the initial assignment. In this case we call prepare_plot which returns a
  # prep object and changes first_plot to false, both as global variables. Note
  # for the future <<- is the operator to change the global variables from
  # within a function. Used twice here.
  
  if(first_plot){prep <<- prepare_plot()
    first_plot <<- FALSE
  }
  
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
              main="US States Separated By Racial Differences Across Bordering States: \n Sized by Number of State Borders",
              frame = F,
              vertex.size = (3 * prep$deg) * scale_factor)
  
  
}

