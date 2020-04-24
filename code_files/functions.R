# When we first load this app we call the function.R file and initialise with an
# indicator variable that shows this is the first time we have built a plot

first_plot <- TRUE


# Create a function to read in the data 

read_data <- function(data_source = "raw-data/statenetworks.csv", 
                      col_types=cols()){
  
  #Load the data into x
  
  x <- read_csv(data_source, 
                na = c("", "NA"))
  
  # Mutate the data to get the relevant variables and sensible relative scaling
  
  x <- x %>%
    select(State1, State2, Border,
           State1_Pop, State2_Pop, ACS_Migration,
           Distance, State1_Long, State2_Long, State1_Lat, State2_Lat,
           RaceDif, IncomingFlights, Imports, IdeologyDif, ReligDif) %>% 
    mutate(logpop = log(State1_Pop / 500000, 3)) %>% 
    mutate(s1_larger = ifelse(State1_Pop >= State2_Pop, TRUE, FALSE)) %>%
    mutate(inverse_racedif = 1 / RaceDif)
  
  # Make this available as a global variable 
  
  x <<- x
  
  # Also create a tibble of each state with necessary characteristics for later
  
  x_state_data <<- x %>%
    select(State1, State1_Lat, State1_Long, State1_Pop, logpop) %>% 
    distinct
  
}

# Function to prepare the data to make into a network 

trim_data <- function(only_border = TRUE, undirected = TRUE, data = x){
  
  # Mutate the original data and return the object as trimmed_x
  # First check the border condition 
  
    if(only_border){
      trimmed_x <- x %>% filter(Border == 1)
    }
    else{
      trimmed_x <- x
    }
  
  # Then check the undirected condition 
  
    if(undirected){
      trimmed_x <- trimmed_x %>% filter(s1_larger == TRUE)
    }
  
  trimmed_x <<- trimmed_x
}


# Since I can only return one object from a function in R all of the outputs are
# returned as a list at the end and the elements of the list are accessed by a
# later function. This function creates the layout for a network if called. 

prepare_plot <- function(data = trimmed_x){
  
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

