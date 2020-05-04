# Source Functions 

source("code_files/read_data.R")
source("code_files/trim_data.R")
source("code_files/prepare_plot.R")
source("code_files/render_netw_plot.R")

# When we first load this app we call the function.R file and initialise with an
# indicator variable that shows this is the first time we have built a plot

first_plot <- TRUE

# Definte a control function which calls the other functions as necessary 

make_plot_master <- function(edges_by = "RaceDif",
                             nodes_by = "borders",
                             only_border = TRUE,
                             scale_factor = 1,
                             edge_proportion = 1){
  
  # Decide whether graph should be directed or undirected
  
  undirected <- if_else(edges_by %in% c("RaceDif", "Distance", "IdeologyDif", "ReligDif"),
                        TRUE, 
                        FALSE)
  
  # If this is the first plot then trim and prepare the data to produce the plot
  
  if(first_plot){
    trim_data(only_border = only_border,
              undirected = undirected)
    prep <<- prepare_plot(edges_by = edges_by,
                          edge_proportion = edge_proportion)
    first_plot <<- FALSE

    # Save the 5 variables as global variables to check in the future if they have
    # changed
    
    last_edges_by <<- edges_by
    last_nodes_by <<- nodes_by
    last_only_border <<- only_border
    last_scale_factor <<- scale_factor
    last_edge_proportion <<- edge_proportion
    
    # Plot 
    
    render_netw_plot(scale_factor = scale_factor, 
                     edges_by = edges_by,
                     nodes_by = nodes_by, 
                     edge_proportion = edge_proportion)
    
    return()
    
  }
  
  # If only scale factor or nodes_by has changed then replot with the new scale facter 
  
  if(scale_factor != last_scale_factor | nodes_by != last_nodes_by) {
    
    # Save the 3 variables as global variables to check in the future if they have
    # changed
    
    last_edges_by <<- edges_by
    last_nodes_by <<- nodes_by
    last_only_border <<- only_border
    last_scale_factor <<- scale_factor
    last_edge_proportion <<- edge_proportion
    
    # Plot 
    
    render_netw_plot(scale_factor = scale_factor, 
                     edges_by = edges_by,
                     nodes_by = nodes_by, 
                     edge_proportion = edge_proportion)
    
    return()
    
  }
  
  # If edges_by has changed then rerun prepare_plot then plot 
  
  if(edges_by != last_edges_by | only_border != last_only_border | edge_proportion != last_edge_proportion) {
    
    # Save the 3 variables as global variables to check in the future if they have
    # changed
    
    last_edges_by <<- edges_by
    last_nodes_by <<- nodes_by
    last_only_border <<- only_border
    last_scale_factor <<- scale_factor
    last_edge_proportion <<- edge_proportion
    
    
    # First trim the data 
    
    trim_data(only_border = only_border,
              undirected = undirected)
    
    # Rerun prepare_plot specifying the new node sizing 
    
    prep <<- prepare_plot(edges_by = edges_by,
                          edge_proportion = edge_proportion)
    
    # Plot 
    
    render_netw_plot(scale_factor = scale_factor, 
                     edges_by = edges_by,
                     nodes_by = nodes_by, 
                     edge_proportion = edge_proportion)
    
    return()
    
  }
  
  # If we get to this point it is neither the first plot nor has anything
  # changed so we should just plot again what we have lined up
  
  render_netw_plot(scale_factor = scale_factor, 
                   edges_by = edges_by,
                   nodes_by = nodes_by, 
                   edge_proportion = edge_proportion)
  

}




