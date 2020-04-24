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
                             scale_factor = 1){
  
  # If this is the first plot then trim and prepare the data to produce the plot
  
  if(first_plot){
    trim_data()
    prep <<- prepare_plot()
    first_plot <<- FALSE

    # Save the 3 variables as global variables to check in the future if they have
    # changed
    
    last_edges_by <<- edges_by
    last_nodes_by <<- nodes_by
    last_scale_factor <<- scale_factor
    
    # Plot 
    
    render_netw_plot(scale_factor = scale_factor, 
                     nodes_by = nodes_by)
    
    return()
    
  }
  
  # If only scale factor or nodes_by has changed then replot with the new scale facter 
  
  if(scale_factor != last_scale_factor | nodes_by != last_nodes_by) {
    
    # Save the 3 variables as global variables to check in the future if they have
    # changed
    
    last_edges_by <<- edges_by
    last_nodes_by <<- nodes_by
    last_scale_factor <<- scale_factor
    
    # Plot 
    
    render_netw_plot(scale_factor = scale_factor, 
                     nodes_by = nodes_by)
    
    return()
    
  }
  
  # If edges_by has changed then rerun prepare_plot then plot 
  
  if(edges_by != last_edges_by) {
    
    # Save the 3 variables as global variables to check in the future if they have
    # changed
    
    last_edges_by <<- edges_by
    last_nodes_by <<- nodes_by
    last_scale_factor <<- scale_factor
    
    # First trim the data 
    
    trim_data()
    
    # Rerun prepare_plot specifying the new node sizing 
    
    prep <<- prepare_plot()
    
    # Plot 
    
    render_netw_plot(scale_factor = scale_factor, 
                     nodes_by = nodes_by)
    
    return()
    
  }

}




