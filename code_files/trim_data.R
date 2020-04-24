# Function to prepare the data to make into a network 

trim_data <- function(only_border = TRUE, 
                      undirected = TRUE, 
                      data = x){
  
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

