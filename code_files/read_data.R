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
