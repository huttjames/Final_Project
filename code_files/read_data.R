# Create a function to read in the data 

read_data <- function(data_source = "raw-data/statenetworks.csv", 
                      col_types=cols()){
  
  #Load the data into x
  
  x <- read_csv(data_source, 
                na = c("", "NA")) %>%
    filter(State1 != "District of Columbia",
           State2 != "District of Columbia")
  
  # Mutate the data to get the relevant variables and sensible relative scaling
  
  x <- x %>%
    select(State1, State2, Border,
           State1_Pop, State2_Pop, ACS_Migration,
           Distance, State1_Long, State2_Long, State1_Lat, State2_Lat,
           RaceDif, IncomingFlights, Imports, IdeologyDif, ReligDif) %>% 
    mutate(logpop = log(State1_Pop / 500000, 3)) %>% 
    mutate(s1_larger = ifelse(State1_Pop >= State2_Pop, TRUE, FALSE)) %>%
    mutate(inverse_racedif = 1 / RaceDif, 
           inverse_distance_sq = 1 / Distance^1.3,
           inverse_migration = log(ACS_Migration + 1),
           IncomingFlights = log(IncomingFlights + 1),
           inverse_imports = 1 / log(Imports + 1), 
           inverse_ideologydif = 1 / (IdeologyDif + 0.001), 
           inverse_religdif = 1 / ReligDif)
    
  borders <- x %>%
    group_by(State1) %>%
    summarise(total_borders = sum(Border))
  
  x <- x %>%
    left_join(borders, by = c("State1" = "State1"))
  
  # Make this available as a global variable 
  
  x <<- x
  
  # Also create a tibble of each state with necessary characteristics for later
  
  x_state_data <- x %>%
    select(State1, State1_Pop, logpop, total_borders) %>% 
    distinct()
  
  # Load in census data 
  
  state_census_data_to_join <- readRDS("raw-data/Tidycensus/state_census_data.rds")
  
  # Join census data to Michigan data 
  
  x_state_data <<- x_state_data %>%
    left_join(state_census_data_to_join, by = c("State1" = "state"))
}
