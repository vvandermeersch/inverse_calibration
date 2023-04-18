# Function to convert geopotential height to geometric height
# see https://confluence.ecmwf.int/display/CKB/ERA5%3A+compute+pressure+and+geopotential+on+model+levels%2C+geopotential+height+and+geometric+height

# author : V. Van der Meersch

geopotential_to_geometric <- function(h){
  
  # Earth's gravitational acceleration
  g0 <-  9.80665
  
  # Mean radius of the earth
  re <- 6371000
  
  # Get geopotential height in meters
  h_m <- h/g0
  
  # Convert to geometric height 
  z <- re*h_m/(re-h_m)
  
  return(z)
  
}