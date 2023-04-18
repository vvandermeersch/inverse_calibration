# Function to convert units as needed by Phenofit

# author : V. Van der Meersch

convert_units <- function(value, var){
  if(var == "2m_temperature" | var == "2m_dewpoint_temperature"){
    # Kelvin to Celsius
    value_c <- sapply(value, function(i){i-273.15})
  }
  if(var == "total_precipitation"){
    # m to mm
    value_c <- sapply(value, function(i){i*1000})
  }
  if(var == "surface_solar_radiation_downwards"){
    # J/m2 to MJ/m2
    value_c <- sapply(value, function(i){i/1000000})
  }
  if(var == "surface_pressure"){
    # Pa to kPa
    value_c <- sapply(value, function(i){i/1000})
  }
  if(var == "potential_evaporation"){
    # m to mm, and negative to positive
    value_c <- sapply(value, function(i){- i*1000})
  }
  
  return(value_c)
}

