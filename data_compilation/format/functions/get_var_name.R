# Function to get variable name as coded in NC files of ERA5-Land database
# Or as needed as input variables for Phenofit

# author : V. Van der Meersch

get_var_name <- function(var, stat=NULL, type){
  long_names <- c("2m_temperature", "total_precipitation", "surface_solar_radiation_downwards", "surface_pressure", 
                  "wind", "relative_humidity", "2m_dewpoint_temperature", "10m_u_component_of_wind", "10m_v_component_of_wind",
                  "potential_evaporation")
  era5_names <- c("t2m", "tp", "ssrd", "ps",  NA, NA, "dpt", "u10", "v10", "pev")
  
  if(type == "ERA5"){ return(era5_names[which(long_names == var)]) }
  
  if(type == "PHENOFIT" | type=="Phenofit"){
    if(stat=="mean"){
      phenofit_names <- c("tmp", NA, NA, "prs", "wnd", "RH", "dtm", NA, NA, NA)
      return(phenofit_names[which(long_names == var)])
    }
    if(stat=="max"){
      phenofit_names <- c("tmx", NA, NA, NA, NA, "RHmax", NA, NA, NA, NA)
      return(phenofit_names[which(long_names == var)])
    }
    if(stat=="min"){
      phenofit_names <- c("tmn", NA, NA, NA, NA, "RHmin", NA, NA, NA, NA)
      return(phenofit_names[which(long_names == var)])
    }
    if(stat=="sum"){
      phenofit_names <- c(NA, "pre", "glo", NA, NA, NA, NA, NA, NA, "pet")
      return(phenofit_names[which(long_names == var)])
    }
  }
  
}
