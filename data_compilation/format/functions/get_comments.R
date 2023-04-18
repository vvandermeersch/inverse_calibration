# Function to get Phenofit input variable definitions

# author : V. Van der Meersch

get_comments <- function(var){
  if(var=='glo'){
    return("Variable : daily global radiation (MJ/m²)")
  }
  else if(var=='pre'){
    return("Variable : daily precipitation, comprising rain and snow (mm)")
  }
  else if(var=='prs'){
    return("Variable : daily mean surface pressure (kPa)")
  }
  else if(var=='RH'){
    return("Variable : daily mean 2m relative humidity (%) calculated with vapor pressure ratio (Clausius-Clapeyron relation)")
  }
  else if(var=='tmn'){
    return("Variable : daily mimimal 2m temperature (°C)")
  }
  else if(var=='tmp'){
    return("Variable : daily mean 2m temperature (°C)")
  }
  else if(var=='tmx'){
    return("Variable : daily maximal 2m temperature (°C)")
  }
  else if(var=='dtm'){
    return("Variable : daily mean 2m dewpoint temperature (°C)")
  }
  else if(var=='wnd'){
    return("Variable : daily mean 10m wind speed (m/s) - used only to compute evapotranspiration")
  }
  else if(var=='RHmin'){
    return("Variable : daily miminal 2m relative humidity (%) - used only to compute evapotranspiration")
  }
  else if(var=='RHmax'){
    return("Variable : daily maximal 2m relative humidity (%) - used only to compute evapotranspiration")
  }
  else if(var=='petPenmanMonteith'){
    return("Variable : potential evapotranspiration (mm) calculated with Penman-Monteith formulation (FAO-56 hypothetical short grass method) ")
  }
  else if(var=='petPriestleyTaylor'){
    return("Variable : potential evapotranspiration (mm) calculated with Priestley-Taylor formulation")
  }
  else if(var=='Altitude'){
    return("Variable : altitude (m) calculated from geopotential height")
  }
  else if(var=='WHC'){
    return("Variable : water holding capacity (mm)")
  }
  
  
}
