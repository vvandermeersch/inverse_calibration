
# Function adapted by V. Van der Meersch from Evapotranspiration package (created by Danlu Guo)
# Added a modification to correct polar night effect...


# method = PenmanMonteith or PriestleyTaylor
# physically based PenmanMonteith (PM) approach is presently considered as the state-of-the art 
# PriestleyTaylor (PT) data requirement does not include some hardly available meteorological variables as wind speed or relative humidity
# PT could fail in mimicking PM values under dry conditions or in zones where wind speed is relatively high


# elev : ground elevation above mean sea level in m
# latitude in radians


compute_PET <- function(data, elev, lat_rad, constants, alpha = 0.23, method){
  
  if(method == "PriestleyTaylor"){
    
    Ta <- (data$Tmax + data$Tmin)/2
    
    # Saturated vapour pressure (August–Roche–Magnus approximation)
    vs_Tmax <- 0.6108 * exp(17.27 * data$Tmax/(data$Tmax + 237.3))
    vs_Tmin <- 0.6108 * exp(17.27 * data$Tmin/(data$Tmin + 237.3))
    vas <- (vs_Tmax + vs_Tmin)/2
    
    # Vapour pressure
    vabar <- (vs_Tmin * data$RHmax/100 + vs_Tmax * data$RHmin/100)/2 
    
    P <- 101.3 * ((293 - 0.0065 * elev)/293)^5.26 # atmospheric pressure
    # effect of atmospheric pressure (i.e. Eevaporation at high altitudes promoted due to low atmospheric pressure) is small
    # thus, the average value of atmospheric pressure for a location is sufficient
    
    delta <- 4098 * (0.6108 * exp((17.27 * Ta)/(Ta + 237.3)))/((Ta + 237.3)^2) # slope of vapour pressure curve
    gamma <- 0.00163 * P/constants$lambda # psychrometric constant
    d_r2 <- 1 + 0.033 * cos(2 * pi/365 * data$J) # dr is the inverse relative distance Earth-Sun
    delta2 <- 0.409 * sin(2 * pi/365 * data$J - 1.39) # solar dedication
    
    # Added by Victor
    aux <- -tan(lat_rad) * tan(delta2) 
    aux[aux > 1] <- 1
    aux[aux < -1] <- -1
    
    w_s <- acos(aux) # sunset hour angle
    N <- 24/pi * w_s
    R_a <- (1440/pi) * d_r2 * constants$Gsc * (w_s * sin(lat_rad) * 
                                                 sin(delta2) + cos(lat_rad) * cos(delta2) * 
                                                 sin(w_s))
    R_so <- (0.75 + (2 * 10^-5) * elev) * R_a
    R_s <- data$Rs
    R_nl <- constants$sigma * (0.34 - 0.14 * sqrt(vabar)) * 
      ((data$Tmax + 273.2)^4 + (data$Tmin + 273.2)^4)/2 * 
      (1.35 * R_s/R_so - 0.35) # estimated net outgoing longwave radiation
    R_nsg <- (1 - alpha) * R_s # net incoming shortwave radiation - water or other evaporative surface with specified Albedo
    R_ng <- R_nsg - R_nl
    E_PT  <- constants$alphaPT * (delta/(delta + gamma) * 
                                         R_ng/constants$lambda - constants$G/constants$lambda)
    ET  <- E_PT 
    
  }
  else if(method == "PenmanMonteith"){
    
    z0 <- 0.02 # short crop, i.e. method for FAO-56 hypothetical short grass
    
    Ta <- (data$Tmax + data$Tmin)/2
    
    # Saturated vapour pressure
    vs_Tmax <- 0.6108 * exp(17.27 * data$Tmax/(data$Tmax + 237.3))
    vs_Tmin <- 0.6108 * exp(17.27 * data$Tmin/(data$Tmin + 237.3))
    vas <- (vs_Tmax + vs_Tmin)/2
    
    # Vapour pressure
    vabar <- (vs_Tmin * data$RHmax/100 + vs_Tmax * data$RHmin/100)/2 
    
    P <- 101.3 * ((293 - 0.0065 * elev)/293)^5.26 # atmospheric pressure
    # effect of atmospheric pressure (i.e. evaporation at high altitudes promoted due to low atmospheric pressure) is small
    # thus, the average value of atmospheric pressure for a location is sufficient
    
    delta <- 4098 * (0.6108 * exp((17.27 * Ta)/(Ta + 237.3)))/((Ta + 237.3)^2) # slope of vapour pressure curve
    gamma <- 0.00163 * P/constants$lambda # psychrometric constant
    d_r2 <- 1 + 0.033 * cos(2 * pi/365 * data$J) # dr is the inverse relative distance Earth-Sun
    delta2 <- 0.409 * sin(2 * pi/365 * data$J - 1.39) # solar dedication
    
    # Added by Victor
    aux <- -tan(lat_rad) * tan(delta2) 
    aux[aux > 1] <- 1
    aux[aux < -1] <- -1
    
    w_s <- acos(aux) # sunset hour angle
    N <- 24/pi * w_s
    R_a <- (1440/pi) * d_r2 * constants$Gsc * (w_s * sin(lat_rad) * 
                                                 sin(delta2) + cos(lat_rad) * cos(delta2) * 
                                                 sin(w_s))
    R_so <- (0.75 + (2 * 10^-5) * elev) * R_a
    
    R_s <- data$Rs
    
    R_nl <- constants$sigma * (0.34 - 0.14 * sqrt(vabar)) * 
      ((data$Tmax + 273.2)^4 + (data$Tmin + 273.2)^4)/2 * 
      (1.35 * R_s/R_so - 0.35) # estimated net outgoing longwave radiation
    R_nsg <- (1 - alpha) * R_s # net incoming shortwave radiation - water or other evaporative surface with specified Albedo
    R_ng <- R_nsg - R_nl
    
    u2 <- data$uz * 4.87/log(67.8 * constants$z - 5.42)

    ET_RC  <- (0.408 * delta * (R_ng - constants$G) + 
                      gamma * 900 * u2 * (vas - vabar)/(Ta + 273))/(delta + 
                                                                      gamma * (1 + 0.34 * u2))
    
    ET  <- ET_RC 
    
  }
  
  return(unlist(ET))

}




