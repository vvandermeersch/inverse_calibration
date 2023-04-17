source(paste0(wd, 'functions/get_var_name.R'))
source(paste0(wd, 'functions/get_comments.R'))
source(paste0(wd, 'functions/convert_units.R'))

# author : V. Van der Meersch

# Package to calculate relative humidity with the ratio of saturation vapor pressure
library(humidity)

# Package for reporting progress updates
library(progressr)
handlers(global = TRUE)
handlers("txtprogressbar")

phenofit_processing_and_formatting <- function(years, var, stat, rd_folder, pd_folder, ncores=1){
  
  if(var %in% c("2m_temperature", "total_precipitation", "surface_solar_radiation_downwards")){
    stop(paste0("For ", var, ", you need to use phenofit_formatting function !"))
  }
  else if( var != "wind" & var != "relative_humidity" ){
    stop("Cannot recognize the variable you asked for !")
  }
  
  # To follow the progress
  p <- progressor(length(years)*12)
  
  months <- c("01", "02", "03", "04", "05", "06",
              "07", "08", "09", "10", "11", "12")
  
  # Get variable names as coded in NC file
  if(var == "wind"){
    var_1 <- "10m_u_component_of_wind"
    var_name_1 <- get_var_name(var_1, type = "ERA5")
    var_2 <- "10m_v_component_of_wind"
    var_name_2 <- get_var_name(var_2, type = "ERA5")
  }
  if(var == "relative_humidity"){
    var_1 <- "2m_temperature"
    var_name_1 <- get_var_name(var_1, type = "ERA5")
    var_2 <- "2m_dewpoint_temperature"
    var_name_2 <- get_var_name(var_2, type = "ERA5")
  }
  
  
  plan(multisession, workers = ncores)
  
  # Parallel loop in case of parallel computing
  out <- future_apply(array(1:ncores), 1, function(i){
    
    # To determine number of years processed by core in case of parallel computing
    quot <- length(years)%/%ncores
    rem <- length(years)%%ncores
    if(i<ncores){ years_i <- years[(1+(i-1)*quot):(quot*i)] }
    else{ years_i <- years[(1+(i-1)*quot):((quot*i)+rem)] }
    
    
    # Loop on years
    for(yr in years_i){
      
      p(message = paste("Processing year", yr, "..."), class = "sticky", amount = 0)
      
      init <- TRUE
      
      # Loop on months
      for(mn in months){
        print(mn)
        
        # Names of the files
        file_1 <- paste0(var_1, "_", yr, "_", mn, ".nc")
        file_2 <- paste0(var_2, "_", yr, "_", mn, ".nc")
        
        # Load NC files
        raster_nc_1 <- brick(paste0(rd_folder, file_1), varname=var_name_1)
        raster_nc_2 <- brick(paste0(rd_folder, file_2), varname= var_name_2)
        
        # Get time
        ntime <- dim(raster_nc_1)[3]
        
        if(yr == 1950 & mn == "01"){ntime <- ntime + 1} # particular case, very first hour of ERA5-land data is missing
        
        # Loop on days
        for(day in 1:(ntime/24)){
          
          start <- 1+(day-1)*24
          end <- 24*day
          
          # particular case, very first hour of ERA5-land data is missing (January 1950)
          if(yr == 1950 & mn == "01" & day == 1){end <- end - 1} else if(yr == 1950 & mn == "01"){
            start <- start - 1 
            end <- end - 1
          }
          
          if(var == "wind"){
            
            hourly_value_1 <- subset(raster_nc_1, start:end)
            hourly_value_2 <- subset(raster_nc_2, start:end)
            
            # Calculate wind speed from u and v components
            values(hourly_value_1) <- sqrt(values(hourly_value_1)^2 + values(hourly_value_2)^2)
             
            NAvalue(hourly_value_1) <- -9999
            daily_value <- calc(hourly_value_1, fun = eval(parse(text=stat)), na.rm = T)
            
            # Fill dataframe (no conversion needed, already in m/s)
            if(init){
              # Create dataframe
              phenofit_format_df <- as.data.frame(daily_value, xy=TRUE)
              # Inverse lon and lat
              phenofit_format_df <- phenofit_format_df[,c(2,1,3)]
              init <- FALSE
            }else{
              daily_value_df <- as.data.frame(daily_value, xy=FALSE)
              phenofit_format_df <- cbind(phenofit_format_df, daily_value_df)
            }
          }
          
          
          if(var == "relative_humidity"){
            # Calculate relative humidity from temperature and dewpoint temperature
            
            hourly_value_1 <- subset(raster_nc_1, start:end)
            hourly_value_2 <- subset(raster_nc_2, start:end)
            
            # To calculate relative humidity with the ratio of saturation vapor pressure (Clausius-Clapeyron equation)
            hourly_value <- humidity::RH(values(hourly_value_1), values(hourly_value_2), isK=TRUE)
            hourly_value[which(hourly_value>100)] <- 100
            values(hourly_value_1) <- hourly_value
            
            NAvalue(hourly_value_1) <- -9999
            daily_value <- calc(hourly_value_1, fun = eval(parse(text=stat)), na.rm = T)
            
            # Fill dataframe
            if(init){
              # Create dataframe
              phenofit_format_df <- as.data.frame(daily_value, xy=TRUE)
              # Inverse lon and lat
              phenofit_format_df <- phenofit_format_df[,c(2,1,3)]
              init <- FALSE
            }else{
              daily_value_df <- as.data.frame(daily_value, xy=FALSE)
              phenofit_format_df <- cbind(phenofit_format_df, daily_value_df)
            }
          }
          
        }
        
        # Update progression
        p()
        
      }
      
      # Remove NA values (not land ?)
      phenofit_format_df <- na.omit(phenofit_format_df)
      
      # Get variable name as needed by Phenofit
      p_var <- get_var_name(var, stat = stat, type = "PHENOFIT")
      
      # File name
      processed_file <- paste0(pd_folder, "ERA5LAND_", p_var, "_", yr, "_dly.fit")
      
      # Fill file
      con <- file(processed_file, open="wt")
      writeLines("Climate datafile for Phenofit model", con)
      writeLines(paste("Created on RStudio, by user", Sys.getenv("USERNAME") ,", on the", Sys.Date()), con)
      comments <- get_comments(var = p_var)
      writeLines(comments, con)
      writeLines(" ", con)
      write.table(phenofit_format_df, file = con, sep = "\t",
                  row.names = FALSE, col.names= FALSE)
      close(con)
      
    }
    
  })
  
  # Free memory
  gc(verbose = FALSE)
  
  # Close background workers (in case of parallel computing)
  plan(sequential)
  
  message("Process completed !")
  
}
