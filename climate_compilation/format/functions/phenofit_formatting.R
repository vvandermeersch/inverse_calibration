source(paste0(wd, 'functions/get_var_name.R'))
source(paste0(wd, 'functions/get_comments.R'))
source(paste0(wd, 'functions/convert_units.R'))

# author : V. Van der Meersch

# Package for reporting progress updates
library(progressr)
handlers(global = TRUE)
handlers("txtprogressbar")

phenofit_formatting2 <- function(years, var, stat, rd_folder, pd_folder, ncores=1, landsea_mask=FALSE){
  
  if(landsea_mask){
    landsea_file <- paste0(rd_folder, 'lsm_1279l4_0.1x0.1.grb_v4_unpack.nc')
    landsea_mask <- brick(landsea_file, varname='lsm')
    landsea_mask <- rotate(landsea_mask) # longitude : go from 0 - 360 to -180 - 180
    e <- extent(-14, 40, 34,72)
    landsea_mask <- crop(landsea_mask, e)
    landsea_mask[landsea_mask<0.1] <- NA
    landsea_mask_df <- na.omit(as.data.frame(landsea_mask, xy=TRUE))
    names(landsea_mask_df) <- c("lon", "lat", "mask")
    landsea_mask_df <- round(landsea_mask_df, 2)
  }
  
  if(var == "wind" | var == "relative_humidity"){
    stop("For wind or relative humidity, you need to use phenofit_processing_and_formatting function !")
  }
  else if( !(var %in% c("2m_temperature", "total_precipitation", "surface_solar_radiation_downwards", 
                        "potential_evaporation", "2m_dewpoint_temperature")) ){
    stop("Cannot recognize the variable you asked for !")
  }
  
  # To follow the progress
  p <- progressor(length(years)*12)
  
  # Get variable name as coded in NC file
  var_name <- get_var_name(var, type = "ERA5")
  
  months <- c("01", "02", "03", "04", "05", "06",
               "07", "08", "09", "10", "11", "12")
  
  plan(multisession, workers = ncores)
  
  # Parallel loop (in case of parallel computing)
  out <- future_apply(array(1:ncores), 1, function(i){
    
    # To determine number of years processed by core (in case of parallel computing)
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
        
        # Name of the file
        file <- paste0(var, "_", yr, "_", mn, ".nc")
        
        # Load NC file as raster
        raster_nc <- brick(paste0(rd_folder, file), varname=var_name)
        
        # Get time
        ntime <- dim(raster_nc)[3]
        
        if(var %in% c("total_precipitation", "potential_evaporation", "surface_solar_radiation_downwards")){
          
          if(yr == 1950 & mn == "01"){ntime <- ntime + 1} # particular case, very first day of ERA5-land data is missing
          
          
          # Loop on days
          for(day in 1:(ntime)){
              
            # ERA5-Land : accumulated from the beginning of the forecast to the end of the forecast step
            # i.e. daily value of day D is the step 24 value = 00h00 of the day D+1
            if(day == ntime){
              # for the last day of the month...
              if(mn == "12"){
                
                file <- paste0(var, "_", yr+1, "_", "01", ".nc") # next year if december...
              }else{
                file <- paste0(var, "_", yr, "_", months[which(months==mn)+1], ".nc")
              }
              raster_nc <- brick(paste0(rd_folder, file), varname=var_name)
              daily_value <- subset(raster_nc, 1) 
            }else{
              if(yr == 1950 & mn == "01"){ # particular case, very first day of ERA5-land data is missing
                daily_value <- subset(raster_nc, day)
              }else{
                daily_value <- subset(raster_nc, day+1)
              }
              
            }
            
            # Convert units (e.g. Kelvin to Celsius)
            values(daily_value) <- convert_units(daily_value, var)
            
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
        
        }else{
          
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
            
            hourly_value <- subset(raster_nc, start:end)
            
            NAvalue(hourly_value) <- -Inf
            daily_value <- calc(hourly_value, fun = eval(parse(text=stat)))
            
            # Convert units (e.g. Kelvin to Celsius)
            values(daily_value) <- convert_units(daily_value, var)
            
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
      
      # Filter with landsea_mask
      names(phenofit_format_df) <- c("lat", "lon", as.character(1:(length(phenofit_format_df)-2)))
      phenofit_format_df$lat <- round(phenofit_format_df$lat, 2)
      phenofit_format_df$lon <- round(phenofit_format_df$lon, 2)
      if(landsea_mask){
        phenofit_format_df <- inner_join(phenofit_format_df, landsea_mask_df, by = c("lon", "lat"))
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
    
  }, future.seed = TRUE)
  
  # Free memory
  gc(verbose = FALSE)
  
  # Close background workers (in case of parallel computing)
  plan(sequential)
  
  message("Process completed !")
  
}
