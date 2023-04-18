# author : V. Van der Meersch

# Loading custom functions
source(paste0(wd, 'functions/compute_PET.R')) 
source(paste0(wd, 'functions/get_comments.R'))

# Package for reporting progress updates
library(progressr)
handlers(global = TRUE)
handlers("txtprogressbar")


# Needed constants
PET_constants <- data.frame(lambda = 2.45, Gsc = 0.082, sigma = 4.903e-09, alphaPT = 1.26, G = 0, z = 10)
  # lambda : latent heat of vaporisation - Gsc : solar constant - sigma : Stefan-Boltzmann constan
  # alphaPT : Priestley-Taylor coefficient - G : soil heat flux (= 0 with daily time step)
  # z : height of wind instrument in m (= 10 in ERA5-Land data)


# Function parameters :
  # - pd_folder : folder where climate data (Phenofit format) are located
  # this pd_folder needs two subfolders : "/pet_PenmanMonteith" and "/pet_PriestleyTaylor"
  #
  # - method = PenmanMonteith or PriestleyTaylor (see compute_PET function)
  #
  # - years : e.g. 1990:2000
  #
  # - ncores : for parallelisation over years
 

phenofit_compute_PET <- function(years, pd_folder, method, ncores, transform_NA_and_zero=FALSE){
  
  pd_folder_method <- paste0(pd_folder, "pet_", method, "/")
  if(transform_NA_and_zero){
    pd_folder_method <- paste0(pd_folder, "pet_", method, "_trans/")
  }
  
  plan(multisession, workers = ncores)
  
  # Get altitude
  alt_file <- paste0(pd_folder, "ERA5LAND_Altitude.fit")
  alt <- fread(alt_file, showProgress=F)
  nrows <- nrow(alt)
  
  p <- progressor(length(years))
  
  # Parallel loop in case of parallel computing
  out <- future_apply(array(1:ncores), 1, function(i){
    
    # To determine number of years processed by core in case of parallel computing
    quot <- length(years)%/%ncores
    rem <- length(years)%%ncores
    if(i<ncores){ years_i <- years[(1+(i-1)*quot):(quot*i)] 
    }else{ years_i <- years[(1+(i-1)*quot):((quot*i)+rem)] }
    
    
    for(yr in years_i){
      
      p(message = paste("Processing year", yr, "..."), class = "sticky", amount = 0)
      
      # Read variables
      tmin_file <- paste0(pd_folder, "ERA5LAND_", "tmn", "_", yr, "_dly.fit")
      tmin <- fread(tmin_file, showProgress=F)
      
      tmax_file <- paste0(pd_folder, "ERA5LAND_", "tmx", "_", yr, "_dly.fit")
      tmax <- fread(tmax_file, showProgress=F)
      
      rhmin_file <- paste0(pd_folder, "ERA5LAND_", "RHmin", "_", yr, "_dly.fit")
      rhmin <- fread(rhmin_file, showProgress=F)
      
      rhmax_file <- paste0(pd_folder, "ERA5LAND_", "RHmax", "_", yr, "_dly.fit")
      rhmax <- fread(rhmax_file, showProgress=F)
      
      rs_file <- paste0(pd_folder, "ERA5LAND_", "glo", "_", yr, "_dly.fit")
      rs <- fread(rs_file, showProgress=F)
      
      uz_file <- paste0(pd_folder, "ERA5LAND_", "wnd", "_", yr, "_dly.fit")
      uz <- fread(uz_file, showProgress=F)
      
      
      # # Temporary : problem with global radiation (259 missing points)
      # colnames(alt)[1:2] <- c("lat", "lon")
      # colnames(rs)[1:2] <- c("lat", "lon")
      # rs <- right_join(rs, alt[,1:2],  by = c("lat", "lon")) # add NA values for missing points
      
      p(message = paste("Climate files loaded !"), class = "sticky", amount = 0)
      
      # Compute PET
      alt_k <- as.numeric(unlist(alt[,3]))
      lat_rad_k = as.numeric(unlist(alt[,1]*pi/180)) # convert in radian
      tmin_k <- as.matrix(tmin[, -c(1,2)])
      tmax_k <- as.matrix(tmax[, -c(1,2)])
      rhmin_k <- as.matrix(rhmin[, -c(1,2)])
      rhmax_k <- as.matrix(rhmax[, -c(1,2)])
      rs_k <- as.matrix(rs[, -c(1,2)])
      uz_k <-as.matrix( uz[, -c(1,2)])
      J = matrix(rep(1:ncol(tmin_k), nrows), nrow=nrows, byrow = TRUE)
      
      data <- list(J = J,
                   Tmin = tmin_k, Tmax = tmax_k, 
                   RHmin = rhmin_k, RHmax = rhmax_k, 
                   Rs= rs_k, uz = uz_k)
      
      ET <- compute_PET(data, elev = alt_k, lat_rad = lat_rad_k, constants = PET_constants, alpha = 0.23, method = "PenmanMonteith")
      
      # Add lat and lon
      latlon <- as.matrix(tmin[,1:2])
      ET <- cbind(latlon, ET)
      
      # File name
      processed_file <- paste0(pd_folder_method, "ERA5LAND_pet_", yr, "_dly.fit")
      
      # Write header
      con <- file(processed_file, open="wt")
      writeLines("Climate datafile for Phenofit model", con)
      writeLines(paste("Created on RStudio, by user", Sys.getenv("USERNAME") ,", on the", Sys.Date()), con)
      comments <- get_comments(var = paste0("pet", method))
      writeLines(comments, con)
      writeLines(" ", con)
      
      if(transform_NA_and_zero){
        ET[is.na(ET) | ET < 0] <- 0
      }
      
      write.table(ET, file = con, sep = "\t",
                  row.names = FALSE, col.names= FALSE)
      
      close(con)
      
      p(message = paste("Year", yr, "completed !"), class = "sticky")
      
    }
    
  })
  
  # Free memory
  gc(verbose = FALSE)
  
  # Close background workers (in case of parallel computing)
  plan(sequential)
  
  message("Process completed !")
  
}  
  










