
# Function to compute bioclim variables from PHENOFIT daily variables

compute_biovars2 <- function(years, pd_folder, bio_folder, ncores){
  
  # initialise progressor
  p <- progressor(length(years)*5)
  
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
      
      # Load data 
      tmin_file <- paste0(pd_folder, "ERA5LAND_", "tmn", "_", yr, "_dly.fit")
      tmin <- fread(tmin_file, showProgress=F)
      nbdays <- ncol(tmin) - 2
      
      tmax_file <- paste0(pd_folder, "ERA5LAND_", "tmx", "_", yr, "_dly.fit")
      tmax <- fread(tmax_file, showProgress=F)
      
      pre_file <- paste0(pd_folder, "ERA5LAND_", "pre", "_", yr, "_dly.fit")
      pre <- fread(pre_file, showProgress=F)
      
      p(message = paste("Climate files loaded !"), class = "sticky")
      
      # Monthly means
      p(message = paste("Computing monthly values for year", yr, "..."), class = "sticky", amount = 0)
      tmin_mn <- monthly_calc(tmin, nbdays, stat = "mean")
      p()
      tmax_mn <- monthly_calc(tmax, nbdays, stat = "mean")
      p()
      pre_mn <- monthly_calc(pre, nbdays, stat = "sum")
      p()
      
      biovars <- matrix(nrow = nrow(tmin_mn), ncol = 21) # lat + lon + 19 biovars
      biovars[,1:2] <- as.matrix(tmin[,1:2])
      p(message = paste("Computing bioclim variables for year", yr, "..."), class = "sticky", amount = 0)
      biovars_values <- dismo::biovars(pre_mn, tmin_mn, tmax_mn)
      biovars[,3:21] <- as.matrix(biovars_values)
      colnames(biovars) <- c("lat", "lon", colnames(biovars_values))
      biovars <- as.data.frame(biovars)
      save_file <- paste0(bio_folder, "biovars_", yr,".Rdata")
      save(biovars, file = save_file)
      
      p(message = paste("Year", yr, "completed !"), class = "sticky")
      
    }
    
  }, future.seed = TRUE)
  
  # Free memory
  gc(verbose = FALSE)
  
  # Close background workers (in case of parallel computing)
  plan(sequential)
  
}



.
monthly_calc <- function(climate_data, nbdays, stat = "mean"){
  
  # Leap year condition
  if(nbdays == 365){
    nbdays_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  }else if(nbdays == 366){
    nbdays_month <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  }
  id_month <- t(as.matrix(rep(1:12, nbdays_month)))
  
  
  clim_data_mn <- as.data.frame(t(rbind(id_month,climate_data[,-c(1,2)], use.names=FALSE)))
  colnames(clim_data_mn)[1] <- "month"
  clim_data_mn <- clim_data_mn %>% 
    group_by(month) %>%
    summarise(across( c(1:nrow(climate_data)), get(stat)))
  clim_data_mn <- t(clim_data_mn[,-1]) # drop months
  
  return(clim_data_mn)
  
}
