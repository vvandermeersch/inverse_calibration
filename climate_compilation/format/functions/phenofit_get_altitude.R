# author : V. Van der Meersch

source(paste0(wd, 'functions/get_comments.R'))
source(paste0(wd, 'functions/geopotential_to_geometric.R'))

# loc : data.frame containing longitude and latitude for which altitude is needed
# if NULL, compute for every point !

phenofit_get_altitude <- function(loc = NULL, rd_folder, pd_folder){

  # Open geopotential file
  geopotential_file <- paste0(rd_folder, "geo_1279l4_0.1x0.1.grib2_v4_unpack.nc")
  
  geopotential_raster <- brick(geopotential_file, varname="z")
  
  geopotential_df <- as.data.frame(geopotential_raster, xy=TRUE)
  names(geopotential_df) <- c("lon", "lat", "z")
  
  # Need to convert longitude from (0-360) to (-180, 180)
  geopotential_df$lon <- ((geopotential_df$lon+180)%%360)-180.0
  
  # Convert to geometric height
  geopotential_df$z <- geopotential_to_geometric(geopotential_df$z)
  
  if(is.null(loc)){
    alt_needed <- geopotential_df
  }else{
    alt_needed <- left_join(round(loc,2),round(geopotential_df,2),by=c("lat","lon"))  # round is important, of left_join won't work
  }
  
  alt_needed$lon <- loc$lon
  alt_needed$lat <- loc$lat
  
  # Fill name
  processed_file <- paste0(pd_folder, "ERA5LAND_Altitude.fit")
  
  # Fill file
  con <- file(processed_file, open="wt")
  writeLines("Elevation datafile for Phenofit model", con)
  writeLines(paste("Created on RStudio, by user", Sys.getenv("USERNAME") ,", on the", Sys.Date()), con)
  comments <- get_comments(var = "Altitude")
  writeLines(comments, con)
  writeLines(" ", con)
  write.table(alt_needed, file = con, sep = "\t",
              row.names = FALSE, col.names= FALSE)
  close(con)
  
  # Free memory
  gc(verbose = FALSE)

}
