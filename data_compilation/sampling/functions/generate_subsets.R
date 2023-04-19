
# Function to generate random pres/(pseudo)abs combination

generate_subsets <- function(N, npres, nabs, nclusters, occ_data, env_data, output_dir){
  
  sub_dir <- paste0(npres, "pres_", nabs, "abs")
  work_dir <- file.path(output_dir, sub_dir)
  dir.create(work_dir, showWarnings = FALSE)
  
  for(i in 1:N){
    
    fagussylvatica_pres <- .sample_presence_by_env(occ_data, env_data, k = nclusters, nb_samples = npres)
    fagussylvatica_abs <- .sample_absence(occ_data, EUForest, ERA5land, nb_samples = nabs, env_data)
    
    species_occurrence <- .create_occurrence(fagussylvatica_pres, fagussylvatica_abs)
    
    save(species_occurrence, file=paste0(work_dir, "/occurrence_subset_", i, ".Rdata"))
    gc()
    
  }
  
}

.create_occurrence <- function(presence, absence){
  
  presence <- presence %>%
    dplyr::select(-c(nb_src, fold))
  presence$pres <- 1
  absence$pres <- 0
  
  return(rbind(presence,absence))
  
}




# Inspired by blockCV package (Valavi et al.)

# species_data : sf object containing species presence
# env_data : data.frame object of covariates to identify environmental groups
# k : number of the folds (e.g. k=10)
# nb_samples : total number of samples (e.g. nb_samples=1000)
.sample_presence_by_env <- function(species_data, env_data, k, nb_samples, debug=FALSE){
  
  # transform species data
  species_data_df <- as.data.frame(species_data, xy=T) 
  species_data_df <- na.omit(species_data_df) %>%
    dplyr::mutate(lon = round(x,1),
                  lat = round(y,1)) %>% 
    dplyr::select(-c(x,y))
  
  # extract environmental values
  env_data$lat <- round(env_data$lat,1)
  env_data$lon <- round(env_data$lon,1)
  
  ext <- left_join(species_data_df, env_data, by = c("lon", "lat"))
  if(anyNA(ext)){
    message("The input environmental dataframe does not cover all the study area !")
    if(debug){
      message("Debug = TRUE, returning missing points...")
      
      return(ext[is.na(ext$bio1), c("lon", "lat")])
    }
    ext <- inner_join(species_data_df, env_data, by = c("lon", "lat")) %>%
      dplyr::select(-c(nb_src, lon, lat))
    message(paste0("Droping ", nrow(species_data_df)-nrow(ext), " species records (out of ", nrow(species_data_df)," records)..."))
    species_data_df <- inner_join(species_data_df, env_data, by = c("lon", "lat")) %>%
      dplyr::select(c(nb_src, lon, lat))
  }
  
  if(is.null(nb_samples)){
    #return all presence points 
    return(species_data_df)
  }
  
  # variables with wider ranges of values may dominate the clusters
  # first we need to standardize them
  ext <- scale(ext)
  
  # environmental clustering
  # k-means algorithms use Euclidean distance
  kms <- stats::kmeans(ext, centers = k, iter.max = 10000000, nstart = 25, algorithm="Lloyd")
  species_data_df$fold <- kms$cluster
  
  # extract number of records per cluster to sample
  cluster_infos <- species_data_df %>% count(fold)
  cluster_infos$n_spl <- round(nb_samples * cluster_infos$n/sum(cluster_infos$n),0)
  
  # sampling
  new_species_data <- c()
  for(i in 1:k){
    n_spl <- as.numeric(cluster_infos[cluster_infos$fold==i, "n_spl"])
    species_data_k <- species_data_df[species_data_df$fold==i,]
    sample_k <- species_data_k[sample(1:nrow(species_data_k),n_spl,replace=FALSE),]
    new_species_data <- rbind(new_species_data, sample_k)
  }
  
  return(new_species_data)
  
}



# env_data is needed because ERA5 land does not cover exactly all points (coastal points)
.sample_absence <- function(species_data, EUForest, ERA5land, nb_samples, env_data){
  
  # prepare EUForest data
  EUForest_sf <- st_as_sf(EUForest, coords = c("X","Y"), crs=st_crs(3035))
  colnames(EUForest_sf)[2] <- "species"
  EUForest_rast <- st_transform(EUForest_sf, crs = st_crs(4326)) %>% 
    dplyr::select(species, geometry) %>% 
    rasterize(ERA5land, field=1, fun='count')
  EUForest_rast[EUForest_rast > 0] <- 1
  EUForest_rast[is.na(EUForest_rast)] <- 0
  
  # prepare specues_data
  species_data[species_data > 0] <- 1
  species_data[is.na(species_data)] <- 0
  
  EUForest_not_species_rast <- EUForest_rast-species_data
  EUForest_not_species <- as.data.frame(EUForest_not_species_rast, xy=T) %>%
    dplyr::mutate(lon = round(x,1),
                  lat = round(y,1)) %>% 
    dplyr::filter(layer>0) %>% 
    dplyr::select(-c(layer, x,y)) 
  
  # drop points not covered by climate data
  env_data$lat <- round(env_data$lat,1)
  env_data$lon <- round(env_data$lon,1)
  EUForest_not_species <- inner_join(EUForest_not_species, env_data, by = c("lon", "lat")) %>%
    dplyr::select(c(lon, lat))
  
  if(!is.null(nb_samples)){
    absence_data <- EUForest_not_species[sample(1:nrow(EUForest_not_species),nb_samples,replace=FALSE),]
  }else{
    absence_data <- EUForest_not_species #if no sample size given by the user, return all the absence points
  }
  
  
  return(absence_data)
  
}