source(paste0(wd, "/functions/create_species_file.R"))
source(paste0(wd, "/functions/modify_species_file.R"))

create_init_species_file <- function(name, params_init, structure_file, lambda){
  species_files <- c()
  for(i in 1:lambda){
    species_files[i] <- paste0(wd, name, "_", as.character(i), ".species")
    create_species_file(species_files[i], structure_file)
    modify_species_file(params_init, param_fixed=NULL, species_files[i])
  }
  return(species_files)
}

