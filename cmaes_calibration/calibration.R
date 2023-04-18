# Genouest cluster adaptation

# Working directory
wd <- "/scratch/vvandermeersch/phenofit_calibration"

source(paste0(wd, "/functions/cmaes_calibration.R"))
source(paste0(wd, "/functions/auc_test.R"))
source(paste0(wd, "/functions/read_species_file.R"))
source("functions/linear_scaling.R")


################
#### SET-UP ####
################

# Parameter inital values and bounds
param_init <- read_species_file(paste0(wd, "/input_files/pinus_sylvestris/Pinus_sylvestris_init.species"))
param_fixed <- read_species_file(paste0(wd, "/input_files/pinus_sylvestris/Pinus_sylvestris_lb.species"))
param_lb <- read_species_file(paste0(wd, "/input_files/pinus_sylvestris/Pinus_sylvestris_lb.species"))
param_ub <- read_species_file(paste0(wd, "/input_files/pinus_sylvestris/Pinus_sylvestris_ub.species"))
scale_factor <- 10
parameters=parameters=list(param_init=param_init, param_fixed=param_fixed, 
                           param_lb=param_lb, param_ub=param_ub, scale_factor=scale_factor)

# Constraints
constraint_function <- function(x){
  x[6] <= x[7] # Leaf Fcrit <= Fflower Fcrit for F. sylvatica
}

# CMA-ES settings
cmaes_controls=list(sigma=2, mu=10, lambda=20, maxit=300)
cmaes_settings=list(rand_init=FALSE, ipop=FALSE, ipop_factor=NULL)

# Parallelization settings
parallel_settings=list(ncores_runs=1, ncores_eval=20)

# Capsis settings
structure_file <- paste0(wd, "/input_files/Fagus_sylvatica_JGauzere.species")
species=list(structure=structure_file, file="fagus_sylvatica")
folders=list(wd=wd, 
             climate="/scratch/vvandermeersch/data/climate/phenofit_format/ERA5Land_fagsyl/1000pres_1000abs/subset_1")
simulation=list(climate_scenario="ERA5LAND", starting_year="1970", ending_year="2000", quiet_mode="true")
capsis_settings=list(java8=NA, cd_java8=NA, cd_capsis="cd /home/genouest/mnhn_cesco/vvandermeersch/capsis4_3")



################
#### CMA-ES #### 
################

# Occurrence data for calibration
load("/scratch/vvandermeersch/data/species/fagsyl/1000pres_1000abs/occurrence_subset_1.Rdata")

# Run calibration
cmaes_fit <- cmaes_calibration(nruns=1, obj_function = auc_test, Yob = species_occurrence,
                      parameters=parameters,
                      is_feasible = constraint_function,
                      controls=cmaes_controls,
                      parallel=parallel_settings,
                      cmaes_settings=cmaes_settings,
                      species=species,
                      folders=folders,
                      simulation=simulation,
                      capsis_settings=capsis_settings)

save(cmaes_fit, file=paste0(wd, "/output_files/cmaes_output_", Sys.Date(), ".Rdata"))





