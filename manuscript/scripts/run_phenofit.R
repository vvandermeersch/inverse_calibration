library(gtools)

# Run Phenofit for every calibrated parameter sets
wd <- 'C:/Users/vandermeersch/Documents/CEFE/phd/present_calibration/calibration_paper'
source(file.path(wd, "functions", "command_file_setup.R"))

cal_folder <- 'D:/calibrations/phenofit/fagus_sylvatica/1000pres_1000abs/paper_data/ABC'
sim_folder <- 'D:/simulations/phenofit/backward/fagus_sylvatica/paper_data/ABC'

species_files <- mixedsort(list.files(path = cal_folder, pattern = "\\.species$", full.names = F, recursive = T))

climate_folder <- "D:/climate/ERA5-Land/phenofit_format/transformed"
capsis_settings=list(java8="java8.cmd", cd_java8="cd C:/Program Files/Java/scripts", cd_capsis="cd/d D:/applications/capsis4 && setmem 30000")

for(cal in species_files){
  subset <- strsplit(cal, "_")[[1]][4]
  rep <- substr(strsplit(cal, "_")[[1]][5], 1, nchar(strsplit(cal, "_")[[1]][5])-8)
  output_folder <- file.path(sim_folder, paste0(subset, "_", rep))
  print(paste0("Repetition ", rep, " of subset ", subset))
  
  species_file <- file.path(cal_folder, cal)
  command_file <- file.path(sim_folder, "command_file.txt")
  
  command_file_setup(command_file, species_file, output_folder, climate_folder,
                     climate_scenario="ERA5LAND", starting_year="1970", ending_year="2000", quiet_mode="true")
  
  run_capsis <- paste("capsis -p script phenofit4.myscripts.ScriptVictor", command_file)
  run <- paste(capsis_settings$cd_java8, capsis_settings$java8, capsis_settings$cd_capsis, run_capsis, sep=' && ')
  
  system.time(shell(run, intern=F))
}

cal_folder <- 'D:/calibrations/phenofit/fagus_sylvatica/1000pres_1000abs/paper_data/ABC'
sim_folder <- 'D:/simulations/phenofit/backward/fagus_sylvatica/paper_data/ABC'

species_files <- mixedsort(list.files(path = cal_folder, pattern = "\\.species$", full.names = F, recursive = T))

