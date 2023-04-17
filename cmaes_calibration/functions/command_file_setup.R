command_file_setup <- function(command_file, species_files, output_folder, climate_folder, 
                       climate_scenario, starting_year, ending_year, quiet_mode){
  
  command_lines <- c()
  command_lines[1] <- c("# File for Phenofit4 CommandScript")
  command_lines[2] <- c(paste0("# Generated with Rstudio (time: ", Sys.time(),", user: ",Sys.info()[["user"]],")"))
  command_lines[3] <- c("\t\t\t\t\t")
  command_lines[4] <- c(paste("speciesInputDir ="))
  command_lines[5] <- c(paste("climateInputDir ="))
  command_lines[6] <- c(paste("outputDir =", output_folder))
  command_lines[7] <- c(paste("quietMode =", quiet_mode))
  command_lines[8] <- c("\t\t\t\t\t")
  command_lines[9] <- c("# speciesFileName\tclimateFolderName\tclimateScenario\tstartingYear\tendingYear")
  for(i in 1:length(species_files)){
    command_lines[9+i] <- paste(species_files[i], climate_folder, 
                              climate_scenario, starting_year, ending_year, sep="\t")
  }
  
  writeLines(command_lines, command_file)
  
}
