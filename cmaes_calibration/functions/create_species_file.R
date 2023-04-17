library("xml2")

create_species_file <- function(file, structure_file){
  species_structure_xml <- read_xml(structure_file)
  write_xml(species_structure_xml, file)
}