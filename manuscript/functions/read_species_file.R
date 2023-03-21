library("xml2")

read_species_file <- function(file){
  species_xml <- read_xml(file)
  species <- xml_children(xml_children(species_xml)[[1]])
  
  
  Leaf <- xml_children(xml_find_all(species, "//Leaf"))
  Flower <- xml_children(xml_find_all(species, "//Flower"))
  FruitMat <- xml_children(xml_find_all(species, "//FruitMaturation"))
  Senes <- xml_children(xml_find_all(species, "//Senescence"))
  CpdBud <- (xml_find_all(species, "//CompoundBud"))
  LeafHabit <- (xml_find_all(species, "//LeafHabit"))
  Tdeath <- (xml_find_all(species, "//Tdeath"))
  Frost <- xml_children(xml_find_all(species, "//Frost"))
  Drought <- xml_children(xml_find_all(species, "//Drought"))
  
  params <- c(leaf = xml_text(Leaf), flower = xml_text(Flower), fruitmat = xml_text(FruitMat), senes = xml_text(Senes), 
              cpdbud = xml_text(CpdBud), leafhabit = xml_text(LeafHabit), tdeath= xml_text(Tdeath), frost = xml_text(Frost), drought = xml_text(Drought))
  
  return(params)
}