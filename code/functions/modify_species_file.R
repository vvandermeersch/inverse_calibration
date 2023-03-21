modify_species_file <- function(new_params, param_fixed=NULL, file){
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
  
  params <- c(Leaf, Flower, FruitMat, Senes, CpdBud, LeafHabit, Tdeath, Frost, Drought)
  
  if(is.null(param_fixed)){
    for(i in 1:length(params)){
      xml_text(params[[i]]) <- new_params[i]
    }
  }else{
    j <- 1
    for(i in which(param_fixed!="FIXED")){
      xml_text(params[[i]]) <- new_params[j]
      j <- j + 1
    }
  }
  
  write_xml(species_xml, file)
}