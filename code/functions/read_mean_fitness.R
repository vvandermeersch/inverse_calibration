
# Old version, phenofit without continuous export

# read_mean_fitness <- function(lambda, output_folder, years_mean){
#   fitness <- read.table(paste0(output_folder,"-",as.character(1),"/FitnessYear.txt"), header=T)
#   fitness_mean <- matrix(ncol=lambda, nrow=nrow(fitness), byrow=TRUE)
#   for(i in 1:lambda){
#     fitness <- read.table(paste0(output_folder,"-",as.character(i),"/FitnessYear.txt"), header=T)
#     fitness_mean[,i] <- apply(fitness[, grepl(paste(years_mean, collapse = "|"), names(fitness))], 1, mean)
#   }
#   return(fitness_mean)
# }

library(data.table) #fastest
setDTthreads(1) #only one thread (in case of parallel cmaes)

read_mean_fitness <- function(lambda, output_folder){
  fitness <- fread(paste0(output_folder,as.character(1),"/Fitness.txt"), skip=3, sep="\t", fill = TRUE)
  fitness_mean <- matrix(ncol=ncol(fitness)-1, nrow=lambda, byrow=TRUE)
  
  fitness <- as.data.frame(fitness)
  fitness_mean[1,] <- apply(fitness[,-1], 2, mean)
  if(lambda>1){
    for(i in 2:lambda){
      fitness <- fread(paste0(output_folder,as.character(i),"/Fitness.txt"), skip=3, sep="\t", fill = TRUE)
      fitness <- as.data.frame(fitness)
      fitness_mean[i,] <- apply(fitness[,-1], 2, mean)
    }
  }
  
  return(fitness_mean)
}
