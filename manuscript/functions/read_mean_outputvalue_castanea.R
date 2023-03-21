library(vroom)

read_mean_outputvalue_castanea <- function(grid, lambda, output_folder = 'D:/applications/capsis4/var', var = "BiomassOfReserves"){
  
  output <- matrix(ncol=length(grid), nrow=lambda, byrow=TRUE)
  
  for(l in 1:lambda){
    
    filenames <- sapply(grid, function(i){
      ind <- paste0("ind", l, "_")
      file.path(output_folder, paste0(format(i, scientific=F), "_", ind, "yearlyResults.log"))})
    
    yearly_results <- vroom(filenames, col_select = c(var), show_col_types = FALSE, progress=FALSE)
    yearly_results[is.na(yearly_results)] <- 0 # NA values mean tree is dead (thus biomass = 0) 
    
    # if(nrow(yearly_results) == 0){
    #   yearly_results[1:(length(grid)*30), var] <- NA
    # }
    
    nyears <- nrow(yearly_results)/length(grid)
    
    mean_results <- aggregate(yearly_results, list(rep(1:(nrow(yearly_results) %/% nyears + 1), each = nyears, len = nrow(yearly_results))), mean)[-1]
    
    output[l,] <- t(mean_results)
    
  }
  
  return(output)
}
