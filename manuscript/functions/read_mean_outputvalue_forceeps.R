library(vroom)

# v2 with meanstandexport

read_mean_outputvalue_forceeps <- function(grid, ind_paths, var = "basalAreaMean"){
  
  
  
  cnames <- c("date", "nTrees", "totalBiomass", "totalBasalArea", "totalHeight", "totalVolume", "LAI", "droughtIndexAnnual", 
              "droughtIndexSeasonal", "winterTMIN", "GDDannual", "GDDseasonal", "nTreesMean", "nTreesSD", "biomassMean", "biomassSD",
              "basalAreaMean", "basalAreaSD", "heightMean", "heightSD", "volumeMean", "volumeSD")
  
  temp <- future_lapply(1:length(ind_paths), function(l){
    
    filenames <- sapply(grid, function(i){
      ind <- paste0(format(i, scientific=F), ".sitemeanstand.txt")
      file.path(ind_paths[l], ind)})
    
    yearly_results <- vroom(filenames, show_col_types = FALSE, progress=FALSE, skip = 3, col_names=cnames) %>%
      dplyr::select(all_of(var))
    
    nyears <- nrow(yearly_results)/length(grid)
    
    mean_results <- aggregate(yearly_results, list(rep(1:(nrow(yearly_results) %/% nyears + 1), each = nyears, len = nrow(yearly_results))), mean)[-1]
    
    return(mean_results)
    
  })
  
  output <- matrix(unlist(temp), ncol=length(grid), nrow=length(ind_paths), byrow=TRUE)
  
  return(output)
}
