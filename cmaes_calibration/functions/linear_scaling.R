linearscaling <- function(x, min, max, scale_factor){
  y <- 1:length(x)
  for(i in 1:length(x)){
    y[i] <- (x[i]-min[i])/(max[i]-min[i])*10
  }
  return(y)
}

inv_linearscaling <- function(x, min, max, scale_factor){
  y <- 1:length(x)
  for(i in 1:length(x)){
    y[i] <- x[i]*(max[i]-min[i])/10+min[i]
  }
  return(y)
}
