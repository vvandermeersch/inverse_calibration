suppressPackageStartupMessages(library(AUC))
source(paste0(wd,"/functions/linear_scaling.R"))
source(paste0(wd, "/functions/read_mean_fitness.R"))

auc_test <- function(x, Yobs, run, run_all, wd, species_files, lambda, param){
  
  # Inverse transformation
  # param_invs <- matrix(ncol=ncol(param), nrow=nrow(param), byrow=TRUE)
  # for(i in 1:ncol(param)){
  #   param_invs[,i] <- inv_linearscaling(param[,i], domain_lb, domain_ub)
  # }
  
  
  param_f <- which(param$param_fixed!="FIXED")
  
  # If mixt bud
  if(parameters$param_init["cpdbud"]=="2"){
    flower_param <- grepl("flower", names(param_f)) # get indices
    flower_param[max(which(flower_param))] <- FALSE # exclude last parameter of flower model (Fcrit)
    
    param_f <- param_f[!flower_param] # new fixed parameter indice list
  }
  
  param_lb <- as.numeric(param$param_lb[param_f])
  param_ub <- as.numeric(param$param_ub[param_f])
  
  param_f <- which(param$param_fixed!="FIXED") # inital fixed parameter indice list
  
  for(i in 1:lambda){
    x_inv <- inv_linearscaling(x[,i], param_lb, param_ub, 
                               param$scale_factor)
    
    
    # If mixt bud
    if(parameters$param_init["cpdbud"]=="2"){
      len <- length(x_inv) #number of parameters found by cmaes algorithm 
      nb <- length(which(flower_param)) #number of parameters to copy from leaf model to flower model
      x_inv <- c(x_inv[1:(nb+1)], x_inv[1:nb], x_inv[(nb+2):len]) # new parameter list to modify
      
    }
    
    
    modify_species_file(as.character(x_inv), param$param_fixed, species_files[i])
  }
  
  
  # Run Capsis simulations
  temp <- future_lapply(run_all, function(x){
    out <- system(x, ignore.stdout = TRUE, ignore.stderr = TRUE)
  })
  #shell(run_all)
  
  # eval(parse(text=paste0("it <- it_run", run)))
  # line <- paste("Iteration", it, "of run", run, "completed\n")
  # write(line,file="/scratch/vvandermeersch/inverse_modelling_cluster/logs/CMAES_log.txt",append=TRUE)
  
  # p(message = paste("Iteration", it, "of run", run, "completed"), "sticky", amount = 0)
  # eval(parse(text=paste0("it_run", run, "<<-", "it_run", run,"+1")))
  # p()
  
  
  # Read fitness and mean between two years
  Ysim <- c()
  for(i in 1:length(run_all)){
    output_folder <- paste0(folders$wd,"/process_files/output/core",i,"/run")
    Ysim <- rbind(Ysim, read_mean_fitness(lambda/length(run_all), output_folder))
    
  }
  
  # Compute AUC
  obs <- as.factor(Yobs$pres)
  auc_pred <- apply(Ysim, 1, FUN= function(x){
    roc_pred <- roc(x, obs)
    auc(roc_pred)
  })
  
  return(1-auc_pred)
}
