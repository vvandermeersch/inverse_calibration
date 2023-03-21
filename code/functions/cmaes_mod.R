# Adapted from cmaes::cma_es function by Olaf Mersmann

# Add message to a log
# Add a is_feasible option for constraints (adapted from rCMA package, by Wolfgang Konen)


cmaes_mod <- function (par, fn, ..., lower, upper, control = list(), is_feasible, log_file, trace_log = NA, trace_iter = c())
{
  norm <- function(x) drop(sqrt(crossprod(x)))
  
  controlParam <- function(name, default) {
    v <- control[[name]]
    if (is.null(v)) 
      return(default)
    else return(v)
  }
  
  ## Inital solution:
  xmean <- par
  N <- length(xmean)
  
  ## Box constraints:
  if (missing(lower)) 
    lower <- rep(-Inf, N)
  else if (length(lower) == 1) 
    lower <- rep(lower, N)
  if (missing(upper)) 
    upper <- rep(Inf, N)
  else if (length(upper) == 1) 
    upper <- rep(upper, N)
  
  ## Parameters
  trace <- controlParam("trace", FALSE)
  fnscale <- controlParam("fnscale", 1)
  stopfitness <- controlParam("stopfitness", -Inf)
  maxiter <- controlParam("maxit", 100 * N^2)
  sigma <- controlParam("sigma", 0.5)
  sc_tolx <- controlParam("stop.tolx", 1e-12 * sigma)
  keep.best <- controlParam("keep.best", TRUE)
  vectorized <- controlParam("vectorized", FALSE)
  log.all <- controlParam("diag", FALSE)
  log.sigma <- controlParam("diag.sigma", log.all)
  log.eigen <- controlParam("diag.eigen", log.all)
  log.value <- controlParam("diag.value", log.all)
  log.pop <- controlParam("diag.pop", log.all)
  
  ## Strategy parameter setting (defaults as recommended by Nicolas Hansen):
  lambda <- controlParam("lambda", 4 + floor(3 * log(N)))
  mu <- controlParam("mu", floor(lambda/2))
  weights <- controlParam("weights", log(mu + 1) - log(1:mu))
  weights <- weights/sum(weights)
  mueff <- controlParam("mueff", sum(weights)^2/sum(weights^2))
  cc <- controlParam("ccum", 4/(N + 4))
  cs <- controlParam("cs", (mueff + 2)/(N + mueff + 3))
  mucov <- controlParam("ccov.mu", mueff)
  ccov <- controlParam("ccov.1", (1/mucov) * 2/(N + 1.4)^2 + 
                         (1 - 1/mucov) * ((2 * mucov - 1)/((N + 2)^2 + 2 * mucov)))
  damps <- controlParam("damps", 1 + 2 * max(0, sqrt((mueff - 
                                                        1)/(N + 1)) - 1) + cs)
  ## Safety checks
  stopifnot(length(upper) == N)
  stopifnot(length(lower) == N)
  stopifnot(all(lower < upper))
  stopifnot(length(sigma) == 1)
  
  ## Bookkeeping variables for the best solution found so far:
  best.fit <- Inf
  best.par <- NULL
  
  # Preallocate logging structures:
  if (log.sigma) 
    sigma.log <- numeric(maxiter)
  if (log.eigen) 
    eigen.log <- matrix(0, nrow = maxiter, ncol = N)
  if (log.value) 
    value.log <- matrix(0, nrow = maxiter, ncol = mu)
  if (log.pop) 
    pop.log <- array(0, c(N, mu, maxiter))
  
  ## Initialize dynamic (internal) strategy parameters and constants
  pc <- rep(0, N)
  ps <- rep(0, N)
  B <- diag(N)
  D <- diag(N)
  BD <- B %*% D
  C <- BD %*% t(BD)
  
  chiN <- sqrt(N) * (1 - 1/(4 * N) + 1/(21 * N^2))
  
  iter <- 0L
  counteval <- 0L
  cviol <- 0L
  msg <- NULL
  nm <- names(par)
  
  ## Preallocate work arrays
  arx <- matrix(0, nrow = N, ncol = lambda)
  arfitness <- numeric(lambda)
  
  ## DP constraint count (add by V. V.)
  n_notfeasible  <- 0
  
  while (iter < maxiter) {
    
    iter <- iter + 1L
    if (!keep.best) {
      best.fit <- Inf
      best.par <- NULL
    }
    if (log.sigma) 
      sigma.log[iter] <- sigma
    
    ## Generate new population
    arz <- matrix(rnorm(N * lambda), ncol = lambda)
    arx <- xmean + sigma * (BD %*% arz)
    
    ## Death penalty (DP) constraint handling approach: if an individual is not feasible, discard it and sample a new one (add by V. Van der Meersch)
    resampleFunc <- function(i) {
      while (!is_feasible(arx[, i])) {
        arz_i <- matrix(rnorm(N * 1), ncol = 1)
        arx[, i] <<- xmean + sigma * (BD %*% arz_i)
        # it is important to use "<<-", to modify the global variable (in the while loop)
        n_notfeasible  <<- n_notfeasible + 1
        
      }
    }
    feasible = sapply(1:lambda, resampleFunc)
    
    ## Check box constraints
    vx <- ifelse(arx > lower, ifelse(arx < upper, arx, upper), 
                 lower)
    
    if (!is.null(nm)) 
      rownames(vx) <- nm
    
    pen <- 1 + colSums((arx - vx)^2)
    pen[!is.finite(pen)] <- .Machine$double.xmax/2
    cviol <- cviol + sum(pen > 1)
    
    if (vectorized) {
      y <- fn(vx, ...) * fnscale
    }
    else {
      y <- apply(vx, 2, function(x) fn(x, ...) * fnscale)
    }
    
    counteval <- counteval + lambda
    arfitness <- y * pen
    valid <- pen <= 1
    
    if (any(valid)) {
      wb <- which.min(y[valid])
      if (y[valid][wb] < best.fit) {
        best.fit <- y[valid][wb]
        best.par <- arx[, valid, drop = FALSE][, wb]
      }
    }
	
	if(!is.na(trace_log)){
		if(iter %in% trace_iter){
			trace_file <- file.path(trace_log, "trace.log")
			line <- paste("Trace for iteration", iter)
			write(line,file=trace_file,append=TRUE)
			line <- paste("best.fit =", best.fit)
			write(line,file=trace_file,append=TRUE)
			line <- paste("current fitness =", y[valid][wb])
			write(line,file=trace_file,append=TRUE)
			line <- paste("current parameters =", arx[, valid, drop = FALSE][, wb])
			saveRDS(arx[, valid, drop = FALSE][, wb], file = file.path(trace_log, paste0("parameter_trace_iter",iter,".rds")))
		}
	}
    
    arindex <- order(arfitness)
    arfitness <- arfitness[arindex]
    aripop <- arindex[1:mu]
    selx <- arx[, aripop]
    xmean <- drop(selx %*% weights)
    selz <- arz[, aripop]
    zmean <- drop(selz %*% weights)
    
    if (log.pop) 
      pop.log[, , iter] <- selx
    if (log.value) 
      value.log[iter, ] <- arfitness[aripop]
    ps <- (1 - cs) * ps + sqrt(cs * (2 - cs) * mueff) * (B %*% 
                                                           zmean)
    hsig <- drop((norm(ps)/sqrt(1 - (1 - cs)^(2 * counteval/lambda))/chiN) < 
                   (1.4 + 2/(N + 1)))
    pc <- (1 - cc) * pc + hsig * sqrt(cc * (2 - cc) * mueff) * 
      drop(BD %*% zmean)
    BDz <- BD %*% selz
    C <- (1 - ccov) * C + ccov * (1/mucov) * (pc %o% pc + 
                                                (1 - hsig) * cc * (2 - cc) * C) + ccov * (1 - 1/mucov) * 
      BDz %*% diag(weights) %*% t(BDz)
    sigma <- sigma * exp((norm(ps)/chiN - 1) * cs/damps)
    e <- eigen(C, symmetric = TRUE)
    if (log.eigen) 
      eigen.log[iter, ] <- rev(sort(e$values))
    if (!all(e$values >= sqrt(.Machine$double.eps) * abs(e$values[1]))) {
      msg <- "Covariance matrix 'C' is numerically not positive definite."
      break
    }
    B <- e$vectors
    D <- diag(sqrt(e$values), length(e$values))
    BD <- B %*% D
    if (arfitness[1] <= stopfitness * fnscale) {
      msg <- "Stop fitness reached."
      break
    }
    if (all(D < sc_tolx) && all(sigma * pc < sc_tolx)) {
      msg <- "All standard deviations smaller than tolerance."
      break
    }
    if (arfitness[1] == arfitness[min(1 + floor(lambda/2), 
                                      2 + ceiling(lambda/4))]) {
      sigma <- sigma * exp(0.2 + cs/damps)
      if (trace) 
        message("Flat fitness function. Increasing sigma.")
    }
    if (trace) 
      message(sprintf("Iteration %i of %i: current fitness %f", 
                      iter, maxiter, arfitness[1] * fnscale))
    
    # Write to log, add by V. Van der Meersch 
    if(iter==1 | iter%%5==0){
      line <- paste("Iteration", iter, "completed... Current fitness:", round(arfitness[1] * fnscale, 2))
      write(line,file=log_file,append=TRUE)
    }
    if(iter%%10==0){
      line <- paste("Best fitness so far:", round(best.fit * fnscale, 2))
      write(line,file=log_file,append=TRUE)
    }
    
  }
  
  
  
  cnt <- c(`function` = as.integer(counteval), gradient = NA)
  log <- list()
  if (log.value) 
    log$value <- value.log[1:iter, ]
  if (log.sigma) 
    log$sigma <- sigma.log[1:iter]
  if (log.eigen) 
    log$eigen <- eigen.log[1:iter, ]
  if (log.pop) 
    log$pop <- pop.log[, , 1:iter]
  names(best.fit) <- NULL
  res <- list(par = best.par, value = best.fit/fnscale, counts = cnt, 
              convergence = ifelse(iter >= maxiter, 1L, 0L), message = msg, 
              constr.violations = cviol, diagnostic = log)
  class(res) <- "cma_es.result"
  return(res)
}