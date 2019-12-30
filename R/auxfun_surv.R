#globalVariables(c("res", "K", "cluster") )

#function for fitting an exponential model
survfitpar <- function(data, xbin){
  thetahat <- sum(data$status)/sum(data$ttilde)
  return(exp(-thetahat*xbin))
}




# function for obtaining the value of the last position
last <- function(x){
  return(x[length(x)])
}



# function that joints the estimations of the km and the parametric model
# (when there is not nonparametric estimations at each kbin)
joint_km_np <- function(aux, h1_param, j, kbin){
  km <- aux$surv[aux$strata == paste("ff=", j, sep="")]
  if (length(km) == kbin){
    res <- km
  }else{
    res <- c(km, h1_param[(length(km) + 1):kbin, j])
    ii <- c(rep(FALSE, length(km)), last(km) < h1_param[(length(km) + 1):kbin, j])
    res[ii] <- last(km)
  }
  return(res)
}




# function for obtaining estimates for each curve under h0 and under h1
# (for using in the test estatistics) in a GRID
chat_grid <- function(h1, h0, data, res, kbin, j){
  grid <- seq(from = min(data$ttilde[data$ff == j]),
              to = max(data$ttilde[data$ff == j]), length.out = kbin)
  aux1 <- summary(h1, times = grid, extend = TRUE)
  surv1 <- aux1$surv[aux1$strata == paste("ff=", j, sep = "")]
  aux0 <- summary(h0, times = grid, extend = TRUE)
  if (length(unique(res$cluster)) == 1) {
    surv0 <- aux0$surv
  }else{
    surv0 <- aux0$surv[aux0$strata == paste("res$cluster[data$ff]=",
                                            res$cluster[j], sep = "")] #estimaciones del cluster que pertenece)
  }
  f <- rep(j, length(surv0))
  ress <- data.frame(surv1, surv0, f)
  return(ress)
}


#innerf <- function(x, foo) assign(paste(x), foo , envir = clustcurv)


# CAMBIAR!!!!!!

# test statistic (valid for kmeans or kmedians algorithm, fill in method argument)
Tvalue <- function(data, K, kbin, method){
  h1 <- survfit(Surv(ttilde, status) ~ ff, data = data)
  xbin <- seq(from = min(data$ttilde), to = max(data$ttilde), length.out = kbin)   # !OJO cambiar esto para que el último tiempo nunca sea censurado!!! que el max sea el max ttilde no censurado!!!
  aux <- summary(h1, times = xbin)
  kbin_km <- as.numeric(table(aux$strata))

  if (sum(kbin_km != kbin) > 0) { # if TRUE, there is some curve without estimates at each kbin
    h1_param <- do.call("cbind", as.list(by(data, data$ff, survfitpar, xbin)))
    muhat <- sapply(1:length(unique(data$ff)),
                    function(x){joint_km_np(aux, h1_param, x, kbin)})
  }else{
    muhat <- matrix(aux$surv, ncol = nlevels(as.factor(data$ff)), nrow = kbin)
  }



  if(method == "kmeans"){
    #res <- NULL
    #rebind("res", kmeans(t(muhat), centers = K, iter.max = 50, nstart = 500))
    #res <<- kmeans(t(muhat), centers = K, iter.max = 50, nstart = 500) #<<-
    #innerf("res", kmeans(t(muhat), centers = K, iter.max = 50, nstart = 500))
    res <- kmeans(t(muhat), centers = K, iter.max = 50, nstart = 500)
  }
  if(method == "kmedians"){
    if (K == 1){
      #res <<- list()
      #res$cluster <<- rep(1, length(unique(data$ff)))
      res <- list()
      res$cluster <- rep(1, length(unique(data$ff)))
      #rebind("res$cluster", rep(1, length(unique(data$ff))))
      #innerf("res$cluster", rep(1, length(unique(data$ff))) )

    } else {
      #res <<- kGmedian(t(muhat), ncenters = K, nstart = 50, nstartkmeans = 10, gamma=0.05)
      res <- kGmedian(t(muhat), ncenters = K, nstart = 50, nstartkmeans = 10, gamma=0.05)
      #rebind("res", kGmedian(t(muhat), ncenters = K, nstart = 50, nstartkmeans = 10, gamma=0.05))
      #innerf("res", kGmedian(t(muhat), ncenters = K, nstart = 50, nstartkmeans = 10, gamma=0.05)  )
    }
  }

  #assign("res", res, environment())
  #print(get("res", thisEnv))
  h0 <- survfit(Surv(ttilde, status) ~ res$cluster[data$ff], data = data)
  mchat <- do.call("rbind", lapply(1:length(unique(data$ff)),
                                   function(x){chat_grid(h1, h0, data, res, kbin, x)}))
  u <- mchat$surv1 - mchat$surv0

  if(method == "kmeans"){
   # t1 <- max(tapply(u^2, mchat$f, mean))
    t <- sum(tapply(u^2, mchat$f, sum))
  }
  if(method == "kmedians"){
   # t1 <- max(tapply(abs(u), mchat$f, mean))
    t <- sum(tapply(abs(u), mchat$f, sum))
  }
  return(list(t = t, res = res))
}










# bootstrap simple  for using in each newf
simpleboot <- function(x){
  d <- data.frame(ttilde = x[, 1], status = x[, 2])
  ii <- sample.int(dim(d)[1], replace = TRUE)
  newd <- data.frame(d[ii,], f = x[, 3], ff = x[, 4])
  return(newd)
}



# simple bootstrap taking into account the groups under H_0
bootstrap <- function(data, newf, K, kbin, method){
  if (K == 1) {
    n <- dim(data)[1]
    ii <- sample.int(n, size = n, replace = TRUE)
    databoot <- data.frame(data[ii,1:2], ff = data$ff)
    tboot <- Tvalue(databoot, K, kbin, method)$t
  }else{
    aux <- by(data, newf, simpleboot)
    databoot <- data.frame()
    for (i in 1:K) {
      databoot <- rbind(databoot,aux[[i]])
    }
    tboot <- Tvalue(databoot, K, kbin, method)$t
  }
}







# function testing H_0 (k)
testing_k <- function(time, status, fac, k, kbin, nboot,
                      algorithm, seed, cluster){
  method <- algorithm
  nf <- nlevels(factor(fac))

  #########################################################
  #get("res", envir = environment())
  ########################################################

  # levels
  f <- factor(fac)
  lab <- levels(f)
  ff <- as.integer(f)

  data <- data.frame(ttilde = time, status = status, f = fac, ff = ff)

  # statistic from the sample
  aux <- Tvalue(data, k, kbin, method)
  tsample <- aux$t

  newf <- aux$res$cluster[data$ff]

  # bootstrap
  if (isTRUE(cluster)) {
    tboot <- foreach(i = 1:nboot, .combine = cbind) %dorng%
      bootstrap(data, newf, k, kbin, method)
  }else{
    tboot <- foreach(i = 1:nboot, .combine = cbind) %do%
      bootstrap(data, newf, k, kbin, method)
  }
  pvalue <- mean(unlist(tboot) >= tsample)

  return(list(pvalue = pvalue, t = tsample, levels = lab,
              cluster = as.numeric(aux$res$cluster)))
}

# corrigiendo "<<-" no visible binding
#
# rebind <- function(name, value, env = parent.frame()) {
#   if (identical(env, emptyenv())) {
#     stop("Can't find ", name, call. = FALSE)
#   } else if (exists(name, envir = env, inherits = FALSE)) {
#     assign(name, value, envir = env)
#   } else {
#     rebind(name, value, parent.env(env))
#   }
#}


