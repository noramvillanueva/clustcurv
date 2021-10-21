# function for obtaining estimates for each curve under h0 and under h1
# (for using in the test estatistics) in a GRID. For h0, it is used the mean of the curves that
# belong to the same cluster
chat_grid_cif_mean <- function(h1, h0, data, ii, xbin, j){
  jscen <- c(1:max(data$ff))[ii == j+1]
  aux1 <- summary(h1, times = xbin, extend = TRUE)
  aux1 <- aux1$pstate
  surv1 <- c(aux1[, jscen])
  if(length(jscen) == 1){
    surv0 <- aux1[, jscen]
  }else{
    surv0 <- rowMeans(aux1[, jscen])
  }
  surv0 <- rep(surv0, length(jscen))
  f <- rep(jscen, each = length(xbin))
  ress <- data.frame(surv1, surv0, f)
  return(ress)
}

# function for obtaining estimates for each curve under h0 and under h1
# (for using in the test estatistics) in a GRID
chat_grid_cif <- function(h1, h0, data, ii, kbin, j){
  mx <- max(data$ttilde[data$ff == j])
 # mx <- 10
  grid <- seq(from = min(data$ttilde[data$ff == j]),
              to = mx, length.out = kbin)
  aux1 <- summary(h1, times = grid, extend = TRUE)
  surv1 <- aux1$pstate[, j]
  aux0 <- summary(h0, times = grid, extend = TRUE)
 # if (length(unique(res$cluster)) == 1) {
 #   surv0 <- aux0$surv
 # }else{

    surv0 <- aux0$pstate[, ii[j]] #estimaciones del cluster que pertenece)
  if(j != 1) {surv0/(max(data$ff) - 1)}


 # }
  f <- rep(j, length(surv0))
  ress <- data.frame(surv1, surv0, f)
  return(ress)
}



# test statistic (valid for kmeans or kmedians algorithm, fill in method argument)
Tvalue_cif <- function(data, K, kbin, method){
  h1 <- Cuminc(time = "ttilde", status = "status", data = data)
  mx <- max(data$ttilde)
  #mx <- 23
  xbin <- seq(from = min(data$ttilde), to = mx, length.out = kbin)   # !OJO cambiar esto para que el último tiempo nunca sea censurado!!! que el max sea el max ttilde no censurado!!!
  aux <- summary(h1, times = xbin)

  # kbin_km <- as.numeric(table(aux$strata))
  # if (sum(kbin_km != kbin) > 0) { # if TRUE, there is some curve without estimates at each kbin
  #   h1_param <- do.call("cbind", as.list(by(data, data$ff, survfitpar, xbin)))
  #   muhat <- sapply(1:length(unique(data$ff)),
  #                   function(x){joint_km_np(aux, h1_param, x, kbin)})
  # }else{
  #   muhat <- matrix(aux$surv, ncol = nlevels(as.factor(data$ff)), nrow = kbin)
  # }

  muhat <- aux$pstate[,-1]

  if(method == "kmeans"){
    res <- kmeans(t(muhat), centers = K, iter.max = 50, nstart = 500)
  }
  if(method == "kmedians"){
    if (K == 1){
      res <- list()
      res$cluster <- rep(1, length(unique(data$ff)))
    } else {
      res <- kGmedian(t(muhat), ncenters = K, nstart = 50, nstartkmeans = 10, gamma=0.05)
    }
  }

  ii <- c(1,res$cluster+1)
  data$status0 <- ii[data$ff] - 1
  h0 <- Cuminc(time = "ttilde", status = "status0", data = data)
  #
  #mchat <- do.call("rbind", lapply(1:length(unique(data$ff)),
  #                                 function(x){chat_grid_cif2(h1, h0, data, ii, xbin, x)}))
  mchat <- do.call("rbind", lapply(1:K,
                                   function(x){chat_grid_cif_mean(h1, h0, data, ii, xbin, x)}))

 # mchat <- chat_grid_cif2(h1, h0, data, ii, xbin)


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
simpleboot_cif <- function(x){
  if(x[1, 2] == 0) {
    newd <- x
  }else{
   # d <- data.frame(ttilde = x[, 1], status = x[, 2])
    d <- data.frame(ttilde = x[, 1])
    ii <- sample.int(dim(d)[1], replace = TRUE)
    newd <- data.frame(ttilde = d[ii,], status = x[, 2], f = x[, 3], ff = x[, 4])
  }
  return(newd)
}


# simple bootstrap taking into account the groups under H_0
bootstrap_cif <- function(data, newf, K, kbin, method){
  # if (K == 1) {
  #   # n <- dim(data)[1]
  #   # ii <- sample.int(n, size = n, replace = TRUE)
  #   # databoot <- data.frame(data[ii,1:2], ff = data$ff)
  #   # tboot <- Tvalue_cif(databoot, K, kbin, method)$t
  #   #
  #  # newf_fake <- newf
  #  # newf_fake[newf_fake != 1] <- 2
  #   aux <- by(data, newf_fake, simpleboot)
  #   databoot <- data.frame()
  #   for (i in 1:2) {
  #     databoot <- rbind(databoot,aux[[i]])
  #   }
  #   tboot <- Tvalue_cif(databoot, K, kbin, method)$t
  #
  #
  # }else{
    aux <- by(data, newf, simpleboot_cif)
    databoot <- data.frame()
    for (i in 1:(K+1)) {
      databoot <- rbind(databoot,aux[[i]])
    }
    tboot <- Tvalue_cif(databoot, K, kbin, method)$t
  #}
}



# function testing H_0 (k)
testing_k_cif <- function(time, status, fac, k, kbin, nboot,
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
  aux <- Tvalue_cif(data, k, kbin, method)
  tsample <- aux$t

  #newf <- aux$res$cluster[data$ff]

  newf <- c(1,aux$res$cluster+1)[data$ff]

  # bootstrap
  if (isTRUE(cluster)) {
    tboot <- foreach(i = 1:nboot, .combine = cbind, .export = "bootstrap_cif") %dorng%
      bootstrap_cif(data, newf, k, kbin, method)
  }else{
    tboot <- foreach(i = 1:nboot, .combine = cbind) %do%
      bootstrap_cif(data, newf, k, kbin, method)
  }
  pvalue <- mean(unlist(tboot) >= tsample)

  return(list(pvalue = pvalue, t = tsample, levels = lab,
              cluster = as.numeric(aux$res$cluster)))
}

