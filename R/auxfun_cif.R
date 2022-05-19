# function for obtaining estimates for each curve under h0 and under h1
# (for using in the test statistics) in a GRID. For h0, it is used the mean of the curves that
# belong to the same cluster
chat_grid_cif_mean <- function(h1, h0, data, ii, xbin, j){
  jscen <- c(1:max(data$status+1))[ii == j+1]
  aux1 <- summary(h1, times = xbin, extend = TRUE)
  aux1 <- aux1$pstate
  surv1 <- c(aux1[, jscen])
  if(length(jscen) == 1){
    surv0 <- aux1[, jscen]
  }else{
    surv0 <- rowMeans(aux1[, jscen])
  }
  surv0 <- rep(surv0, length(jscen))
  ff <- rep(jscen, each = length(xbin))
  ress <- data.frame(surv1, surv0, ff)
  return(ress)
}

# function for obtaining estimates for each curve under h0 and under h1
# (for using in the test estatistics) in a GRID NO VALE!
chat_grid_cif <- function(h1, h0, data, ii, kbin, j){
  mx <- max(data$ttilde[data$ff == j])
  #mx <- 70
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
Tvalue_cif <- function(data, K, kbin, method, group = NULL, max_time = max_time,
                       weights = NULL){

  if(is.null(group)){
    h1 <- Cuminc(time = "ttilde", status = "status", data = data)
  }else{
  h1 <- Cuminc(time = "ttilde", status = "status", data = data, group = "f")
 }
  if(is.null(max_time)) max_time <- max(data$ttilde)

  xbin <- seq(from = min(data$ttilde), to = max_time, length.out = kbin)
  aux <- summary(h1, times = xbin)


  muhat <- aux$pstate[,-1]

  if(method == "kmeans"){
    res <- kmeans(t(muhat), centers = K, iter.max = 50, nstart = 500)
  }
  if(method == "kmedians"){
    if (K == 1){
      res <- list()
      res$cluster <- rep(1, length(unique(data$status))-1)
    } else {
      res <- kGmedian(t(muhat), ncenters = K, nstart = 50, nstartkmeans = 10, gamma=0.05)
    }
  }

  ii <- c(1,res$cluster+1)
  data$status0 <- ii[data$status+1] - 1
  h0 <- Cuminc(time = "ttilde", status = "status0", data = data)

  mchat <- do.call("rbind", lapply(1:K,
                                   function(x){chat_grid_cif_mean(h1, h0, data, ii, xbin, x)}))


  if(is.null(weights)){
    w <- rep(1, length(mchat$surv1))
  }else if(weights == "KM"){
    status1 <- data$status
    status1[status1 != 0] <- 1
    #d <- data.frame(time = data$ttilde, status = status1)
    #hat_status1 <- fitted(glm(status ~ time, family = binomial, data = d),
    #                      type = "response", newdata = data.frame(time = xbin))
    wei <- survidm::PKMW(time = data$ttilde, status = status1)
    d <- data.frame(y = wei, x = data$ttilde)
    w <- as.numeric(predict(lm(y ~ poly(x, 3), data = d),
                              newdata = data.frame(x = xbin), type = "response"))
    ii <- which(w < 0)
    w[ii] <- NA

    #kmw_fun <- splinefun(x= data$ttilde, y = wei, method = "natural")
    #w <- kmw_fun(xbin)
    w <- w**(-1/5)
    w <- (w - min(w, na.rm = TRUE))/(max(w, na.rm = TRUE) - min(w, na.rm = TRUE))
    w <- rep(w, length(unique(data$status))-1)



  }else if(weights == "var"){
    m0hat <- summary(h0, times = xbin)
    se <- as.matrix(m0hat$std.err[, -1]) # deleting 0
    inv_se <- 1/(se+1e-3)
    inv_se[1] <- inv_se[2]
    #ww <- inv_se
    ww <- apply(inv_se, 2, function(x)(x-min(x))/(max(x)-min(x)))
    w <- c(); ff <- c()
    for (j in 1:K){
      jscen <- c(1:max(data$status+1))[ii == j+1] # las curvas del j centroide
      w <- c(w, rep(ww[, j], length(jscen)))
      ff <- c(ff, rep(jscen, each = length(xbin)))
     # }
    }
  }



  u <- (mchat$surv1 - mchat$surv0)*w

  if(method == "kmeans"){
    t <- sum(tapply(u^2, mchat$f, sum, na.rm = TRUE))
  }
  if(method == "kmedians"){
    t <- sum(tapply(abs(u), mchat$f, sum, na.rm = TRUE))
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
    newd <- data.frame(ttilde = d[ii,], status = x[, 2])
    #newd <- data.frame(ttilde = d[ii,], status = x[, 2], f = x[, 3], ff = x[, 4])
  }
  return(newd)
}





# simple bootstrap taking into account the groups under H_0
bootstrap_cif <- function(data, newf, K, kbin, method, group, max_time, weights){
  aux <- by(data, newf, simpleboot_cif)
  databoot <- data.frame()
  for (i in 1:(K+1)) {
    databoot <- rbind(databoot,aux[[i]])
  }
  h1 <- Cuminc(time = "ttilde", status = "status", data = databoot)
  plot(h1)
  print(table(databoot$status))
  tboot <- Tvalue_cif(databoot, K, kbin, method, group = group, max_time = max_time,
                      weights)$t

}






# # bootstrap simple with weights  (CIFS) for using in each newf
# simpleboot_cif_weighted <- function(x, n){
#   if(x[1, 2] == 0) {
#     newd <- x
#   }else{
#     # d <- data.frame(ttilde = x[, 1], status = x[, 2])
#     w <- rep(n[1], length(x$status))
#     w[x$status == 2] <- n[2]
#     x$w <- as.vector(w)
#     #d <- data.frame(ttilde = x[, 1])
#     ii <- sample.int(length(x$status), length(x$status), replace = TRUE, prob = x$w)
#     newd <- x[ii, ]
#     newd <- newd[,-3]
#     #newd <- data.frame(ttilde = d[ii,], status = x[, 2], f = x[, 3], ff = x[, 4])
#   }
#   return(newd)
# }



# simple bootstrap taking into account the groups under H_0
bootstrap_cif_permutation <- function(data, newf, K, kbin, method, group, max_time, weights){
    ns <- tapply(data$ttilde, data$status, length)
    nsam <- round(sum(ns[-1])/2)
    aux <- by(data, data$status, simpleboot_cif, n = nsam)
    databoot <- data.frame()
    for (i in 1:(K+1)) {
      databoot <- rbind(databoot,aux[[i]])
    }
    tboot <- Tvalue_cif(databoot, K, kbin, method, group = group, max_time = max_time,
                        weights)$t

}



# bootstrap simple with weights  (CIFS) for using in each newf
simpleboot_cif_weighted <- function(x){
  if(x[1, 2] == 0) {
    newd <- x
  }else{
    pini <- prop.table(table(x$status))
    waux <- (1/length(pini)) * (1/pini)
    w <- rep(NA, length(x$status))
    for (i in unique(x$status)){
      w[x$status == i] <- waux[paste0("",i,"")]
    }
    w <- w/sum(w)
    aux <- sample.int(length(x$status), length(x$status), replace = TRUE, prob = as.vector(w))
    newd <- x[aux, ]
  }
  return(newd)
}



# simple bootstrap taking into account the groups under H_0, the sampling is done according
# to the
bootstrap_cif_balanced <- function(data, newf, K, kbin, method, group,
                                   max_time, weights){


  aux <- by(data, newf, simpleboot_cif_weighted)
  databoot <- data.frame()

  for (i in 1:(K+1)) {
    databoot <- rbind(databoot,aux[[i]])
  }
  #h1 <- Cuminc(time = "ttilde", status = "status", data = databoot)
  #plot(h1)
  #print(table(databoot$status))
  tboot <- Tvalue_cif(databoot, K, kbin, method, group = group, max_time = max_time,
                      weights)$t


}

# function for testing H_0 (k)
testing_k_cif <- function(time, status, fac, k, kbin, nboot,
                      algorithm, seed, cluster, max_time = max_time,
                      weights){
  method <- algorithm
  nf <- nlevels(factor(fac))

  #########################################################
  #get("res", envir = environment())
  ########################################################

  # levels
  f <- factor(fac)
  lab <- levels(f)
  ff <- as.integer(f)

  data <- data.frame(ttilde = time, status = status)

  print(table(data$status))
  # statistic from the sample
  aux <- Tvalue_cif(data, k, kbin, method, group = fac, max_time = max_time,
                    weights)
  tsample <- aux$t
  newf <- c(1,aux$res$cluster+1)[data$status + 1]

  # bootstrap
  if (isTRUE(cluster)) {
    tboot <- foreach(i = 1:nboot, .combine = cbind, .export = "bootstrap_cif") %dorng%
      bootstrap_cif_balanced(data, newf, k, kbin, method, group = fac, max_time = max_time, weights)
  }else{
    tboot <- foreach(i = 1:nboot, .combine = cbind) %do%
      bootstrap_cif_balanced(data, newf, k, kbin, method, group = fac, max_time = max_time, weights)
  }
  pvalue <- mean(unlist(tboot) >= tsample, na.rm = TRUE)

  return(list(pvalue = pvalue, t = tsample, levels = lab,
              cluster = as.numeric(aux$res$cluster)))
}

