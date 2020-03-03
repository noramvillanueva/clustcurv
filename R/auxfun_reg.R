

# Funtion to obtain the estimates by kernel
muhatrfast <- function(x, grid, h){
  d <- data.frame(x = x[, 1], y = x[, 2])
  # print(c(length(d$x), "n", x[1, 3]))
  if (length(d$x) > 10) {
    model <- frfast(y ~ x, data = d, kbin = length(grid),
                    h0 = h, p = 2, nboot = 1)
    #model <- frfast(y ~ s(x), data = d, kbin = length(grid),
    #                h0 = h, p = 2, nboot = 1, smooth = "splines")
    fit <- predict(model, newdata = data.frame(x = grid))$Estimation[,1]
  }else if (length(d$x) > 5) {
    fit <- as.numeric(predict(lm(y ~ poly(x, 3), data = d),
                              newdata = data.frame(x = grid), type = "response"))
  }else{
    fit <- as.numeric(predict(lm(y ~ x, data = d),
                              newdata = data.frame(x = grid), type = "response"))
  }
  return(fit)
}


# Funtion to obtain the estimates by kernel
muhatrfast2 <- function(x, h){
  d <- data.frame(x = x[, 1], y = x[, 2])
  # print(c(length(d$x), "n", x[1, 3]))
  if (length(d$x) > 10) {
    model <- frfast(y ~ x, data = d, kbin = 200,
                    h0 = h, p = 2, nboot = 1)
    #model <- frfast(y ~ s(x), data = d, kbin = length(grid),
    #                h0 = h, p = 2, nboot = 1, smooth = "splines")
    fit <- predict(model, newdata = data.frame(x = d$x))$Estimation[,1]
  }else if (length(d$x) > 5) {
    fit <- as.numeric(predict(lm(y ~ poly(x, 3), data = d),
                              newdata = data.frame(x = d$x), type = "response"))
  }else{
    fit <- as.numeric(predict(lm(y ~ x, data = d),
                              newdata = data.frame(x = d$x), type = "response"))
  }
  return(fit)
}



# Funtion to evaluate the statistics test
Tvalue_app <- function(y, x, f, K, grid, h, ngrid, algorithm){
  nf <- length(unique(f))
  data <- data.frame(x, y, f)
  mins <- tapply(data[, 1], data[, 3], min)
  maxs <- tapply(data[, 1], data[, 3], max)
  grid <- seq(max(mins), min(maxs), length = ngrid)

  aux <- by(data, data$f, muhatrfast, grid = grid, h = h)
  Mf <- matrix(unlist(aux), ncol = nf, nrow = length(grid))
  if (sum(is.na(Mf)) > 0) {
    warning("Problem in estimation, there are NAs. Check another h",
            call. = TRUE)}
  if(algorithm == "kmeans"){
  A <- kmeans(t(Mf), centers = K, iter.max = 500, nstart = 20, algorithm = "Hartigan-Wong")
  }
  if(algorithm == "kmedians"){
    if (K == 1){
      A <- list()
      A$cluster <- rep(1, length(unique(data$f)))
    } else {
      A <- kGmedian(t(Mf), ncenters = K, nstart = 50, nstartkmeans = 10, gamma=0.05)
    }
  }

  aux <- by(data, A$cluster[f], muhatrfast, grid = grid, h = h)
  Mg <- matrix(unlist(aux), ncol = K, nrow = length(grid))
  if (sum(is.na(Mg)) > 0) {
    warning("Problem in estimation, there are NAs. Check another h",
            call. = TRUE)}
  if(algorithm == "kmeans"){
  t <- sum(colSums((Mf - Mg[, A$cluster]) ^ 2))
  }
  if(algorithm == "kmedians"){
  t <- sum(colSums(abs(Mf - Mg[, A$cluster])))
  }
 # t2 <- sum(colSums(abs(Mf - Mg[, A$cluster])))
#  comb <- by(c(1:nf), A$cluster, maxD, matrix = Mf)
 # t3 <- sum(unlist(sapply(comb,function(x){return(x[1])})))
#  t4 <- sum(unlist(sapply(comb,function(x){return(x[2])})))
  return(list(t = t))
}





# Function to test K groups (in app.R file)
kgroups <- function(x, y, f, nboot = 100, K = 3, h, ngrid, algorithm, seed,
                    cluster) {
  nf <- length(unique(f))
  data <- data.frame(x, y, f)
  mins <- tapply(data[, 1], data[, 3], min)
  maxs <- tapply(data[, 1], data[, 3], max)
  grid <- seq(max(mins), min(maxs), length = ngrid)

  aux <- by(data, data$f, muhatrfast, grid = grid, h = h)
  Mf <- matrix(unlist(aux), ncol = nf, nrow = length(grid))
  if (sum(is.na(Mf)) > 0) {
    warning("Problem in estimation, there are NAs. Check another h",
            call. = TRUE)}
  if(algorithm == "kmeans"){
  A <- kmeans(t(Mf), centers = K, iter.max = 50, nstart = 500)
  }
  if(algorithm == "kmedians"){
    if (K == 1){
      A <- list()
      A$cluster <- rep(1, length(unique(data$f)))
    } else {
      A <- kGmedian(t(Mf), ncenters = K, nstart = 50, nstartkmeans = 10, gamma=0.05)
    }
  }


  aux <- by(data, A$cluster[f], muhatrfast, grid = grid, h = h)
  Mg <- matrix(unlist(aux), ncol = K, nrow = length(grid))
  if (sum(is.na(Mg)) > 0) {
    warning("Problem in estimation, there are NAs. Check another h",
            call. = TRUE)}
  if(algorithm == "kmeans"){
  t <- sum(colSums((Mf - Mg[, A$cluster]) ^ 2))
  }
  if(algorithm == "kmedians"){
  t <- sum(colSums(abs(Mf - Mg[, A$cluster])))
  }
 # t2 <- sum(colSums(abs(Mf - Mg[, A$cluster])))
#  comb <- by(c(1:nf), A$cluster, maxD, matrix = Mf)
 # t3 <- sum(unlist(sapply(comb,function(x){return(x[1])})))
#  t4 <- sum(unlist(sapply(comb,function(x){return(x[2])})))


  if (nboot != 0){
  n <- length(x)
  pred <- numeric(n)
  for (i in 1:nf) {
    ii <- f == i
    xaux <- x[ii]
    Aux <- interpSpline(Mf[, i] ~ grid)
    pred[ii] <- predict(Aux, xaux)$y
  }

  pred0 <- numeric(n)
  for (i in 1:K) {
    ii <- A$cluster[f] == i
    xaux <- x[ii]
    Aux <- interpSpline(Mg[, i] ~ grid)
    pred0[ii] <- predict(Aux, xaux)$y
  }

  err0 <- y - pred0
  err0 <- err0 - mean(err0)


  # bootstrap
  yboot <- replicate(nboot, pred0 + err0 *
                       sample(c(-sqrt(5) + 1, sqrt(5) + 1)/2, size = n,
                              replace = TRUE,
                              prob = c(sqrt(5) + 1, sqrt(5) - 1)/(2 * sqrt(5))))



  if (isTRUE(cluster)) {
  Tboot <- foreach(i = 1:nboot, .combine = cbind) %dorng%
    Tvalue_app(yboot[, i], x = x, f = f, K = K, grid = grid,
               h = h, ngrid = ngrid, algorithm = algorithm)
  }else{
    Tboot <- foreach(i = 1:nboot, .combine = cbind) %do%
      Tvalue_app(yboot[, i], x = x, f = f, K = K, grid = grid,
                 h = h, ngrid = ngrid, algorithm = algorithm)
  }



    pvalue <- mean(Tboot >= t)
  }else{
  pvalue <- NULL
}
  return(list(pvalue = pvalue, t = t, muhat = Mf, xgrid = grid,
              levels = levels(factor(f)), centers = Mg, cluster = A$cluster,
              grid = grid))

}





