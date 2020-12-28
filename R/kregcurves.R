#' k-groups of multiple regression curves
#'
#' @description Function for grouping regression curves, given a number k,
#' based on the k-means or k-medians algorithm.
#'
#' @param y Response variable.
#' @param x Dependent variable.
#' @param z Categorical variable indicating the population to which
#' the observations belongs.
#' @param k An integer specifying the number of groups of curves to be
#'  performed.
#' @param kbin Size of the grid over which the survival functions
#' are to be estimated.
#' @param h The kernel bandwidth smoothing parameter.
#' @param algorithm A character string specifying which clustering algorithm is used,
#'  i.e., k-means(\code{"kmeans"}) or k-medians (\code{"kmedians"}).
#' @param seed Seed to be used in the procedure.
#'
#'@return
#'A list containing the following items:
#'\item{measure}{Value of the test statistic.}
#'  \item{levels}{Original levels of the variable \code{fac}.}
#'  \item{cluster}{A vector of integers (from 1:k) indicating the cluster to
#'  which each curve is allocated.}
#'  \item{centers}{An object containing the fitted centroids
#'  (mean of the curves pertaining to the same group).}
#'  \item{curves}{An object containing the fitted regression
#'  curves for each population.}
#'@author Nora M. Villanueva and Marta Sestelo.
#'
#'@examples
#' library(clustcurv)
#'
#' # Regression: 2 groups k-means
#' r2 <- kregcurves(y = barnacle5$DW, x = barnacle5$RC,
#' z = barnacle5$F, k = 2, algorithm = "kmeans")
#'
#' data.frame(level = r2$level, cluster = r2$cluster)
#'
#'
#'
#' @importFrom survival survfit
#' @importFrom stats kmeans p.adjust
#' @importFrom Gmedian kGmedian
#' @export


kregcurves <- function(y, x, z, k, kbin = 50, h = -1,
                       algorithm = "kmeans", seed = NULL){

  method <- "regression"

  # Defining error codes
  error.code.0 <- "Argument seed must be a numeric."
  #error.code.1 <- "Argument method must be a string with 'survival' or 'regression'."
  error.code.2 <- "Argument algorithm must be a string with 'kmeans' or 'kmedians'."

  # # Checking method  as strings and type
  # if (!is.character(method) ) {
  #   stop(error.code.1)
  # }else if (nchar(method)!= 8 & nchar(method)!= 10) {
  #   stop(error.code.1)
  # }else if(method != 'survival' & method != 'regression') {
  #   stop(error.code.1)
  # }

  # Checking algorithm  as string and type
  if (!is.character(algorithm) ) {
    stop(error.code.2)
  }else if (nchar(algorithm)!= 6 & nchar(algorithm)!= 8) {
    stop(error.code.2)
  }else if(algorithm != 'kmeans' & algorithm != 'kmedians') {
    stop(error.code.2)
  }

  # Checking seed as numeric
  if (!is.null(seed)) {
    if(!is.numeric(seed)){
      stop(error.code.0)
    }
    set.seed(seed)
  }

  time <- y
 # status <- weights
  fac <- z

  nf <- nlevels(factor(fac))

  # levels
  f <- factor(fac)
  lab <- levels(f)
  ff <- as.integer(f)



  # if(method == "survival"){
  #   data <- data.frame(ttilde = time, status = status, f = fac, ff = ff)
  #   # measure
  #   aux <- Tvalue(data, k, kbin, method = algorithm)
  #   tsample <- aux$t
  #   cluster <- aux$res$cluster
  #
  #   # muhat under h0 and under h1
  #   h0 <- survfit(Surv(ttilde, status) ~ aux$res$cluster[data$ff], data = data)
  #   h1 <- survfit(Surv(ttilde, status) ~ ff, data = data)
  # }
  if(method == "regression"){
    aux <- kgroups(x = x, y = y, f = z, nboot = 0, K = k,
                         h = h, ngrid = kbin, algorithm = algorithm, seed = seed,
                         cluster = FALSE)

    tsample <- aux$t
    cluster <- aux$cluster
    #h0 <- aux$centers
    #h1 <- aux$muhat
    data <- data.frame(x = x, y = y, f = z)
    data0 <- data
    data0$f <- aux$levels[aux$cluster[data$f]]
    h0 <- by(data0, data0$f, muhatrfast2, h = h, kbin = kbin)
    h1 <- by(data, data$f, muhatrfast2, h = h, kbin = kbin)
  }


  res <- list(measure = as.numeric(tsample), levels = lab,
              cluster = as.numeric(cluster), centers = h0, curves = h1,
              method = method, data = data, algorithm = algorithm, call = match.call())
  class(res) <- c("kcurves", "clustcurves")
  return(res)
}














