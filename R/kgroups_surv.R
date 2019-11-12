#' k-groups of survival curves
#'
#' @description Function for grouping survival curves, given a number k,
#' based on the k-means or k-medians algorithm.
#'
#' @param y Survival time (method = "survival") or response variable (method = "regression").
#' @param x Only for method = "regression". Dependent variable.
#' @param z Categorical variable indicating the population to which
#' the observations belongs.
#' @param weights Only for method = "survival". Censoring indicator of the survival
#' time of the process; 0 if the total time is censored and 1 otherwise.
#' @param k An integer specifying the number of groups of curves to be
#'  performed.
#' @param kbin Size of the grid over which the survival functions
#' are to be estimated.
#' @param h The kernel bandwidth smoothing parameter (for method = "regression").
#' @param algorithm A character string specifying which clustering algorithm is used,
#'  i.e., k-means(\code{"kmeans"}) or k-medians (\code{"kmedians"}).
#' @param seed Seed to be used in the procedure.
#'
#'@return
#'A list containing the following items:
#'\item{measure}{A measure of...}
#'  \item{levels}{Original levels of the variable \code{fac}.}
#'  \item{cluster}{A vector of integers (from 1:k) indicating the cluster to
#'  which each curve is allocated.}
#'  \item{centers}{An object of class \code{survfit} containing the centroids
#'  (mean of the curves pertaining to the same group).}
#'  \item{curves}{An object of class \code{survfit} containing the survival
#'  curves for each population.}
#'@author Marta Sestelo, Nora M. Villanueva.
#'
#'@examples
#' library(clustcurv)
#' library(survival)
#' data(veteran)
#'
#' # Survival: 2 groups k-means
#' cl2 <- kclustcurv(time = veteran$time, status = veteran$status,
#' fac = veteran$celltype, k = 2, algorithm = "kmeans")
#'
#' data.frame(level = cl2$level, cluster = cl2$cluster)
#'
#'
#' # Survival: 2 groups k-medians
#' cl2 <- kclustcurv(time = veteran$time, status = veteran$status,
#' fac = veteran$celltype, method = "survival", k = 2, algorithm = "kmedians")
#'
#' data.frame(level = cl2$level, cluster = cl2$cluster)
#'
#'
#'
#' # Survival: 3 groups
#' cl3 <- kclustcurv(time = veteran$time, status = veteran$status,
#' fac = veteran$celltype, method = "survival", k = 3, algorithm = "kmeans")
#'
#' data.frame(level = cl3$level, cluster = cl3$cluster)
#'
#'
#'
#'
#'
#' @importFrom survival survfit
#' @importFrom stats kmeans p.adjust
#' @importFrom Gmedian kGmedian
#' @export


kclustcurv <- function(y, x, z, weights, k, method = "survival", kbin = 50,
                         h = -1, algorithm = "kmeans", seed = NULL){
  if (!is.null(seed)) set.seed(seed)
  time <- y
  status <- weights
  fac <- z

  nf <- nlevels(factor(fac))

  # levels
  f <- factor(fac)
  lab <- levels(f)
  ff <- as.integer(f)

  data <- data.frame(ttilde = time, status = status, f = fac, ff = ff)

  if(method == "survival"){
    # measure
    aux <- Tvalue(data, k, kbin, method = algorithm)
    tsample <- aux$t
    cluster <- aux$res$cluster

    # muhat under h0 and under h1
    h0 <- survfit(Surv(ttilde, status) ~ aux$res$cluster[data$ff], data = data)
    h1 <- survfit(Surv(ttilde, status) ~ ff, data = data)
  }
  if(method == "regression"){
    aux <- kgroups(x = x, y = y, f = z, nboot = 0, K = k,
                         h = h, ngrid = kbin, algorithm = algorithm, seed = seed,
                         cluster = FALSE)

    tsample <- aux$t
    cluster <- aux$cluster
    h0 <- aux$centers
    h1 <- aux$muhat
  }
  res <- list(measure = as.numeric(tsample), levels = lab,
              cluster = as.numeric(cluster), centers = h0, curves = h1)
  class(res) <- c("kgroups_surv", "clustcurv_surv")
  return(res)
}














