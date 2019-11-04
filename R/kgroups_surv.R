#' k-groups of survival curves
#'
#' @description Function for grouping survival curves, given a number k,
#' based on the k-means or k-medians algorithm.
#'
#' @param time Survival time.
#' @param status Censoring indicator of the survival time of the process; 0 if
#' the total time is censored and 1 otherwise.
#' @param fac Categoriacl variable indicating the population to which
#' the subject belongs
#' @param k An integer specifying the number of groups of curves to be
#'  performed.
#' @param kbin Size of the grid over which the survival functions
#' are to be estimated.
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
#' # 2 groups k-means
#' cl2 <- kgroups_surv(time = veteran$time, status = veteran$status,
#' fac = veteran$celltype, k = 2, algorithm = "kmeans")
#'
#' data.frame(level = cl2$level, cluster = cl2$cluster)
#'
#'
#' # 2 groups k-medians
#' cl2 <- kgroups_surv(time = veteran$time, status = veteran$status,
#' fac = veteran$celltype, k = 2, algorithm = "kmedians")
#'
#' data.frame(level = cl2$level, cluster = cl2$cluster)
#'
#'
#'
#' # 3 groups
#' cl3 <- kgroups_surv(time = veteran$time, status = veteran$status,
#' fac = veteran$celltype, k = 3, algorithm = "kmeans")
#'
#' data.frame(level = cl3$level, cluster = cl3$cluster)
#'
#' @importFrom survival survfit
#' @importFrom stats kmeans p.adjust
#' @importFrom Gmedian kGmedian

#' @export


kgroups_surv <- function(time, status, fac, k, kbin = 50,
                    algorithm = "kmeans", seed = NULL){
  if (!is.null(seed)) set.seed(seed)

  method <- algorithm
  nf <- nlevels(factor(fac))

  # levels
  f <- factor(fac)
  lab <- levels(f)
  ff <- as.integer(f)

  data <- data.frame(ttilde = time, status = status, f = fac, ff = ff)

  # measure
  aux <- Tvalue(data, k, kbin, method)
  tsample <- aux$t

  # muhat under h0 and under h1
  h0 <- survfit(Surv(ttilde, status) ~ aux$res$cluster[data$ff], data = data)
  h1 <- survfit(Surv(ttilde, status) ~ ff, data = data)

  res <- list(measure = as.numeric(tsample), levels = lab,
              cluster = as.numeric(aux$res$cluster), centers = h0, curves = h1)
  class(res) <- c("kgroups_surv", "clustcurv_surv")
  return(res)
}














