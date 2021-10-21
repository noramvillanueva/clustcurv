#' k-groups of multiple Cumulative Incidents Functions curves
#'
#' @description Function for grouping CIF curves, given a number k,
#' based on the k-means or k-medians algorithm.
#'
#' @param time Survival time.
#' @param status Censoring indicator of the survival
#' time of the process; 0 if the total time is censored and 1 otherwise.
#' @param x Categorical variable indicating the population to which
#' the observations belongs.
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
#'\item{measure}{Value of the test statistics.}
#'  \item{levels}{Original levels of the variable \code{x}.}
#'  \item{cluster}{A vector of integers (from 1:k) indicating the cluster to
#'  which each curve is allocated.}
#'  \item{centers}{An object of class \code{survfit} containing the centroids
#'  (mean of the curves pertaining to the same group).}
#'  \item{curves}{An object of class \code{survfit} containing the survival
#'  curves for each population.}
#'@author Nora M. Villanueva and Marta Sestelo.
#'
#'@examples
#' library(clustcurv)
#' library(survival)
#' data(veteran)
#'
#' # Survival: 2 groups k-means
#' s2 <- ksurvcurves(time = veteran$time, status = veteran$status,
#' x = veteran$celltype, k = 2, algorithm = "kmeans")
#'
#' data.frame(level = s2$level, cluster = s2$cluster)
#'
#'
#' # Survival: 2 groups k-medians
#' s22 <- ksurvcurves(time = veteran$time, status = veteran$status,
#' x = veteran$celltype, k = 2, algorithm = "kmedians")
#'
#' data.frame(level = s22$level, cluster = s22$cluster)
#'
#'
#'
#'
#' @importFrom survival survfit
#' @importFrom stats kmeans p.adjust
#' @importFrom Gmedian kGmedian
#' @export


kcifcurves <- function(time, status = NULL, x, k, kbin = 50,
                        algorithm = "kmeans", seed = NULL){


  y <- time
  weights <- status
  z <- x
  method <- "cif"

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
  status <- weights
  fac <- z

  nf <- nlevels(factor(fac))

  # levels
  f <- factor(fac)
  lab <- levels(f)
  ff <- as.integer(f)




    data <- data.frame(ttilde = time, status = status, f = fac, ff = ff)
    # measure
    aux <- Tvalue_cif(data, k, kbin, method = algorithm)
    tsample <- aux$t
    #cluster <- aux$res$cluster
    cluster <- c(1,aux$res$cluster+1)

    # muhat under h0 and under h1
    data$status0 <- cluster[data$ff] - 1
    h0 <- Cuminc(time = "ttilde", status = "status0", data = data)
    h1 <- Cuminc(time = "ttilde", status = "status", data = data)



  res <- list(measure = as.numeric(tsample), levels = lab,
              cluster = as.numeric(cluster), centers = h0, curves = h1,
              method = method, data = data, algorithm = algorithm, call = match.call())
  class(res) <- c("kcurves", "clustcurves")
  return(res)
}














