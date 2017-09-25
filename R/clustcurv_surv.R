#' Clustering survival curves
#'
#' @description Function for grouping survival curves based on the k-means or
#' k-medians algorithm. It returns the number of groups and the assignement.
#'
#' @param time Survival time.
#' @param status Censoring indicator of the survival time of the process; 0 if
#' the total time is censored and 1 otherwise.
#' @param fac Categoriacl variable indicating the population to which
#' the subject belongs
#' @param kvector A vector specifying the number of groups of curves to be
#'  checking.
#' @param kbin Size of the grid over which the survival functions
#' are to be estimated.
#' @param algorithm A character string specifying which clustering algorithm is used,
#'  i.e., k-means(\code{"kmeans"}) or k-medians (\code{"kmedians"}).
#' @param alpha Seed to be used in the procedure.
#' @param cluster A logical value. If  \code{TRUE} (default), the
#'  testing procedure is  parallelized. Note that there are cases
#'  (e.g., a low number of bootstrap repetitions) that R will gain in
#'  performance through serial computation. R takes time to distribute tasks
#'  across the processors also it will need time for binding them all together
#'  later on. Therefore, if the time for distributing and gathering pieces
#'  together is greater than the time need for single-thread computing, it does
#'  not worth parallelize.
#' @param ncores An integer value specifying the number of cores to be used
#' in the parallelized procedure. If \code{NULL} (default), the number of cores
#' to be used is equal to the number of cores of the machine - 1.
#' @param seed Seed to be used in the procedure.
#'
#'@return
#'A list containing the following items:
#'  \item{table}{A data frame containing the null hypothesis tested, the values
#'  of the test statistics and the obtained pvalues.}
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
#' data(colonCS)
#'
#' res <- clustcurv_surv(time = veteran$time, status = veteran$status,
#' fac = veteran$celltype, algorithm = "kmeans")
#'
#' #res <- clustcurv_surv(colonCS$time, status = colonCS$status, fac = colonCS$nodes, nboot = 20)
#'
#'
#' @importFrom survival survfit
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom parallel parLapply
#' @importFrom parallel stopCluster
#' @importFrom doRNG %dorng%
#' @importFrom foreach %do% foreach
#'



#'@export









clustcurv_surv <- function(time, status, fac, kvector = NULL, kbin = 50,
                           nboot = 100, algorithm = "kmeans", alpha = 0.05,
                           cluster = FALSE, ncores = NULL, seed = NULL){

  #---------------

  if (!is.null(seed)) set.seed(seed)

  #---------------
  if (isTRUE(cluster)) {
    if (is.null(ncores)) {
      num_cores <- detectCores() - 1
    }else{
      num_cores <- ncores
    }
    registerDoParallel(cores = num_cores)
    if (!is.null(seed)) registerDoRNG(seed)
    on.exit(stopImplicitCluster())
  }
  #------------------



  if(is.null(kvector)) kvector <- c(1:100)

  ii <- 1
  pval <- NA
  tval <- NA
  h0tested <- NA

  for (k in kvector){
    if(k == 1){
      cat(paste("Checking",k, "cluster...", "\n"), sep = "")
    }else{
      cat(paste("Checking",k, "clusters...", "\n"), sep = "")
    }
    aux <- testing_k(time = time, status = status, fac = fac, k = k,
                     kbin = kbin, nboot = nboot, algorithm = algorithm,
                     seed = seed)

    pval[ii] <- aux$pvalue
    tval[ii] <- aux$t
    h0tested[ii] <- k
    ii <- ii + 1
    if(aux$pvalue >= alpha){break}
  }


  if(k == 1){
    cat("\n")
    cat(paste("Finally, there is one cluster.", "\n"), sep = "")
  }else{
    cat("\n")
    cat(paste("Finally, there are",k, "clusters.", "\n"), sep = "")
  }


  # muhat under h0 and under h1
  h0 <- survfit(Surv(time, status) ~ aux$cluster[fac])
  h1 <- survfit(Surv(time, status) ~ fac)

  res <- list(table = data.frame(H0 = h0tested, Tvalue = tval, pvalue = pval),
              levels = aux$levels, cluster = as.numeric(aux$cluster),
              centers = h0, curves = h1)
  class(res) <- "clustcurv_surv"
  return(res)

} #end clustcurv



