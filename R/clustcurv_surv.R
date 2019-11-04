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
#' @param nboot Number of bootstrap repeats.
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
#' @param multiple A logical value. If  \code{TRUE} (not default), the resulted
#' pvalues are adjunted by using one of several methods for multiple comparisons.
#' @param multiple.method Correction method. See Details.
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
#'
#'@details The adjustment methods include the Bonferroni correction ("bonferroni")
#' in which the p-values are multiplied by the number of comparisons.
#' Less conservative corrections are also included by Holm (1979) ("holm"),
#' Hochberg (1988) ("hochberg"), Hommel (1988) ("hommel"),
#' Benjamini & Hochberg (1995) ("BH" or its alias "fdr"), and
#' Benjamini & Yekutieli (2001) ("BY"), respectively.
#' A pass-through option ("none") is also included.
#'
#'
#'@author Marta Sestelo, Nora M. Villanueva.
#'
#'@examples
#'\donttest{
#' library(clustcurv)
#' library(survival)
#' library(condSURV)
#' data(veteran)
#' data(colonCS)
#'
#' res <- clustcurv_surv(time = veteran$time, status = veteran$status,
#' fac = veteran$celltype, algorithm = "kmeans")
#' }
#' #res <- clustcurv_surv(colonCS$time, status = colonCS$status, fac = colonCS$nodes, nboot = 20)
#'
#'
#' @importFrom survival survfit
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom parallel parLapply
#' @importFrom parallel stopCluster
#' @importFrom doRNG %dorng% registerDoRNG
#' @importFrom foreach %do% foreach
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @export









clustcurv_surv <- function(time, status, fac, kvector = NULL, kbin = 50,
                           nboot = 100, algorithm = "kmeans", alpha = 0.05,
                           cluster = FALSE, ncores = NULL, seed = NULL,
                           multiple = FALSE, multiple.method = "holm"){

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

  accept <- 0
  ii <- 1
  pval <- NA
  tval <- NA
  h0tested <- NA
  aux <- list()

  for (k in kvector){
    if(k == 1){
      cat(paste("Checking",k, "cluster...", "\n"), sep = "")
    }else{
      cat(paste("Checking",k, "clusters...", "\n"), sep = "")
    }
    aux[[ii]] <- testing_k(time = time, status = status, fac = fac, k = k,
                     kbin = kbin, nboot = nboot, algorithm = algorithm,
                     seed = seed, cluster = cluster)

    pval[ii] <- aux[[ii]]$pvalue
    tval[ii] <- aux[[ii]]$t
    h0tested[ii] <- k


    if(isTRUE(multiple)){
      pval <- p.adjust(pval, method = multiple.method)
      ind <- pval >= alpha
      if(sum(ind) > 0){
        ind_list <- which(ind)[1]
        k <- kvector[ind_list]
        aux <- aux[[ind_list]]
        accept <- 1
        break
        }
      }

    if(aux[[ii]]$pvalue >= alpha){
      aux <- aux[[ii]]
      accept <- 1
      break
      }
    ii <- ii + 1
  }


  if (accept == 1) {

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

  }else{
    k <- paste( ">", k, sep ="")
    aux$levels <- NA
    aux$cluster <- NA
    h0 <- NA
    h1 <- survfit(Surv(time, status) ~ fac)
    cat("\n")
    cat(paste("The number 'k' of clusters has not been found, try another kvector.", "\n"), sep = "")

  }


  res <- list(num_groups = k, table = data.frame(H0 = h0tested, Tvalue = tval, pvalue = pval),
              levels = aux$levels, cluster = as.numeric(aux$cluster),
              centers = h0, curves = h1)
  class(res) <- "clustcurv_surv"
  return(res)

} #end clustcurv



