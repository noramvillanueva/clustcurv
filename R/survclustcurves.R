#' Clustering multiple survival curves
#'
#' @description Function for grouping survival  curves based on the k-means or
#' k-medians algorithm. It returns the number of groups and the assignment.
#'
#' @param time Survival time.
#' @param status Censoring indicator of the survival
#' time of the process; 0 if the total time is censored and 1 otherwise.
#' @param x Categorical variable indicating the population to which
#' the observations belongs.
#' @param kvector A vector specifying the number of groups of curves to be
#'  checking.
#' @param kbin Size of the grid over which the survival functions
#' are to be estimated.
#' @param method  Procedure used for grouping survival curves. Default is \code{"LR"}
#' Possible values are one of:
#' - "LR": Regular Log-Rank test, sensitive to detect late differences.
#' - "GB": Gehan-Breslow (generalized Wilcoxon), detect early differences.
#' - "TW": Tarone-Ware, detect early differences.
#' - "PP": Peto-Peto's modified survival estimate, more robust than
#' Tharone-Ware or Gehan-Breslow, detect early differences
#' - "mPP": modified Peto-Peto (by Andersen)
#' - "FH": Fleming-Harrington (p = 1, q = 1)
#' - "VSM": Villanueva, Sestelo & Meira-Machado bootstrap procedure.
#' @param nboot Number of bootstrap repeats.Only for VSM method.
#' @param algorithm A character string specifying which clustering algorithm is used,
#'  i.e., k-means(\code{"kmeans"}) or k-medians (\code{"kmedians"}).
#' @param alpha Significance level of the testing procedure. Defaults to 0.05.
#' @param cluster A logical value. If  \code{TRUE} (default), the
#'  bootstrap testing procedure is  parallelized. Note that there are cases
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
#' @param multiple.method Correction for multiple comparisons. See Details.
#'
#'@return
#'A list containing the following items:
#'  \item{table}{A data frame containing the null hypothesis tested, the values
#'  of the test statistic and the obtained pvalues.}
#'  \item{levels}{Original levels of the variable \code{x}.}
#'  \item{cluster}{A vector of integers (from 1:k) indicating the cluster to
#'  which each curve is allocated.}
#'  \item{centers}{An object containing the centroids
#'  (mean of the curves pertaining to the same group).}
#'  \item{curves}{An object containing the fitted curves for each population.}
#'
#'@details The adjustment methods include the Bonferroni correction ("bonferroni")
#' in which the p-values are multiplied by the number of comparisons.
#' Less conservative corrections are also included by Holm (1979) ('holm'),
#' Hochberg (1988) ('hochberg'), Hommel (1988) ('hommel'),
#' Benjamini & Hochberg (1995) ('BH' or its alias 'fdr'), and
#' Benjamini & Yekutieli (2001) ('BY'), respectively.
#'
#' This correction is not applied in the VSM method.
#'
#'
#'@author Nora M. Villanueva and Marta Sestelo.
#'
#'@examples
#' library(clustcurv)
#' library(survival)
#' library(condSURV)
#' data(veteran)
#' data(colonCS)
#'
#'# Survival framework
#' res <- survclustcurves(time = veteran$time, status = veteran$status,
#' x = veteran$celltype, algorithm = 'kmeans', nboot = 2)
#'
#' @importFrom survival Surv
#' @importFrom survival survfit
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom parallel parLapply
#' @importFrom parallel stopCluster
#' @importFrom doRNG %dorng% registerDoRNG
#' @importFrom foreach %do% foreach
#' @importFrom foreach %dopar% foreach
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom splines interpSpline
#' @importFrom npregfast frfast
#' @importFrom stats lm predict
#' @export









survclustcurves <- function(time, status = NULL, x,
                           kvector = NULL, kbin = 50, method = "LR",
                           nboot = 100, algorithm = 'kmeans', alpha = 0.05,
                           cluster = FALSE, ncores = NULL, seed = NULL,
                           multiple.method = 'bonferroni'){


  y <- time
  weights <- status
  z <- x


  # Defining error codes
  error.code.0 <- "Argument seed must be an object of type numeric."
 # error.code.1 <- "Argument method must be a string with 'survival' or 'regression'."
  error.code.2 <- "Argument algorithm must be a string with 'kmeans' or 'kmedians'."
  error.code.3 <- "Argument multiple must be an object of type logical."
  error.code.4 <- "Argument multiple.method must be an object of type string."
  error.code.5 <- "Argument multiple.method must be some of the correction methods: 'bonferroni', 'holm', 'hochberg', etc."
  error.code.6 <- "Argument kvector must be an object of type numeric."
  error.code.7 <- "Argument status is missing and it is required."
  error.code.8 <- "Argument status must be a vector of binary numbers."
 # error.code.9 <- "Argument x is missing and it is required when method is 'regression'."
  error.code.10 <- "Argument y is missing and it is required."
 # error.code.11 <- "Argument y is missing and it is required when method is 'regression'."


  # # Checking method  as strings and type
  # if(missing(method)){
  #   stop(error.code.1)
  # }else if (!is.character(method) ) {
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

  # Checking multiple  as logical
  # if (!is.logical(multiple) ) {
  #   stop(error.code.3)
  # }

  # Checking multiple.method as string
  # if (!is.character(multiple.method)  )  {
  #   stop(error.code.4)
  # }else if(nchar(multiple.method) == 0){
  #     stop(error.code.5)
  # }


  # Checking seed as numeric
  if (!is.null(seed)) {
    if(!is.numeric(seed)){
      stop(error.code.0)
    }
    set.seed(seed)
  }

  # Checking kvector
  if(is.null(kvector)) {
    kvector <- c(1:(length(unique(z))-1))
  }else{
    if(!is.numeric(kvector)){
      stop(error.code.6)
    }
  }



  #---------------
  if (isTRUE(cluster)) {
    if (is.null(ncores)) {
      num_cores <- parallel::detectCores() - 1
    }else{
      num_cores <- ncores
    }
    registerDoParallel(cores = num_cores)
    if (!is.null(seed)) registerDoRNG(seed)
    on.exit(stopImplicitCluster())
  }
  #------------------
  time <- y
  status <- weights
  fac <- z

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



      if(missing(weights)) {
        stop(error.code.7)
      }else if(length(unique(weights)) > 2){
          stop(error.code.8)
      }else if(sum(unique(weights))>1 | sum(unique(weights))<0){
          stop(error.code.8)
      }

      if(missing(y)) {
        stop(error.code.10)
      }

    if(method == "VSM"){
    aux[[ii]] <- testing_k(time = time, status = status, fac = fac, k = k,
                     kbin = kbin, nboot = nboot, algorithm = algorithm,
                     seed = seed, cluster = cluster)
    }else{
      aux[[ii]] <- testing_k_LR(time = time, status = status, fac = fac, k = k,
                             kbin = kbin, stat = method, nboot = nboot, algorithm = algorithm,
                             seed = seed, cluster = cluster, correction = multiple.method)
    }
    data <- NULL



    pval[ii] <- aux[[ii]]$pvalue
    tval[ii] <- aux[[ii]]$t
    h0tested[ii] <- k
    # multiple <- FALSE # for bootstrap procedure the correction is not applied
    # if(isTRUE(multiple)){
    #   pval <- p.adjust(pval, method = multiple.method)
    #   ind <- pval >= alpha
    #   if(sum(ind) > 0){
    #     ind_list <- which(ind)[1]
    #     k <- kvector[ind_list]
    #     aux <- aux[[ind_list]]
    #     accept <- 1
    #     break
    #     }
    #   }
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

  ii <- aux$cluster[as.factor(fac)]
  h0 <- survfit(Surv(time, status) ~ ii)
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
              centers = h0, curves = h1, method = method, data = data, algorithm = algorithm,
              call = match.call())
  class(res) <- "clustcurves"
  return(res)

} #end clustcurv



