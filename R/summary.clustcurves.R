#' Summarizing fits of \code{kclustcurves} class produced by \code{survclustcurves} and
#' \code{regclustcurves}
#'
#' @description Takes a  clustcurves object
#' and produces various useful summaries from it.
#'
#' @param object a clustcurves object as producted by \code{survclustcurves} and
#' \code{regclustcurves}
#' @param \ldots additional arguments.
#'
#' @details \code{print.clustcurves} tries to be smart about \code{summary.clustcurves}.
#'
#' @return \code{summary.clustcurves} computes and returns a list of summary
#' information for a \code{clustcurves} object.
#' \item{levels}{Levels of the factor.}
#' \item{cluster}{A vector containing the assignment of each factor's level to its group.}
#' \item{table}{A data.frame containing the results from the hypothesis test.}
#'@author Nora M. Villanueva and Marta Sestelo.
#'
#'@examples
#' library(clustcurv)
#' library(survival)
#' data(veteran)
#'
#'# Survival framework
#' ressurv <- survclustcurves(time = veteran$time, status = veteran$status,
#' x = veteran$celltype, algorithm = 'kmeans', nboot = 2)
#'
#' summary(ressurv)
#'
#'
#'# Regression framework
#' resreg <- regclustcurves(y = barnacle5$DW, x = barnacle5$RC, z = barnacle5$F,
#' algorithm = 'kmeans', nboot = 2)
#'
#' summary(resreg)
#'
#'
#'
#' @export



summary.clustcurves <- function(object, ...){
  cat("\nCall: ","\n")
  print(object$call)
  cat("\nClustering curves in ", object$num_groups,
      " groups", "\n", sep = "")
  if(object$method == "survival"){
    cat("\nNumber of observations: ",length(object$centers$time))
  }else{
    cat("\nNumber of observations: ",dim(object$data)[1])
  }
  cat("\nCluster method: ", object$algorithm, "\n")
  cat("\nFactor's levels:\n")
  print(object$levels, ...)
  cat("\nClustering factor's levels:\n")
  print(object$cluster, ...)
  cat("\nTesting procedure:\n")
  print(object$table, ...)

  cat("\nAvailable components:", sep = "\n")
  print(names(object))
  invisible(object)
}




