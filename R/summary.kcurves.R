#' Summarizing fits of \code{kcurves} class produced by \code{ksurvcurves} and
#' \code{kregcurves}
#'
#' @description Takes a  kcurves object
#' and produces various useful summaries from it.
#'
#' @param object a kcurves object as producted by \code{ksurvcurves} and
#' \code{kregcurves}
#' @param \ldots additional arguments.
#'
#' @details \code{print.kcurves} tries to be smart about \code{summary.kcurves}.
#'
#' @return \code{summary.kcurves} computes and returns a list of summary
#' information for a \code{kcurves} object.
#' \item{levels}{Levels of the factor.}
#' \item{cluster}{A vector containing the assignment of each factor's level to its group.}
#'
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
#' summary(s2)
#'
#'
#' # Regression: 2 groups k-means
#' r2 <- kregcurves(y = barnacle5$DW, x = barnacle5$RC,
#' z = barnacle5$F, k = 2, algorithm = "kmeans")
#'
#' summary(r2)
#'
#'
#'
#' @export



summary.kcurves <- function(object, ...){
  cat("\nCall: ","\n")
  print(object$call)
  cat("\nClustering curves in ", length(unique(object$cluster)),
      " groups", "\n", sep = "")
  if(object$method == "survival"){
    cat("\nNumber of observations: ",length(object$centers$time)) #cambiar a lenght(object$curves$time)
  }else{
    cat("\nNumber of observations: ",dim(object$data)[1])
  }
  cat("\nCluster method: ", object$algorithm, "\n")
  cat("\nFactor's levels:\n")
  print(object$levels, ...)
  cat("\nClustering factor's levels:\n")
  print(object$cluster, ...)


  cat("\nAvailable components:", sep = "\n")
  print(names(object))
  invisible(object)
}




