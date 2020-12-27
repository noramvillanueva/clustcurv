#' @export
print.clustcurves <- function(x = model, ...) {

  if (inherits(x, "clustcurves")) {
    model <- x
    cat("\nCall: ","\n")
    print(model$call)
    cat("\nClustering curves in ", length(unique(object$cluster)),
        " groups", "\n", sep = "")
    cat("\nNumber of observations: ",dim(model$data)[1])
    cat("\nNumber of variables: ", dim(model$data)[2])
    cat("\nCluster method: ", model$algorithm)
  }else{
    stop("Argument x must be either 'clustcurves' object.")
  }
}
