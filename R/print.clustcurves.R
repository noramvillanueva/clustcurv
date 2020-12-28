#' @export
print.clustcurves <- function(x = model, ...) {

  if (inherits(x, "clustcurves")) {
    model <- x
    cat("\nCall: ","\n")
    print(model$call)
    cat("\nClustering curves in ", length(unique(model$cluster)),
        " groups", "\n", sep = "")
    if(model$method == "survival"){
      cat("\nNumber of observations: ",length(model$centers$time))
    }else{
      cat("\nNumber of observations: ",dim(model$data)[1])
    }
    cat("\nCluster method: ", model$algorithm)
  }else{
    stop("Argument x must be either 'clustcurves' object.")
  }
}
