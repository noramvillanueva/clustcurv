#' @export
print.clustcurves <- function(x = model, ...) {

  if (inherits(x, "clustcurves")) {
    model <- x
    cat("\nOriginal data: ","\n")
    #  cat("\n------------------------ ","\n")
    #  print(head(model$data))
    #  cat("\n")
    cat("\nNumber of observations: ",dim(model$data)[1])
    cat("\nNumber of variables: ", dim(model$data)[2])
    cat("\n")
    cat("\n")
    cat("\nOutput: ","\n")
    print(head(model)[1:4])

    cat("\nAvailable components:", sep = "\n")
    print(names(model))


  }else{
    stop("Argument x must be either 'clustcurves' object.")
  }
}
