 #' @importFrom ggplot2 autoplot
 #' @import ggfortify
 #' @export
 ggplot2::autoplot


#' Visualization of \code{clustcurv_surv} objects with ggplot2 graphics
#'
#' @description Useful for drawing the estimated survival functions grouped by
#' color and the centroids (mean curve of the curves pertaining to the
#' same group).
#'
#' @param x Object of \code{clustcurv_surv} class.
#' @param groups_by_colour jsjhd
#' @param centers  Draw the centroids (mean of the curves pertaining to the
#' same group) into the plot. By default it is \code{FALSE}.
#' @param conf.int Logical flag indicating whether to plot confidence intervals.
#' @param censor Logical flag indicating whether to plot censors.
#' @param xlab A title for the \code{x} axis.
#' @param ylab A title for the \code{y} axis.
#' @param \ldots Other options.
#'
#' @details See help page of the function \code{\link{autoplot.survfit}}.
#'
#'@return A ggplot object, so you can use common features from
#' ggplot2 package to manipulate the plot.
#'
#'@author Nora M. Villanueva and Marta Sestelo.
#'
#' @examples
#'
#' library(survival)
#' data(veteran)
#'
#' cl2 <- kgroups_surv(time = veteran$time, status = veteran$status,
#' fac = veteran$celltype, k = 2, algorithm = "kmeans", nboot = 20)
#'
#' autoplot(cl2)
#' autoplot(cl2, groups_by_colour = FALSE)
#' autoplot(cl2, centers = TRUE)
#'
#' res <- clustcurv_surv(colonCSm$time, status = colonCSm$status,
#' fac = colonCSm$nodes, algorithm = "kmeans", nboot = 20)
#'
#' autoplot(res)
#' autoplot(res, groups_by_colour = FALSE)
#' autoplot(res, centers = TRUE)
#'
#' @export


autoplot.clustcurv_surv <- function(x = object, groups_by_colour = TRUE,
                              centers = FALSE, conf.int = FALSE, censor = FALSE,
                              xlab = "Time", ylab = "Survival",
                              ...){

  k <- length(x$centers$strata)
 # colnm <- wesanderson::wes_palette("Zissou", length(x$levels), type = c("continuous"))
  colgr <- wesanderson::wes_palette("Zissou", k, type = c("continuous"))

  if(!isTRUE(centers)){

  names(x$curves$strata) <- x$levels
  plot1 <- autoplot(x$curves, conf.int = conf.int, censor = censor, xlab = xlab,
                    ylab = ylab, ...)

  if (isTRUE(groups_by_colour)){
    plot2 <- plot1 + ggplot2::scale_color_manual(values = colgr[x$cluster])
    plot2
  }else{
    plot1
  }

  }else{
    #names(x$curves$strata) <- x$levels

      data <- data.frame(t = x$centers$time,
                         surv = x$centers$surv,
                         cen = factor(unlist(sapply(1:k, function(x, y){rep(x, y[x])},
                                                  y = x$centers$strata))))



      plot1 <- autoplot(x$curves, conf.int = conf.int, censor = censor, xlab = xlab,
                        ylab = ylab, ...) + ggplot2::scale_color_manual(values = colgr[x$cluster])


      plot1 + ggplot2::geom_step(data = data,
                        ggplot2::aes_string(x = "t", y = "surv", group = "cen"), size = 1)

  }
}


