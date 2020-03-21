#' @importFrom ggplot2 autoplot
#' @importFrom tidyr gather
#' @importFrom data.table rbindlist
#' @import ggfortify
#' @export
ggplot2::autoplot



#' Visualization of \code{clustcurv} objects with ggplot2 graphics
#'
#' @description Useful for drawing the estimated functions grouped by
#' color and the centroids (mean curve of the curves pertaining to the
#' same group).
#'
#' @param object Object of \code{clustcurv} class.
#' @param groups_by_colour A specification for the plotting groups by color.
#' @param centers  Draw the centroids (mean of the curves pertaining to the
#' same group) into the plot. By default it is \code{FALSE}.
#' @param conf.int Only for method = "survival". Logical flag indicating whether to plot confidence intervals.
#' @param censor Only for method = "survival". Logical flag indicating whether to plot censors.
#' @param xlab A title for the \code{x} axis.
#' @param ylab A title for the \code{y} axis.
#' @param \ldots Other options.
#'
#' @details See help page of the function \code{\link{autoplot.survfit}}.
#'
#' @return A ggplot object, so you can use common features from
#' ggplot2 package to manipulate the plot.
#'
#' @author Nora M. Villanueva and Marta Sestelo.
#' @examples
#'
#' library(survival)
#' library(clustcurv)
#' library(condSURV)
#' library(ggplot2)
#' library(ggfortify)
#'
#' # Survival
#'
#' data(veteran)
#' data(colonCS)
#'
#' cl2 <- kclustcurv(y = veteran$time, weights = veteran$status,
#' z = veteran$celltype, k = 2, method = "survival", algorithm = "kmeans")
#'
#' autoplot(cl2)
#' autoplot(cl2, groups_by_colour = FALSE)
#' autoplot(cl2, centers = TRUE)
#'
#'
#'\donttest{
#' # Regression
#'
#' r2 <- kclustcurv(y = barnacle5$DW, x = barnacle5$RC,
#' z = barnacle5$F, k = 2, method = "regression", algorithm = "kmeans")
#'
#' autoplot(r2)
#' autoplot(r2, groups_by_colour = FALSE)
#' autoplot(r2, centers = TRUE)
#'
#'
#' colonCSm <- data.frame(time = colonCS$Stime, status = colonCS$event,
#'                       nodes = colonCS$nodes)
#'
#' table(colonCSm$nodes)
#' colonCSm$nodes[colonCSm$nodes == 0] <- NA
#' colonCSm <- na.omit(colonCSm)
#' colonCSm$nodes[colonCSm$nodes >= 10] <- 10
#' table(colonCSm$nodes) # ten levels
#'
#' res <- autoclustcurv(y = colonCSm$time, weights = colonCSm$status,
#'        z = colonCSm$nodes, method = "survival", algorithm = "kmeans",
#'        nboot = 20)
#'
#' autoplot(res)
#' autoplot(res, groups_by_colour = FALSE)
#' autoplot(res, centers = TRUE)
#' }
#' @importFrom wesanderson wes_palette
#' @importFrom RColorBrewer brewer.pal
#' @export

autoplot.clustcurv <- function(object = object, groups_by_colour = TRUE,
          centers = FALSE, conf.int = FALSE, censor = FALSE,
          xlab = "Time", ylab = "Survival", ...){

  x <- object
  y <- c()
  k <- length(unique(x$cluster))

  if(k < 3){
    colgr <- brewer.pal(n = 3, name = "Dark2")
  }else{
    colgr <- brewer.pal(n = k, name = "Dark2")
    }


  if(x$method == "survival"){

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


      plot2 <- plot1 + ggplot2::geom_step(data = data,
                        ggplot2::aes_string(x = "t", y = "surv", group = "cen"), size = 1)
      plot2

  }

  }else{ #method regression

      if(isFALSE(centers)){

        data <- x$data
        data <- data[order(data$f),]
        data$f <- as.factor(data$f)
        data$y <- unlist(x$curves)
        names(data) <- c("x", "y", "levels")

        plot1 <- ggplot2::qplot(x, y, data = data, colour = levels, geom = "line")
        ii <- order(x$levels) # for solving the problem of ggplot legend (alphabetic order)
        if (isTRUE(groups_by_colour)){
          plot2 <- plot1 + ggplot2::scale_color_manual(values = colgr[x$cluster])
          plot2
        }else{
          plot1
        }
      }else{

        data <- x$data
        data <- data[order(data$f),]
        data$f <- as.factor(data$f)
        data$y <- unlist(x$curves)
        names(data) <- c("x", "y", "levels")

        data2 <- x$data
        data2$f <- x$levels[x$cluster[data2$f]]
        data2 <- data2[order(data2$f),]
        data2$f <- as.factor(data2$f)
        data2$y <- unlist(x$centers)
        names(data2) <- c("x", "y", "levels")
        levels(data2$levels) <- paste(" G", 1:k, sep = "")

        dat <- rbind(data, data2)


        plot1 <- ggplot2::qplot(x, y, data = dat, colour = levels, geom = "line")
        ii <- order(x$levels) # for solving the problem of ggplot legend (alphabetic order)
        plot2 <- plot1 + ggplot2::scale_color_manual(values = c(colgr[x$cluster], rep(1,k)))
        plot2

    }

 }

}



