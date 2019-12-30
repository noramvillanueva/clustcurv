#' @importFrom ggplot2 autoplot
#' @importFrom tidyr gather
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
#' # Regression
#'
#' r2 <- kclustcurv(y = barnacle5$DW, x = barnacle5$RC,
#' z = barnacle5$F, k = 2, method = "regression", algorithm = "kmeans")
#'
#' autoplot(r2)
#' autoplot(r2, groups_by_colour = FALSE)
#' autoplot(r2, centers = TRUE)
#'
#'\donttest{
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
  k <- length(unique(x$cluster))
  #colgr <- wes_palette("Cavalcanti1", k, type = c("continuous"))
  colgr <- brewer.pal(n = k, name = "Dark2")

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

      if(!isTRUE(centers)){
        colnames(x$curves) <- x$levels
        data <- cbind(x = x$grid, x$curves)
        data <- tidyr::gather(data.frame(data), levels, y, 2:dim(data)[2])

        plot1 <- ggplot2::qplot(x, y, data = data, colour = levels, geom = "line")
        ii <- order(x$levels) # for solving the problem of ggplot legend (alphabetic order)
        if (isTRUE(groups_by_colour)){
          plot2 <- plot1 + ggplot2::scale_color_manual(values = colgr[x$cluster][ii])
          plot2
        }else{
          plot1
        }
      }else{

        colnames(x$curves) <- x$levels
        data <- cbind(x = x$grid, x$curves)
        data <- tidyr::gather(data.frame(data), levels, y, 2:dim(data)[2])

        plot1 <- ggplot2::qplot(x, y, data = data, colour = levels, geom = "line")
        ii <- order(x$levels) # for solving the problem of ggplot legend (alphabetic order)
        plot2 <- plot1 + ggplot2::scale_color_manual(values = c(colgr[x$cluster][ii]))

        colnames(x$centers) <- unique(x$cluster)
        data2 <- cbind(x = x$grid, x$centers)

        data2 <- tidyr::gather(data.frame(data2), cluster, y, 2:dim(data2)[2],
                               factor_key = TRUE)
        levels(data2$cluster) <- paste(" G", 1:k, sep = "")
        #data2$cluster <- factor(data2$cluster)

        #levels(data2$cluster) <- c(2,1)
        #data2$cluster <- as.numeric(as.character(data2$cluster))

        #data2 <- data.frame(x = x$grid, y = x$center[,1], cluster = rep(1, length(x$grid)))




       plot2 + ggplot2::geom_line(data = data2,
                                   ggplot2::aes(x = x, y = y, colour = cluster),
                                  size = 0.8) +
          ggplot2::scale_color_manual(values = c( rep(1,k), colgr[x$cluster][ii]))





    }

 }

}



