#' \code{clustcurv}: Determining Groups in Multiple Curves.
#'
#'
#' This package provides a method for determining groups in multiple survival
#' curves with an automatic selection of their number based on k-means or
#' k-medians algorithms. The selection of the optimal number is provided by
#' bootstrap methods.
#'
#'
#' @name clustcurv
#' @docType package
#' @details \tabular{ll}{ Package: \tab clustcurv \cr Type: \tab Package\cr
#'  License: \tab MIT + file LICENSE\cr}
#'
#'\code{clustcurv} is designed along lines similar to those of other \code{R}
#' packages. This software helps the user determine groups in multiple curves
#' (survival curves in the current version). In addition, it enables both numerical
#' and graphical outputs to be displayed (by means og ggplot2). The package provides
#' the  \code{kgroups_surv()} function that groups the curves given a number k and
#' the \code{clustcurv_surv()} function that selects the optimal number of groups
#'  automatically through a boostrap-based test. The \code{autoplot()} function
#'  let the user draws the resulted estimated curves coloured by groups.
#'
#'
#' For a listing of all routines in the clustcurv package type:
#' \code{library(help="clustcurv")}.
#'
#'
#' @author Nora M. Villanueva and Marta Sestelo
#'
#' @references
#'
#'   Villanueva, N. M., Sestelo, M., and  Meira-Machado, J. (2019). A method for
#'    determining groups in multiple survival curves. Statistics in Medicine,
#'    8(5):866-877

"_PACKAGE"
