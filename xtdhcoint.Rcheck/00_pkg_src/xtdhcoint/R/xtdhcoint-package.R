#' @keywords internal
"_PACKAGE"

#' @name xtdhcoint-package
#' @aliases xtdhcoint-package
#'
#' @title Durbin-Hausman Panel Cointegration Tests
#'
#' @description
#' Implements the Durbin-Hausman panel cointegration tests of
#' Westerlund (2008). The tests are robust to cross-sectional dependence
#' through common factor extraction using principal components.
#'
#' @details
#' The package provides the \code{\link{xtdhcoint}} function for performing
#' panel cointegration tests that account for cross-sectional dependence.
#'
#' The main features include:
#' \itemize{
#'   \item DHg (group-mean) statistic for heterogeneous cointegration
#'   \item DHp (panel) statistic for homogeneous cointegration
#'   \item Automatic factor number selection via information criteria
#'   \item Long-run variance estimation using Bartlett kernel
#' }
#'
#' @references
#' Westerlund, J. (2008). Panel cointegration tests of the Fisher effect.
#' \emph{Journal of Applied Econometrics}, 23(2), 193--233.
#' \doi{10.1002/jae.963}
#'
#' @importFrom stats model.frame model.matrix model.response pnorm qnorm
#' @importFrom stats arima.sim rnorm
#'
NULL
