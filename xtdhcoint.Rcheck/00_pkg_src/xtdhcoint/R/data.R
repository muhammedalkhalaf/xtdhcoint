#' Fisher Effect Panel Dataset
#'
#' @description
#' Simulated panel dataset for testing the Fisher effect (cointegration
#' between inflation and nominal interest rates). The data are generated
#' with cointegration and common factors, suitable for demonstrating the
#' Durbin-Hausman panel cointegration tests.
#'
#' @format A data frame with 1000 observations and 4 variables:
#' \describe{
#'   \item{country}{Character. Country identifier (Country01 through Country20).}
#'   \item{year}{Integer. Year (1970-2019).}
#'   \item{inflation}{Numeric. Annual inflation rate (percent).}
#'   \item{interest}{Numeric. Nominal interest rate (percent).}
#' }
#'
#' @details
#' The data are simulated according to:
#' \deqn{inflation_{it} = \alpha_i + \beta_i \cdot interest_{it} + F_t' \lambda_i + e_{it}}
#' where \eqn{F_t} are common factors, \eqn{\lambda_i} are factor loadings,
#' and \eqn{e_{it}} is a stationary cointegration error (under cointegration).
#'
#' The cointegrating coefficient \eqn{\beta_i} is approximately 1, consistent
#' with the Fisher hypothesis.
#'
#' @source Simulated data.
#'
#' @examples
#' data(fisher_panel)
#' head(fisher_panel)
#' table(fisher_panel$country)
#'
"fisher_panel"
