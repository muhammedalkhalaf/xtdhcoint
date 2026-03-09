#' Print Method for xtdhcoint Objects
#'
#' @description
#' Prints the results of the Durbin-Hausman panel cointegration test.
#'
#' @param x An object of class \code{"xtdhcoint"}.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns the input object.
#'
#' @export
#' @method print xtdhcoint
print.xtdhcoint <- function(x, ...) {

  cat("\n")
  cat(rep("=", 70), "\n", sep = "")
  cat("Westerlund (2008) Durbin-Hausman Panel Cointegration Tests\n")
  cat(rep("=", 70), "\n", sep = "")
  cat("\n")

  cat("H0: No cointegration\n")
  cat("H1(DHg): Cointegration for at least some units\n")
  cat("H1(DHp): Cointegration for all units (common AR parameter)\n")
  cat("\n")

  cat(rep("-", 70), "\n", sep = "")
  cat(sprintf("Panel dimensions:     N = %5d     T = %5d\n", x$N, x$TT))
  cat(sprintf("Number of regressors: K = %5d\n", x$K))
  cat(sprintf("Estimated factors:    F = %5d\n", x$nf))
  cat(sprintf("Kernel bandwidth:     M = %5d\n", x$bandwidth))
  cat(rep("-", 70), "\n", sep = "")
  cat("\n")

  # Averages
  dhg_avg <- x$dhg / x$N
  dhp_avg <- x$dhp / x$N

  # Theoretical means under H0
  mu_g <- 5.5464
  mu_p_inv <- 1 / 0.5005

  cat(rep("-", 70), "\n", sep = "")
  cat(sprintf("%-8s %12s %12s %12s %12s\n",
              "Test", "Avg.Stat", "E(.) H0", "Z-value", "P-value"))
  cat(rep("-", 70), "\n", sep = "")

  cat(sprintf("%-8s %12.4f %12.4f %12.4f %12.4f\n",
              "DHg", dhg_avg, mu_g, x$dhg_z, x$dhg_pval))
  cat(sprintf("%-8s %12.4f %12.4f %12.4f %12.4f\n",
              "DHp", dhp_avg, mu_p_inv, x$dhp_z, x$dhp_pval))

  cat(rep("-", 70), "\n", sep = "")
  cat("\n")

  cat("Avg.Stat = average per-unit statistic (raw sum / N).\n")
  cat("E(.) H0  = expected value under H0. Z = sqrt(N)*(Avg.Stat - E(.))/se.\n")
  cat("\n")

  invisible(x)
}


#' Summary Method for xtdhcoint Objects
#'
#' @description
#' Provides a detailed summary of Durbin-Hausman test results with
#' decision tables at various significance levels.
#'
#' @param object An object of class \code{"xtdhcoint"}.
#' @param level Numeric. Confidence level for hypothesis testing. Default is 0.95.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns a list with summary statistics.
#'
#' @export
#' @method summary xtdhcoint
summary.xtdhcoint <- function(object, level = 0.95, ...) {

  x <- object

  # Print basic results
  print(x)

  # Critical values (right tail of standard normal)
  cv10 <- qnorm(0.90)
  cv05 <- qnorm(0.95)
  cv01 <- qnorm(0.99)

  # Decision functions
  dhg_d10 <- if (x$dhg_z > cv10) "Reject" else "  --  "
  dhg_d05 <- if (x$dhg_z > cv05) "Reject" else "  --  "
  dhg_d01 <- if (x$dhg_z > cv01) "Reject" else "  --  "
  dhp_d10 <- if (x$dhp_z > cv10) "Reject" else "  --  "
  dhp_d05 <- if (x$dhp_z > cv05) "Reject" else "  --  "
  dhp_d01 <- if (x$dhp_z > cv01) "Reject" else "  --  "

  cat("Decision: Z-value vs Critical Values (right tail)\n")
  cat(rep("=", 70), "\n", sep = "")
  cat(sprintf("%-8s %12s %12s %12s %12s\n",
              "Test", "Z-value", "CV 10%", "CV 5%", "CV 1%"))
  cat(sprintf("%-8s %12s %12.4f %12.4f %12.4f\n",
              "", "", cv10, cv05, cv01))
  cat(rep("-", 70), "\n", sep = "")
  cat(sprintf("%-8s %12.4f %12s %12s %12s\n",
              "DHg", x$dhg_z, dhg_d10, dhg_d05, dhg_d01))
  cat(sprintf("%-8s %12.4f %12s %12s %12s\n",
              "DHp", x$dhp_z, dhp_d10, dhp_d05, dhp_d01))
  cat(rep("=", 70), "\n", sep = "")
  cat("\n")

  cat("Reject = Z-value > Critical Value (evidence of cointegration).\n")
  cat("  --   = Fail to reject H0 (no evidence of cointegration).\n")
  cat("\n")
  cat("Reference: Westerlund (2008), J. of Applied Econometrics, 23: 193-233\n")
  cat("\n")

  # Return summary statistics invisibly
  result <- list(
    dhg_z = x$dhg_z,
    dhp_z = x$dhp_z,
    dhg_pval = x$dhg_pval,
    dhp_pval = x$dhp_pval,
    reject_10 = c(DHg = x$dhg_z > cv10, DHp = x$dhp_z > cv10),
    reject_05 = c(DHg = x$dhg_z > cv05, DHp = x$dhp_z > cv05),
    reject_01 = c(DHg = x$dhg_z > cv01, DHp = x$dhp_z > cv01),
    cv = c("10%" = cv10, "5%" = cv05, "1%" = cv01)
  )

  invisible(result)
}


#' Coef Method for xtdhcoint Objects
#'
#' @description
#' Extracts the test statistics from the Durbin-Hausman test.
#'
#' @param object An object of class \code{"xtdhcoint"}.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Named numeric vector of z-statistics and p-values.
#'
#' @export
#' @method coef xtdhcoint
coef.xtdhcoint <- function(object, ...) {
  c(dhg_z = object$dhg_z,
    dhp_z = object$dhp_z,
    dhg_pval = object$dhg_pval,
    dhp_pval = object$dhp_pval)
}
