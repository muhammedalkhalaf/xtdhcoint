#' Durbin-Hausman Panel Cointegration Tests
#'
#' @description
#' Performs the Durbin-Hausman panel cointegration tests of Westerlund (2008).
#' The tests are robust to cross-sectional dependence through common factor
#' extraction using principal components.
#'
#' @param formula A formula specifying the cointegrating relationship
#'   (e.g., \code{y ~ x1 + x2}).
#' @param data A data frame containing panel data.
#' @param id Character string naming the cross-sectional unit identifier.
#' @param time Character string naming the time period identifier.
#' @param kmax Integer. Maximum number of common factors to consider.
#'   Default is 5.
#' @param criterion Character string specifying the information criterion for
#'   factor number selection. One of \code{"ic"} (default), \code{"pc"},
#'   \code{"aic"}, or \code{"bic"}.
#' @param penalty Integer. Penalty type for factor selection (1, 2, or 3).
#'   Default is 1.
#' @param bandwidth Integer. Kernel bandwidth for long-run variance estimation.
#'   If -1 (default), automatic bandwidth selection using Newey-West rule.
#' @param predet Numeric. If non-zero, uses predetermined coefficient instead
#'   of OLS estimation. Default is 0 (estimate via OLS).
#' @param constant Logical. If TRUE (default), includes a constant term.
#'
#' @return An object of class \code{"xtdhcoint"} containing:
#' \describe{
#'   \item{dhg}{Raw DHg (group-mean) statistic.}
#'   \item{dhp}{Raw DHp (panel) statistic.}
#'   \item{dhg_z}{Standardized DHg z-statistic.}
#'   \item{dhp_z}{Standardized DHp z-statistic.}
#'   \item{dhg_pval}{P-value for DHg (right-tail).}
#'   \item{dhp_pval}{P-value for DHp (right-tail).}
#'   \item{nf}{Estimated number of common factors.}
#'   \item{N}{Number of cross-sectional units.}
#'   \item{TT}{Number of time periods.}
#'   \item{bandwidth}{Bandwidth used.}
#'   \item{criterion}{Information criterion used.}
#'   \item{call}{The matched call.}
#' }
#'
#' @details
#' The Durbin-Hausman tests examine the null hypothesis of no cointegration
#' against the alternative of cointegration. The tests are based on comparing
#' OLS and instrumental variable estimators of the autoregressive parameter
#' in the cumulated residuals.
#'
#' \strong{DHg (Group-mean statistic):}
#' Tests the null of no cointegration against the heterogeneous alternative
#' that at least some units are cointegrated. Under the null, the standardized
#' statistic is asymptotically N(0,1).
#'
#' \strong{DHp (Panel statistic):}
#' Tests the null of no cointegration against the homogeneous alternative
#' that all units are cointegrated with a common autoregressive parameter.
#' Under the null, the standardized statistic is asymptotically N(0,1).
#'
#' Both statistics are computed using right-tail critical values. Large positive
#' values indicate evidence of cointegration.
#'
#' Cross-sectional dependence is handled by extracting common factors from
#' the first-differenced residuals using principal components. The number
#' of factors is selected automatically using information criteria.
#'
#' @references
#' Westerlund, J. (2008). Panel cointegration tests of the Fisher effect.
#' \emph{Journal of Applied Econometrics}, 23(2), 193--233.
#' \doi{10.1002/jae.963}
#'
#' @examples
#' # Load example data
#' data(fisher_panel)
#'
#' \donttest{
#' # Basic test with default settings
#' result <- xtdhcoint(inflation ~ interest, data = fisher_panel,
#'                     id = "country", time = "year")
#' print(result)
#' summary(result)
#' }
#'
#' @export
xtdhcoint <- function(formula,
                      data,
                      id,
                      time,
                      kmax = 5,
                      criterion = c("ic", "pc", "aic", "bic"),
                      penalty = 1,
                      bandwidth = -1,
                      predet = 0,
                      constant = TRUE) {

  # Match call

call <- match.call()

  # Validate criterion
  criterion <- match.arg(criterion)

  # Map criterion to code (matches GAUSS fact() parameter c)
  cri <- switch(criterion,
                "pc" = 1,
                "ic" = 2,
                "aic" = 3,
                "bic" = 3)

  # BIC uses penalty type 2
  if (criterion == "bic") penalty <- 2

  # Validate penalty
  if (!penalty %in% 1:3) {
    stop("penalty must be 1, 2, or 3", call. = FALSE)
  }

  # Parse formula
  mf <- model.frame(formula, data = data)
  depvar <- model.response(mf)
  indepvars <- model.matrix(formula, data = mf)

  # Remove intercept if noconstant
  if (!constant && "(Intercept)" %in% colnames(indepvars)) {
    indepvars <- indepvars[, colnames(indepvars) != "(Intercept)", drop = FALSE]
  } else if (constant && "(Intercept)" %in% colnames(indepvars)) {
    # Remove intercept from model matrix (handled separately in first-diff)
    indepvars <- indepvars[, colnames(indepvars) != "(Intercept)", drop = FALSE]
  }

  K <- ncol(indepvars)
  if (K < 1) {
    stop("At least one regressor required", call. = FALSE)
  }

  # Get panel identifiers
  if (!id %in% names(data) || !time %in% names(data)) {
    stop("id and time variables must be columns in data", call. = FALSE)
  }

  panel_id <- data[[id]]
  time_id <- data[[time]]

  # Sort data by panel and time
  ord <- order(panel_id, time_id)
  depvar <- depvar[ord]
  indepvars <- indepvars[ord, , drop = FALSE]
  panel_id <- panel_id[ord]
  time_id <- time_id[ord]

  # Get panel dimensions
  units <- unique(panel_id)
  N <- length(units)

  if (N < 2) {
    stop("At least 2 cross-sectional units required", call. = FALSE)
  }

  # Count observations per unit
  obs_per_unit <- table(panel_id)
  Tmin <- min(obs_per_unit)
  Tmax <- max(obs_per_unit)

  if (Tmin != Tmax) {
    message("Note: Unbalanced panel. Truncating each unit to T = ", Tmin)
  }

  TT <- Tmin

  if (TT < 10) {
    stop("At least 10 time periods required", call. = FALSE)
  }

  # Automatic bandwidth (Newey-West)
  if (bandwidth < 0) {
    bandwidth <- floor(4 * (TT / 100)^(2/9))
  }
  if (bandwidth < 1) bandwidth <- 1

  # Validate kmax
  if (kmax < 1) {
    stop("kmax must be at least 1", call. = FALSE)
  }
  if (kmax >= min(N, TT)) {
    kmax <- min(N, TT) - 1
    message("Note: kmax adjusted to ", kmax)
  }

  # Create balanced panel (keep first Tmin obs per unit)
  keep_idx <- logical(length(depvar))
  for (u in units) {
    unit_idx <- which(panel_id == u)
    if (length(unit_idx) >= TT) {
      keep_idx[unit_idx[1:TT]] <- TRUE
    }
  }

  depvar <- depvar[keep_idx]
  indepvars <- indepvars[keep_idx, , drop = FALSE]
  panel_id <- panel_id[keep_idx]

  # Reshape to wide format (T x N matrix for Y)
  Y_wide <- matrix(depvar, nrow = TT, ncol = N)

  # Reshape X to list of T x K matrices (one per unit)
  X_list <- vector("list", N)
  for (i in seq_len(N)) {
    start_idx <- (i - 1) * TT + 1
    end_idx <- i * TT
    X_list[[i]] <- indepvars[start_idx:end_idx, , drop = FALSE]
  }

  # Compute cumulated defactored residuals
  cum_result <- xtdh_cum(Y_wide, X_list, N, TT, K, kmax, penalty, cri, predet)
  e <- cum_result$e
  nf <- cum_result$nf

  # Compute DHg = sum of individual DH statistics
  dhg_raw <- 0
  for (i in seq_len(N)) {
    dhg_raw <- dhg_raw + xtdh_gdh(e[, i], bandwidth)
  }

  # Compute DHp = panel DH statistic
  dhp_raw <- xtdh_pdh(e, bandwidth)

  # Simulated moments from Westerlund (2008) Remark 2, p.204
  mu_g <- 5.5464   # E(Bi)
  var_g <- 36.7673 # var(Bi)
  mu_p <- 0.5005   # E(Ci)
  var_p <- 0.3348  # var(Ci)

  # Standardize
  dhg_z <- sqrt(N) * (dhg_raw / N - mu_g) / sqrt(var_g)
  dhp_z <- sqrt(N) * (dhp_raw / N - mu_p^(-1)) / sqrt(mu_p^(-4) * var_p)

  # P-values (right tail of standard normal)
  dhg_pval <- 1 - pnorm(dhg_z)
  dhp_pval <- 1 - pnorm(dhp_z)

  # Issue warning for multiple regressors
  if (K > 1) {
    message("Warning: Simulated moments are derived for K=1 (one regressor).\n",
            "With K=", K, ", z-values may not follow N(0,1) under H0.\n",
            "Consider running separate bivariate tests.")
  }

  # Create result object
  result <- list(
    dhg = dhg_raw,
    dhp = dhp_raw,
    dhg_z = dhg_z,
    dhp_z = dhp_z,
    dhg_pval = dhg_pval,
    dhp_pval = dhp_pval,
    nf = nf,
    N = N,
    TT = TT,
    K = K,
    bandwidth = bandwidth,
    kmax = kmax,
    criterion = criterion,
    penalty = penalty,
    call = call
  )

  class(result) <- "xtdhcoint"
  return(result)
}


#' Cumulated Defactored Residuals
#'
#' @description
#' Computes cumulated defactored residuals for the Durbin-Hausman test.
#' Replicates GAUSS cum() lines 10-38.
#'
#' @param Y_wide T x N matrix of dependent variable.
#' @param X_list List of N matrices, each T x K, containing regressors.
#' @param N Number of cross-sectional units.
#' @param TT Number of time periods.
#' @param K Number of regressors.
#' @param kmax Maximum number of factors.
#' @param penalty Penalty type.
#' @param cri Criterion code.
#' @param predet Predetermined coefficient.
#'
#' @return List with elements:
#' \describe{
#'   \item{e}{T x N matrix of cumulated defactored residuals.}
#'   \item{nf}{Estimated number of factors.}
#' }
#'
#' @keywords internal
#' @noRd
xtdh_cum <- function(Y_wide, X_list, N, TT, K, kmax, penalty, cri, predet) {

  # Initialize first-differenced residuals matrix
  de <- matrix(0, nrow = TT, ncol = N)

  for (i in seq_len(N)) {
    yi_level <- Y_wide[, i]
    xi_level <- X_list[[i]]

    # GAUSS zero-padding + differencing (lines 19-22):
    # Prepend zero row, then take first differences
    # dy[1] = y[1] - 0 = y[1], dy[2] = y[2]-y[1], ...
    dyi <- yi_level - c(0, yi_level[1:(TT - 1)])
    dxi <- xi_level - rbind(rep(0, K), xi_level[1:(TT - 1), , drop = FALSE])

    # Compute residuals
    if (predet != 0) {
      # Predetermined coefficient: de = dy - predet*dx
      de[, i] <- dyi - dxi %*% rep(predet, K)
    } else {
      # OLS projection: de = (I - X(X'X)^{-1}X') * dy
      XtX_inv <- solve(crossprod(dxi))
      de[, i] <- dyi - dxi %*% (XtX_inv %*% crossprod(dxi, dyi))
    }
  }

  # Select number of factors
  nf <- xtdh_fact(de, kmax, penalty, cri)

  # Extract factors
  prin_result <- xtdh_prin(de, nf)
  F_mat <- prin_result$F_mat
  Lambda <- prin_result$Lambda

  # Defactor residuals
  de <- de - F_mat %*% t(Lambda)

  # Cumulative sum (column-by-column)
  e <- apply(de, 2, cumsum)

  return(list(e = e, nf = nf))
}


#' Individual DH Statistic
#'
#' @description
#' Computes the individual Durbin-Hausman statistic for one unit.
#' Replicates GAUSS gdh() lines 40-63.
#'
#' @param w Numeric vector. Cumulated residuals for one unit.
#' @param p Integer. Bandwidth for kernel estimation.
#'
#' @return Numeric scalar. The DH statistic for this unit.
#'
#' @keywords internal
#' @noRd
xtdh_gdh <- function(w, p) {

  t_len <- length(w)

  # Lags and leads
  wl <- w[1:(t_len - 1)]
  w0 <- w[2:t_len]

  # OLS estimator: phi_hat
  b1 <- sum(wl * w0) / sum(wl^2)

  # IV estimator: phi_tilde
  b2 <- sum(w0^2) / sum(wl * w0)

  # OLS residuals
  resid <- w0 - wl * b1

  # sigma^2
  s <- sum(resid^2) / length(resid)

  # One-sided long-run covariance
  if (p == 0) {
    io <- 0
  } else {
    io <- xtdh_fejer(resid, p)
  }

  # omega^2 = sigma^2 + 2*io
  sig <- s + 2 * io

  # sigma^4 / omega^2
  s <- s^2 / sig

  # DH statistic
  dhs <- (b2 - b1)^2 / (s * (1 / sum(wl^2)))

  return(dhs)
}


#' Panel DH Statistic
#'
#' @description
#' Computes the panel Durbin-Hausman statistic.
#' Replicates GAUSS pdh() lines 65-102.
#'
#' @param w T x N matrix of cumulated residuals.
#' @param p Integer. Bandwidth for kernel estimation.
#'
#' @return Numeric scalar. The panel DH statistic.
#'
#' @keywords internal
#' @noRd
xtdh_pdh <- function(w, p) {

  t_len <- nrow(w)
  n <- ncol(w)

  # Create lags
  wl <- w[1:(t_len - 1), , drop = FALSE]
  w0 <- w[2:t_len, , drop = FALSE]

  # Pool via vectorization (column stacking)
  wl_pool <- as.vector(wl)
  w0_pool <- as.vector(w0)

  # Pooled OLS: phi_hat
  b1 <- sum(wl_pool * w0_pool) / sum(wl_pool^2)

  # Pooled IV: phi_tilde
  b2 <- sum(w0_pool^2) / sum(wl_pool * w0_pool)

  # Reshape residuals to (t-1) x n
  e_vec <- w0_pool - wl_pool * b1
  e_reshaped <- matrix(e_vec, nrow = t_len - 1, ncol = n)

  # Compute variance components
  snt <- 0  # sum of sigma_i^2
  lnt <- 0  # sum of omega_i^2

  for (i in seq_len(n)) {
    resid_i <- e_reshaped[, i]

    # sigma_i^2
    s <- sum(resid_i^2) / length(resid_i)

    # One-sided long-run covariance
    if (p == 0) {
      io <- 0
    } else {
      io <- xtdh_fejer(resid_i, p)
    }

    snt <- snt + s                # sum sigma_i^2
    lnt <- lnt + (s + 2 * io)     # sum omega_i^2
  }

  # Panel DH statistic
  dhs <- ((lnt / n) * (b2 - b1)^2) / (((snt / n)^2) * (1 / sum(wl_pool^2)))

  return(dhs)
}


#' Factor Number Selection
#'
#' @description
#' Selects the number of common factors using information criteria.
#' Replicates GAUSS fact() lines 104-158.
#'
#' @param e T x N matrix of first-differenced residuals.
#' @param kmax Maximum number of factors.
#' @param p Penalty type (1, 2, or 3).
#' @param c Criterion code (1=PC, 2=IC, 3=AIC/BIC).
#'
#' @return Integer. Estimated number of factors.
#'
#' @keywords internal
#' @noRd
xtdh_fact <- function(e, kmax, p, c) {

  TT <- nrow(e)
  N <- ncol(e)

  # Compute maximum variance
  prin_result <- xtdh_prin(e, kmax)
  u <- e - prin_result$F_mat %*% t(prin_result$Lambda)
  smax <- sum(u^2) / (N * TT)

  cr <- numeric(kmax)

  for (k in seq_len(kmax)) {
    prin_result <- xtdh_prin(e, k)
    u <- e - prin_result$F_mat %*% t(prin_result$Lambda)
    s <- sum(u^2) / (N * TT)

    # Penalty selection
    if (c == 1 || c == 2) {
      if (p == 1) {
        pen_val <- (N + TT) / (N * TT) * log((N * TT) / (N + TT))
      } else if (p == 2) {
        pen_val <- (N + TT) / (N * TT) * log(min(N, TT))
      } else {
        pen_val <- log(min(N, TT)) / min(N, TT)
      }
    } else {  # c == 3 (AIC/BIC)
      if (p == 1) {
        pen_val <- 2 * (N + TT - k) / (N * TT)
      } else {
        pen_val <- (N + TT - k) * log(N * TT) / (N * TT)
      }
    }

    # Criterion value
    if (c == 1) {
      cr[k] <- s + k * smax * pen_val    # PC
    } else if (c == 2) {
      cr[k] <- log(s) + k * pen_val      # IC
    } else {
      cr[k] <- s + k * smax * pen_val    # AIC/BIC
    }
  }

  # Return k that minimizes criterion
  return(which.min(cr))
}


#' Principal Components Estimation
#'
#' @description
#' Extracts principal components from a matrix.
#' Replicates GAUSS prin() lines 160-178.
#'
#' @param e T x N matrix.
#' @param nf Number of factors to extract.
#'
#' @return List with elements:
#' \describe{
#'   \item{F_mat}{T x nf matrix of factor estimates.}
#'   \item{Lambda}{N x nf matrix of loadings.}
#' }
#'
#' @keywords internal
#' @noRd
xtdh_prin <- function(e, nf) {

  TT <- nrow(e)
  N <- ncol(e)

  if (N > TT) {
    # SVD of e*e' (T x T matrix)
    svd_result <- svd(tcrossprod(e), nu = nf, nv = 0)
    F_mat <- svd_result$u[, 1:nf, drop = FALSE] * sqrt(TT)
    Lambda <- (t(e) %*% F_mat) / TT
  } else {
    # SVD of e'e (N x N matrix)
    svd_result <- svd(crossprod(e), nu = nf, nv = 0)
    Lambda <- svd_result$u[, 1:nf, drop = FALSE] * sqrt(N)
    F_mat <- (e %*% Lambda) / N
  }

  return(list(F_mat = F_mat, Lambda = Lambda))
}


#' Fejer (Bartlett) Kernel
#'
#' @description
#' Computes one-sided long-run covariance using Bartlett kernel.
#'
#' @param e Numeric vector of residuals.
#' @param M Integer. Bandwidth.
#'
#' @return Numeric scalar. One-sided long-run covariance.
#'
#' @keywords internal
#' @noRd
xtdh_fejer <- function(e, M) {

  TT <- length(e)
  io <- 0

  for (j in seq_len(M)) {
    w <- 1 - j / (M + 1)  # Bartlett weight
    gam_j <- sum(e[1:(TT - j)] * e[(j + 1):TT]) / TT
    io <- io + w * gam_j
  }

  return(io)
}
