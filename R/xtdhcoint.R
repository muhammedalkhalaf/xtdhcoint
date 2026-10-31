#' Durbin-Hausman Panel Cointegration Tests
#'
#' @description
#' Performs the Durbin-Hausman panel cointegration tests of Westerlund (2008).
#' The tests are robust to cross-sectional dependence through common factor
#' extraction using principal components.
#'
#' @param formula A formula specifying the cointegrating relationship
#'   (e.g., \code{y ~ x1 + x2}). The intercept is never estimated: the
#'   unit-specific constant of the levels model vanishes when the data are
#'   first-differenced (Westerlund, 2008, eq. 12).
#' @param data A data frame containing panel data. Missing values in the
#'   response, the regressors, \code{id} or \code{time} are not allowed; the
#'   function stops and reports their count and row positions.
#' @param id Character string naming the cross-sectional unit identifier.
#' @param time Character string naming the time period identifier. Units are
#'   aligned on this variable (see Details, "Unbalanced panels").
#' @param kmax Integer. Maximum number of common factors to consider in the
#'   search \eqn{k = 0, 1, \ldots, kmax}. Default is 5. Values of at least
#'   \eqn{\min(N, T - 1)} are reduced to \eqn{\min(N, T - 1) - 1} with a
#'   message.
#' @param nfactors Integer or \code{NULL}. If supplied, the number of common
#'   factors is fixed at this value (0 means no defactoring) and the
#'   information criterion is not used. Useful when \code{N} is small (see
#'   Details).
#' @param criterion Character string specifying the information criterion for
#'   factor number selection. One of \code{"ic"} (default, the IC criteria of
#'   Bai and Ng, 2002, eq. 9), \code{"pc"} (their PC criteria), \code{"aic"}
#'   (their AIC3) or \code{"bic"} (their BIC3).
#' @param penalty Integer 1, 2 or 3 selecting the penalty function
#'   \eqn{g(N, T)} of the \code{"ic"} and \code{"pc"} criteria (the default,
#'   \code{NULL}, means 1). Bai and Ng (2002) define a single AIC3 and a single
#'   BIC3, so \code{penalty} must be \code{NULL} when \code{criterion} is
#'   \code{"aic"} or \code{"bic"}; any other combination is an error.
#' @param bandwidth Integer. Bartlett kernel bandwidth \eqn{M} for the
#'   long-run variance estimator. If -1 (default) the rule of Westerlund (2008,
#'   Section 3.4) is used: \eqn{M = \lfloor 4 (T/100)^{2/9} \rfloor}, the same
#'   for every unit, where \eqn{T} is the number of time periods in the
#'   analysed window. This is a fixed function of \eqn{T}, not the
#'   data-dependent Newey and West (1994) procedure. A value of 0 uses the
#'   contemporaneous variance only. Values other than -1 or a non-negative
#'   integer smaller than \eqn{T - 1} are an error.
#' @param predet Numeric. If non-zero, the slope(s) of the first-difference
#'   regression are not estimated but set to the given value(s) (Westerlund,
#'   2008, Remark 4). Either a single number applied to every regressor or a
#'   vector of length \eqn{K}. Default is 0 (estimate the slopes by OLS).
#'
#' @return An object of class \code{"xtdhcoint"} containing:
#' \describe{
#'   \item{dhg}{Raw DHg (group-mean) statistic.}
#'   \item{dhp}{Raw DHp (panel) statistic.}
#'   \item{dhg_z}{Standardized DHg z-statistic.}
#'   \item{dhp_z}{Standardized DHp z-statistic.}
#'   \item{dhg_pval}{P-value for DHg (right-tail).}
#'   \item{dhp_pval}{P-value for DHp (right-tail).}
#'   \item{nf}{Number of common factors used (estimated, or fixed by
#'     \code{nfactors}). 0 means that no factor was removed.}
#'   \item{nf_at_kmax}{Logical. \code{TRUE} when the information criterion
#'     selected the upper bound \code{kmax}.}
#'   \item{nfactors}{The value of the \code{nfactors} argument.}
#'   \item{N}{Number of cross-sectional units.}
#'   \item{TT}{Number of time periods in the analysed window.}
#'   \item{time_window}{The first and last value of \code{time} in the
#'     analysed window.}
#'   \item{n_dropped}{Number of observations outside the common time window
#'     that were dropped.}
#'   \item{K}{Number of regressors.}
#'   \item{bandwidth}{Bandwidth used.}
#'   \item{kmax}{Upper bound of the factor search actually used.}
#'   \item{criterion}{Information criterion used.}
#'   \item{penalty}{Penalty function used (\code{NA} for \code{"aic"} and
#'     \code{"bic"}).}
#'   \item{call}{The matched call.}
#' }
#'
#' @details
#' The Durbin-Hausman tests examine the null hypothesis of no cointegration
#' against the alternative of cointegration. The tests are based on comparing
#' OLS and instrumental variable estimators of the autoregressive parameter
#' in the cumulated residuals.
#'
#' \strong{Computation.} Following Westerlund (2008, Section 3.2), for each
#' unit the first differences of the response are regressed on the first
#' differences of the regressors over \eqn{t = 2, \ldots, T} without an
#' intercept (eq. 12). Common factors are extracted from the
#' \eqn{(T - 1) \times N} matrix of residuals by principal components, the
#' defactored differences are recumulated from zero,
#' \eqn{\tilde e_{it} = \sum_{j = 2}^{t} \Delta \tilde e_{ij}} with
#' \eqn{\tilde e_{i1} = 0}, and the autoregression (13) is fitted over
#' \eqn{t = 2, \ldots, T}. The statistics are therefore invariant to adding a
#' constant to the response or to any regressor.
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
#' \strong{Number of factors.} The number of common factors is selected by
#' minimising the chosen criterion over \eqn{k = 0, 1, \ldots, kmax}
#' (Westerlund, 2008, Remark 5; Bai and Ng, 2002, Theorem 2). \eqn{k = 0}
#' means that no factor is removed. For the \code{"pc"}, \code{"aic"} and
#' \code{"bic"} criteria the scale \eqn{\hat\sigma^2} is \eqn{V(kmax)}, as
#' allowed by Bai and Ng (2002, p. 201).
#'
#' \strong{Number of regressors.} The limiting distributions in Westerlund
#' (2008, Theorem 1 and Remark 3) do not depend on the regressors, so the
#' simulated moments of Remark 2 (5.5464, 36.7673, 0.5005, 0.3348) apply for
#' any number of regressors \eqn{K}; there is a single set of critical values.
#' What the theory does require is that the regressors are not cointegrated
#' among themselves (Assumption 1(d)). With several regressors the user should
#' check this condition; the function only reports \eqn{K}. With
#' \code{predet} and \eqn{K > 1}, a single value is applied to every
#' regressor; supply a vector of length \eqn{K} for different slopes.
#'
#' \strong{Unbalanced panels.} The test is defined for a balanced panel: the
#' common factors \eqn{F_t} are extracted from a \eqn{(T - 1) \times N} matrix
#' whose rows are calendar periods shared by all units (Westerlund, 2008,
#' Section 3.1 and p. 201), and the criterion of Remark 5 sums over
#' \eqn{t = 2, \ldots, T} for every unit. When units are observed over
#' different periods the function keeps the common time window, the set of
#' values of \code{time} at which every unit is observed, drops the
#' observations outside it with a message, and requires that window to consist
#' of consecutive periods of the data (no unit may have a gap inside it). If
#' the common window has gaps, or fewer than 10 periods, the function stops
#' with an error. Duplicate values of \code{time} within a unit are an error.
#'
#' \strong{Small N and N larger than T.} The limiting distributions of
#' Theorem 1 require both \eqn{N} and \eqn{T} to grow, with \eqn{N/T} going
#' to zero. In a Monte Carlo study of this version (Westerlund's Section 3.4
#' design, nominal 5 percent level, one factor fixed with \code{nfactors = 1},
#' 1000 replications for each of two seed sets) the null rejection rate was
#' 38 to 41 percent for DHp with \eqn{N = 2} (\eqn{T = 50} and \eqn{100}),
#' 23 to 24 percent for DHp with \eqn{N = 5} (\eqn{T = 50} and \eqn{100}),
#' and 57 to 60 percent for DHg with \eqn{N = 25} and \eqn{T = 20}. The
#' function therefore warns when \eqn{N < 5} or \eqn{N > T}. The Bai and Ng
#' (2002) criteria are also asymptotic in both \eqn{N} and \eqn{T}: "for very
#' small N or T, the proposed criteria are inadequate" (p. 203). In their
#' Table I (\eqn{r = 1}, \eqn{k_{max} = 8}) with \eqn{N = 10}, every criterion
#' except BIC3 averages the upper bound 8, whereas BIC3 averages 7.18
#' (\eqn{T = 50}) and 5.88 (\eqn{T = 100}). The function warns when \eqn{N < 10} and the
#' number of factors is selected automatically; fix the number of factors with
#' \code{nfactors} or report results for several values of \code{kmax}.
#'
#' @references
#' Westerlund, J. (2008). Panel cointegration tests of the Fisher effect.
#' \emph{Journal of Applied Econometrics}, 23(2), 193--233.
#' \doi{10.1002/jae.967}
#'
#' Bai, J. and Ng, S. (2002). Determining the number of factors in approximate
#' factor models. \emph{Econometrica}, 70(1), 191--221.
#' \doi{10.1111/1468-0262.00273}
#'
#' Newey, W. K. and West, K. D. (1994). Automatic lag selection in covariance
#' matrix estimation. \emph{Review of Economic Studies}, 61(4), 631--653.
#' \doi{10.2307/2297912}
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
                      penalty = NULL,
                      bandwidth = -1,
                      predet = 0,
                      nfactors = NULL) {

  call <- match.call()

  # ---- criterion and penalty -------------------------------------------
  criterion <- match.arg(criterion)

  # Criterion code: 1 = PC, 2 = IC, 3 = AIC3, 4 = BIC3
  cri <- switch(criterion, "pc" = 1L, "ic" = 2L, "aic" = 3L, "bic" = 4L)

  if (criterion %in% c("aic", "bic")) {
    if (!is.null(penalty)) {
      stop("penalty does not apply to criterion = \"", criterion, "\": Bai and ",
           "Ng (2002) define a single AIC3 and a single BIC3. Leave penalty ",
           "NULL, or use criterion \"ic\" or \"pc\" with penalty 1, 2 or 3.",
           call. = FALSE)
    }
    penalty_used <- NA_integer_
  } else {
    if (is.null(penalty)) penalty <- 1L
    if (length(penalty) != 1L || is.na(penalty) || !penalty %in% 1:3) {
      stop("penalty must be 1, 2, or 3", call. = FALSE)
    }
    penalty_used <- as.integer(penalty)
  }

  # ---- model frame (keep NA so that rows stay aligned with data) ---------
  if (!is.data.frame(data)) {
    stop("data must be a data frame", call. = FALSE)
  }
  if (!is.character(id) || length(id) != 1L ||
      !is.character(time) || length(time) != 1L) {
    stop("id and time must be single character strings", call. = FALSE)
  }
  if (!id %in% names(data) || !time %in% names(data)) {
    stop("id and time variables must be columns in data", call. = FALSE)
  }

  mf <- model.frame(formula, data = data, na.action = stats::na.pass)
  if (nrow(mf) != nrow(data)) {
    stop("model.frame returned a different number of rows than data",
         call. = FALSE)
  }
  depvar <- as.numeric(model.response(mf))
  indepvars <- model.matrix(formula, data = mf)
  indepvars <- indepvars[, colnames(indepvars) != "(Intercept)", drop = FALSE]

  K <- ncol(indepvars)
  if (K < 1) {
    stop("At least one regressor required", call. = FALSE)
  }

  panel_id <- data[[id]]
  time_id <- data[[time]]

  # ---- missing values: reject with count and positions -------------------
  na_y <- which(is.na(depvar))
  na_x <- which(rowSums(is.na(indepvars)) > 0)
  na_i <- which(is.na(panel_id))
  na_t <- which(is.na(time_id))
  na_rows <- sort(unique(c(na_y, na_x, na_i, na_t)))
  if (length(na_rows) > 0) {
    where <- c(if (length(na_y)) "response", if (length(na_x)) "regressors",
               if (length(na_i)) "id", if (length(na_t)) "time")
    shown <- if (length(na_rows) > 20) {
      paste0(paste(na_rows[1:20], collapse = ", "), ", ...")
    } else {
      paste(na_rows, collapse = ", ")
    }
    stop("Missing values are not allowed: ", length(na_rows), " row(s) of data ",
         "contain NA in the ", paste(where, collapse = ", "),
         " (rows ", shown, "). Remove or impute them before calling xtdhcoint.",
         call. = FALSE)
  }

  # ---- predet ---------------------------------------------------------------
  if (!is.numeric(predet) || any(is.na(predet)) ||
      !(length(predet) == 1L || length(predet) == K)) {
    stop("predet must be a single number or a numeric vector of length K = ",
         K, call. = FALSE)
  }
  if (length(predet) == 1L) predet <- rep(predet, K)

  # ---- time alignment: common window ---------------------------------------
  units <- unique(panel_id)
  N <- length(units)
  if (N < 2) {
    stop("At least 2 cross-sectional units required", call. = FALSE)
  }

  # duplicate time stamps within a unit
  dup <- duplicated(data.frame(panel_id, time_id))
  if (any(dup)) {
    stop("Duplicate time values within a unit at data row(s) ",
         paste(which(dup), collapse = ", "), call. = FALSE)
  }

  time_all <- sort(unique(time_id))
  # periods observed by every unit
  obs_tab <- table(factor(time_id, levels = time_all))
  common <- time_all[obs_tab == N]
  if (length(common) < 10) {
    stop("At least 10 time periods common to all units are required; the ",
         "common time window has ", length(common), " period(s).",
         call. = FALSE)
  }
  # the common window must be a run of consecutive periods of the data
  pos <- match(common, time_all)
  if (any(diff(pos) != 1L)) {
    gaps <- time_all[setdiff(seq(min(pos), max(pos)), pos)]
    stop("The common time window (", format(common[1]), " to ",
         format(common[length(common)]), ") has gaps at time ",
         paste(format(gaps), collapse = ", "), ": at these periods at least ",
         "one unit is not observed. The test requires a balanced panel of ",
         "consecutive periods (Westerlund, 2008, Section 3.1 and Remark 5).",
         call. = FALSE)
  }

  keep <- time_id %in% common
  n_dropped <- sum(!keep)
  if (n_dropped > 0) {
    message("Note: Unbalanced panel. Keeping the common time window ",
            format(common[1]), " to ", format(common[length(common)]),
            " (", length(common), " periods) and dropping ", n_dropped,
            " observation(s) outside it.")
  }

  depvar <- depvar[keep]
  indepvars <- indepvars[keep, , drop = FALSE]
  panel_id <- panel_id[keep]
  time_id <- time_id[keep]

  ord <- order(match(panel_id, units), time_id)
  depvar <- depvar[ord]
  indepvars <- indepvars[ord, , drop = FALSE]
  panel_id <- panel_id[ord]
  time_id <- time_id[ord]

  TT <- length(common)

  # ---- bandwidth -------------------------------------------------------------
  if (!is.numeric(bandwidth) || length(bandwidth) != 1L || is.na(bandwidth)) {
    stop("bandwidth must be -1 or a single non-negative integer", call. = FALSE)
  }
  if (bandwidth == -1) {
    # Westerlund (2008, Section 3.4): M = largest integer below 4 (T/100)^(2/9)
    bandwidth <- floor(4 * (TT / 100)^(2 / 9))
  } else if (bandwidth < 0 || bandwidth != floor(bandwidth) ||
             bandwidth >= TT - 1) {
    stop("bandwidth must be -1 (rule floor(4 (T/100)^(2/9))) or an integer ",
         "between 0 and T - 2 = ", TT - 2, call. = FALSE)
  }
  bandwidth <- as.integer(bandwidth)

  # ---- kmax and nfactors -----------------------------------------------------
  # The PCA matrix has T - 1 rows and N columns
  max_k <- min(N, TT - 1L) - 1L
  if (length(kmax) != 1L || is.na(kmax) || kmax < 1) {
    stop("kmax must be at least 1", call. = FALSE)
  }
  kmax <- as.integer(kmax)
  if (kmax > max_k) {
    kmax <- max_k
    message("Note: kmax adjusted to ", kmax, " = min(N, T - 1) - 1")
  }
  if (!is.null(nfactors)) {
    if (length(nfactors) != 1L || is.na(nfactors) ||
        nfactors != floor(nfactors) || nfactors < 0 || nfactors > max_k) {
      stop("nfactors must be a single integer between 0 and min(N, T - 1) - 1",
           " = ", max_k, call. = FALSE)
    }
    nfactors <- as.integer(nfactors)
    kmax <- nfactors
  }

  # ---- warnings on the asymptotic range -------------------------------------
  if (N < 5 || N > TT) {
    warning("N = ", N, " and T = ", TT, ": the asymptotic moments of ",
            "Westerlund (2008, Theorem 1) require large N and T with N/T ",
            "small. In a Monte Carlo study of this version (nominal 5 percent, ",
            "one factor fixed) the null rejection rate was 38 to 41 percent ",
            "for DHp with N = 2, 23 to 24 percent for DHp with N = 5, and ",
            "57 to 60 percent for DHg with N = 25 and T = 20. Interpret the ",
            "p-values with caution.", call. = FALSE)
  }
  if (is.null(nfactors) && N < 10L) {
    warning("N = ", N, " cross-sectional units. The Bai and Ng (2002) ",
            "criteria are asymptotic in N and T and tend to select the upper ",
            "bound kmax when N is small (Bai and Ng, 2002, p. 203; in their ",
            "Table I, N = 10, every criterion except BIC3 selects the upper bound). ",
            "Consider fixing the number of factors with 'nfactors' ",
            "or reporting results for several values of 'kmax'.",
            call. = FALSE)
  }

  # ---- reshape ---------------------------------------------------------------
  Y_wide <- matrix(depvar, nrow = TT, ncol = N)
  X_list <- vector("list", N)
  for (i in seq_len(N)) {
    idx <- ((i - 1) * TT + 1):(i * TT)
    X_list[[i]] <- indepvars[idx, , drop = FALSE]
  }

  # ---- cumulated defactored residuals ---------------------------------------
  cum_result <- xtdh_cum(Y_wide, X_list, N, TT, K, kmax, penalty_used, cri,
                         predet, nfixed = nfactors)
  e <- cum_result$e
  nf <- cum_result$nf
  nf_at_kmax <- is.null(nfactors) && nf == kmax

  # ---- statistics ------------------------------------------------------------
  dhg_raw <- 0
  for (i in seq_len(N)) {
    dhg_raw <- dhg_raw + xtdh_gdh(e[, i], bandwidth)
  }
  dhp_raw <- xtdh_pdh(e, bandwidth)

  # Simulated moments from Westerlund (2008) Remark 2, p.204
  mu_g <- 5.5464   # E(Bi)
  var_g <- 36.7673 # var(Bi)
  mu_p <- 0.5005   # E(Ci)
  var_p <- 0.3348  # var(Ci)

  dhg_z <- sqrt(N) * (dhg_raw / N - mu_g) / sqrt(var_g)
  dhp_z <- sqrt(N) * (dhp_raw / N - mu_p^(-1)) / sqrt(mu_p^(-4) * var_p)

  dhg_pval <- 1 - pnorm(dhg_z)
  dhp_pval <- 1 - pnorm(dhp_z)

  # Westerlund (2008, Remark 3): the limiting distributions, and hence the
  # simulated moments above, do not depend on the regressors; there is a
  # single set of critical values for any K. The theory does assume that the
  # regressors are not cointegrated among themselves (Assumption 1(d)).
  if (K > 1) {
    message("Note: K = ", K, " regressors. The asymptotic moments of Westerlund ",
            "(2008) do not depend on K (Remark 3), but the tests assume the ",
            "regressors are not cointegrated among themselves (Assumption 1(d)).")
  }

  result <- list(
    dhg = dhg_raw,
    dhp = dhp_raw,
    dhg_z = dhg_z,
    dhp_z = dhp_z,
    dhg_pval = dhg_pval,
    dhp_pval = dhp_pval,
    nf = nf,
    nf_at_kmax = nf_at_kmax,
    nfactors = nfactors,
    N = N,
    TT = TT,
    time_window = c(common[1], common[length(common)]),
    n_dropped = n_dropped,
    K = K,
    bandwidth = bandwidth,
    kmax = kmax,
    criterion = criterion,
    penalty = penalty_used,
    call = call
  )

  class(result) <- "xtdhcoint"
  return(result)
}


#' Cumulated Defactored Residuals
#'
#' @description
#' Computes the cumulated defactored residuals of Westerlund (2008,
#' Section 3.2): first-difference regression (eq. 12) over t = 2..T,
#' principal components on the (T - 1) x N residual matrix, recumulation
#' from zero.
#'
#' @param Y_wide T x N matrix of dependent variable.
#' @param X_list List of N matrices, each T x K, containing regressors.
#' @param N Number of cross-sectional units.
#' @param TT Number of time periods.
#' @param K Number of regressors.
#' @param kmax Maximum number of factors.
#' @param penalty Penalty type (1, 2, 3 or NA).
#' @param cri Criterion code.
#' @param predet Numeric vector of length K of predetermined slopes (all zero
#'   means OLS).
#' @param nfixed Fixed number of factors or NULL.
#'
#' @return List with elements:
#' \describe{
#'   \item{e}{T x N matrix of cumulated defactored residuals; the first row
#'     is zero (e_i1 = 0).}
#'   \item{nf}{Number of factors removed.}
#' }
#'
#' @keywords internal
#' @noRd
xtdh_cum <- function(Y_wide, X_list, N, TT, K, kmax, penalty, cri, predet,
                     nfixed = NULL) {

  # First-differenced residuals, t = 2..T: (T - 1) x N
  de <- matrix(0, nrow = TT - 1L, ncol = N)

  for (i in seq_len(N)) {
    dyi <- diff(Y_wide[, i])
    dxi <- diff(X_list[[i]])
    dxi <- matrix(dxi, ncol = K)

    if (any(predet != 0)) {
      # Remark 4: known slope
      de[, i] <- dyi - dxi %*% predet
    } else {
      # eq. (12): OLS of dy on dx without intercept
      XtX <- crossprod(dxi)
      if (qr(dxi)$rank < K) {
        stop("The first-differenced regressors of unit ", i,
             " are exactly collinear; eq. (12) cannot be estimated.",
             call. = FALSE)
      }
      de[, i] <- dyi - dxi %*% solve(XtX, crossprod(dxi, dyi))
    }
  }

  # Number of factors (search over 0..kmax, or fixed by the user)
  nf <- if (is.null(nfixed)) xtdh_fact(de, kmax, penalty, cri) else as.integer(nfixed)

  # Defactor (k = 0 leaves the residuals unchanged)
  if (nf > 0L) {
    prin_result <- xtdh_prin(de, nf)
    de <- de - prin_result$F_mat %*% t(prin_result$Lambda)
  }

  # Recumulate from zero: e_i1 = 0, e_it = sum_{j=2}^t de_ij
  e <- rbind(0, apply(de, 2, cumsum))

  return(list(e = e, nf = nf))
}


#' Individual DH Statistic
#'
#' @description
#' Computes the individual Durbin-Hausman statistic for one unit
#' (Westerlund, 2008, p. 203).
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
#' Computes the panel Durbin-Hausman statistic (Westerlund, 2008, p. 203).
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
#' Selects the number of common factors by minimising a Bai and Ng (2002)
#' criterion over k = 0, 1, ..., kmax (Bai and Ng, 2002, Theorem 2;
#' Westerlund, 2008, Remark 5).
#'
#' @param e (T - 1) x N matrix of first-differenced residuals.
#' @param kmax Maximum number of factors.
#' @param p Penalty type (1, 2 or 3 for PC and IC; ignored otherwise).
#' @param c Criterion code (1 = PC, 2 = IC, 3 = AIC3, 4 = BIC3).
#'
#' @return Integer. Estimated number of factors (0 means none).
#'
#' @keywords internal
#' @noRd
xtdh_fact <- function(e, kmax, p, c) {

  TT <- nrow(e)
  N <- ncol(e)

  # V(k) for k = 0..kmax
  V <- numeric(kmax + 1L)
  V[1L] <- sum(e^2) / (N * TT)
  for (k in seq_len(kmax)) {
    prin_result <- xtdh_prin(e, k)
    u <- e - prin_result$F_mat %*% t(prin_result$Lambda)
    V[k + 1L] <- sum(u^2) / (N * TT)
  }
  smax <- V[kmax + 1L]   # sigma_hat^2 = V(kmax), Bai and Ng (2002, p. 201)

  ks <- 0:kmax
  cr <- numeric(kmax + 1L)
  for (j in seq_along(ks)) {
    k <- ks[j]
    if (c == 1L || c == 2L) {
      pen_val <- if (p == 1) {
        (N + TT) / (N * TT) * log((N * TT) / (N + TT))
      } else if (p == 2) {
        (N + TT) / (N * TT) * log(min(N, TT))
      } else {
        log(min(N, TT)) / min(N, TT)
      }
      cr[j] <- if (c == 1L) V[j] + k * smax * pen_val else log(V[j]) + k * pen_val
    } else if (c == 3L) {
      # AIC3
      cr[j] <- V[j] + k * smax * 2 * (N + TT - k) / (N * TT)
    } else {
      # BIC3
      cr[j] <- V[j] + k * smax * (N + TT - k) * log(N * TT) / (N * TT)
    }
  }

  # k that minimises the criterion (first minimum, i.e. the smallest k on ties)
  return(ks[which.min(cr)])
}


#' Principal Components Estimation
#'
#' @description
#' Extracts principal components from a matrix (Westerlund, 2008, p. 201).
#'
#' @param e (T - 1) x N matrix.
#' @param nf Number of factors to extract (at least 1).
#'
#' @return List with elements:
#' \describe{
#'   \item{F_mat}{(T - 1) x nf matrix of factor estimates.}
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
