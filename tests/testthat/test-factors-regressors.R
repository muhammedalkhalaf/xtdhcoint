## Regression tests for 1.0.4 (updated in 1.0.5: the small-N note is a
## warning, nfactors = 0 is allowed):
##  (a) no "moments derived for K=1" warning: Westerlund (2008, Remark 3)
##      shows the limiting distributions do not depend on the regressors;
##  (b) small-N warning and nf_at_kmax flag when the IC hits the upper bound;
##  (c) nfactors fixes the number of factors and bypasses the criterion.

make_panel <- function(N = 6, TT = 17, K = 2, seed = 1) {
  set.seed(seed)
  d <- do.call(rbind, lapply(seq_len(N), function(i) {
    X <- apply(matrix(rnorm(TT * K), TT, K), 2, cumsum)
    y <- cumsum(rnorm(TT)) + X %*% rep(0.5, K)
    out <- data.frame(id = i, t = seq_len(TT), y = as.numeric(y))
    for (k in seq_len(K)) out[[paste0("x", k)]] <- X[, k]
    out
  }))
  d
}

test_that("multiple regressors no longer trigger the K=1 moments warning", {
  d <- make_panel(K = 3)
  msgs <- character(0)
  withCallingHandlers(
    r <- suppressWarnings(xtdhcoint(y ~ x1 + x2 + x3, data = d, id = "id", time = "t",
                   kmax = 1, criterion = "ic")),
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  expect_false(any(grepl("derived for K=1", msgs, fixed = TRUE)))
  expect_true(any(grepl("do not depend on K", msgs, fixed = TRUE)))
  expect_equal(r$K, 3L)
  expect_true(is.finite(r$dhg_z) && is.finite(r$dhp_z))
})

test_that("small N produces a warning and flags nf at kmax", {
  d <- make_panel(N = 6, K = 1)
  expect_warning(
    r <- suppressMessages(xtdhcoint(y ~ x1, data = d, id = "id", time = "t", kmax = 3,
                   criterion = "ic")),
    "tend to select the upper bound")
  expect_type(r$nf_at_kmax, "logical")
  if (r$nf == r$kmax) expect_true(r$nf_at_kmax)
})

test_that("nfactors fixes the number of factors and bypasses selection", {
  d <- make_panel(N = 6, K = 1)
  r1 <- suppressWarnings(suppressMessages(xtdhcoint(y ~ x1, data = d, id = "id", time = "t",
                                   nfactors = 1)))
  r2 <- suppressWarnings(suppressMessages(xtdhcoint(y ~ x1, data = d, id = "id", time = "t",
                                   kmax = 1, criterion = "ic")))
  expect_equal(r1$nf, 1L)
  expect_equal(r1$nfactors, 1L)
  expect_false(isTRUE(r1$nf_at_kmax))
  # with kmax = 1 the criterion picks 0 or 1 factor; if it picks 1 the
  # statistics agree with the fixed computation
  if (r2$nf == 1L) {
    expect_equal(r1$dhg, r2$dhg)
    expect_equal(r1$dhp, r2$dhp)
  } else {
    expect_equal(r2$nf, 0L)
  }
  r0 <- suppressWarnings(suppressMessages(xtdhcoint(y ~ x1, data = d, id = "id", time = "t",
                                   nfactors = 0)))
  expect_equal(r0$nf, 0L)
  expect_error(xtdhcoint(y ~ x1, data = d, id = "id", time = "t", nfactors = -1),
               "nfactors")
  expect_error(xtdhcoint(y ~ x1, data = d, id = "id", time = "t", nfactors = 6),
               "nfactors")
})
