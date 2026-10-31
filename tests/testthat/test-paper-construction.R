## Tests for 1.0.5: hand rebuild of DHg and DHp following Westerlund (2008,
## Section 3.2) with lm.fit / eigen / cumsum, level invariance, k = 0
## selection, missing values, unbalanced panels, argument validation.

sim_panel <- function(N, TT, K = 1, rho = 1, lambda_sd = 1, alpha_sd = 1,
                      burn = 50, seed = 1) {
  set.seed(seed)
  Tt <- TT + burn
  Ft <- rnorm(Tt)
  lam <- rnorm(N, 0, lambda_sd)
  out <- vector("list", N)
  for (i in seq_len(N)) {
    X <- apply(matrix(rnorm(Tt * K), Tt, K), 2, cumsum)
    e <- numeric(Tt); v <- rnorm(Tt)
    for (t in 2:Tt) e[t] <- rho * e[t - 1] + v[t]
    z <- lam[i] * Ft + e
    y <- rnorm(1, 0, alpha_sd) + X %*% rep(1, K) + z
    idx <- (burn + 1):Tt
    d <- data.frame(id = i, t = seq_len(TT), y = as.numeric(y[idx]))
    for (k in seq_len(K)) d[[paste0("x", k)]] <- X[idx, k]
    out[[i]] <- d
  }
  do.call(rbind, out)
}

## Independent rebuild of the paper's statistics.
## eq. (12): OLS of dy on dx over t = 2..T, no intercept (lm.fit)
## p. 201: F = sqrt(T - 1) eigenvectors of D D', Lambda = F'D/(T - 1)
## p. 202: e_it = sum_{j=2}^t de_ij, e_i1 = 0; eq. (13) over t = 2..T
## p. 202-203: Bartlett omega^2, sigma^2, S_i, DHg, S_n, DHp
hand_dh <- function(dat, N, TT, K, nf, M, slope = NULL) {
  ## slope: if given, the first-difference slopes are fixed (Remark 4) instead of OLS
  xn <- paste0("x", seq_len(K))
  D <- NULL
  for (i in seq_len(N)) {
    di <- dat[dat$id == i, ]
    dy <- diff(di$y)
    dX <- matrix(apply(as.matrix(di[, xn, drop = FALSE]), 2, diff), ncol = K)
    D <- cbind(D, if (is.null(slope)) lm.fit(dX, dy)$residuals
               else dy - dX %*% slope)
  }
  if (nf > 0) {
    Tn <- nrow(D)
    ev <- eigen(D %*% t(D), symmetric = TRUE)
    Fh <- ev$vectors[, 1:nf, drop = FALSE] * sqrt(Tn)
    Lh <- t(D) %*% Fh / Tn
    D <- D - Fh %*% t(Lh)
  }
  E <- rbind(0, apply(D, 2, cumsum))
  lrv <- function(v) {
    Tn <- length(v); s0 <- sum(v^2) / Tn; io <- 0
    for (j in seq_len(M))
      io <- io + (1 - j / (M + 1)) * sum(v[1:(Tn - j)] * v[(j + 1):Tn]) / Tn
    c(s0, s0 + 2 * io)
  }
  Tn <- nrow(E); dhg <- 0
  for (i in seq_len(N)) {
    w0 <- E[2:Tn, i]; wl <- E[1:(Tn - 1), i]
    phi_hat <- sum(wl * w0) / sum(wl^2)
    phi_til <- sum(w0^2) / sum(wl * w0)
    vv <- lrv(w0 - phi_hat * wl)
    dhg <- dhg + (vv[2] / vv[1]^2) * (phi_til - phi_hat)^2 * sum(wl^2)
  }
  W0 <- as.vector(E[2:Tn, ]); WL <- as.vector(E[1:(Tn - 1), ])
  phi_hat_p <- sum(WL * W0) / sum(WL^2)
  phi_til_p <- sum(W0^2) / sum(WL * W0)
  Vp <- matrix(W0 - phi_hat_p * WL, Tn - 1, N)
  snt <- 0; lnt <- 0
  for (i in seq_len(N)) { vv <- lrv(Vp[, i]); snt <- snt + vv[1]; lnt <- lnt + vv[2] }
  dhp <- ((lnt / N) / (snt / N)^2) * (phi_til_p - phi_hat_p)^2 * sum(WL^2)
  c(dhg = dhg, dhp = dhp)
}

test_that("DHg and DHp equal the hand rebuild of Westerlund (2008, Section 3.2)", {
  for (cfg in list(list(N = 10, TT = 30, K = 1, nf = 1),
                   list(N = 8, TT = 30, K = 2, nf = 2),
                   list(N = 12, TT = 25, K = 1, nf = 0))) {
    dat <- sim_panel(cfg$N, cfg$TT, cfg$K, seed = 11 + cfg$K + cfg$nf)
    M <- floor(4 * (cfg$TT / 100)^(2 / 9))
    fml <- as.formula(paste("y ~", paste0("x", seq_len(cfg$K), collapse = " + ")))
    r <- suppressWarnings(suppressMessages(
      xtdhcoint(fml, dat, id = "id", time = "t", nfactors = cfg$nf)))
    h <- hand_dh(dat, cfg$N, cfg$TT, cfg$K, cfg$nf, M)
    # tolerance 1e-8: both sides are closed-form arithmetic on the same data;
    # the audit found agreement at 3e-13
    expect_equal(r$dhg, unname(h["dhg"]), tolerance = 1e-8)
    expect_equal(r$dhp, unname(h["dhp"]), tolerance = 1e-8)
    expect_equal(r$bandwidth, M)
    expect_equal(r$TT, cfg$TT)
  }
})

test_that("standardisation uses the Remark 2 moments", {
  dat <- sim_panel(10, 30, 1, seed = 2)
  r <- suppressMessages(xtdhcoint(y ~ x1, dat, id = "id", time = "t", nfactors = 1))
  expect_equal(r$dhg_z, sqrt(10) * (r$dhg / 10 - 5.5464) / sqrt(36.7673))
  expect_equal(r$dhp_z, sqrt(10) * (r$dhp / 10 - 1 / 0.5005) / sqrt(0.3348 / 0.5005^4))
  expect_equal(r$dhg_pval, 1 - pnorm(r$dhg_z))
})

test_that("statistics are invariant to the level of y and x", {
  dat <- sim_panel(10, 40, 1, seed = 5)
  d2 <- dat; d2$y <- d2$y + 100
  d3 <- dat; d3$x1 <- d3$x1 - 250
  a <- suppressMessages(xtdhcoint(y ~ x1, dat, id = "id", time = "t", nfactors = 1))
  b <- suppressMessages(xtdhcoint(y ~ x1, d2, id = "id", time = "t", nfactors = 1))
  c3 <- suppressMessages(xtdhcoint(y ~ x1, d3, id = "id", time = "t", nfactors = 1))
  for (nm in c("dhg", "dhp", "dhg_z", "dhp_z", "dhg_pval", "dhp_pval")) {
    expect_equal(a[[nm]], b[[nm]], tolerance = 1e-10)
    expect_equal(a[[nm]], c3[[nm]], tolerance = 1e-10)
  }
  # also with automatic factor selection
  a2 <- suppressMessages(xtdhcoint(y ~ x1, dat, id = "id", time = "t"))
  b2 <- suppressMessages(xtdhcoint(y ~ x1, d2, id = "id", time = "t"))
  expect_equal(a2$nf, b2$nf)
  expect_equal(a2$dhg_z, b2$dhg_z, tolerance = 1e-10)
})

test_that("the factor search includes k = 0 and picks it on a no-factor DGP", {
  dat <- sim_panel(20, 100, 1, lambda_sd = 0, seed = 3)
  r <- suppressMessages(xtdhcoint(y ~ x1, dat, id = "id", time = "t"))
  expect_equal(r$nf, 0L)
  expect_false(r$nf_at_kmax)
  # k = 0 selected means the statistics equal the nfactors = 0 computation
  r0 <- suppressMessages(xtdhcoint(y ~ x1, dat, id = "id", time = "t", nfactors = 0))
  expect_equal(r$dhg, r0$dhg)
  expect_equal(r$dhp, r0$dhp)
  # IC_p1 hand computation over k = 0..kmax on the differenced residuals
  D <- NULL
  for (i in 1:20) {
    di <- dat[dat$id == i, ]
    D <- cbind(D, lm.fit(matrix(diff(di$x1), ncol = 1), diff(di$y))$residuals)
  }
  Tn <- nrow(D); Nn <- ncol(D)
  V <- numeric(6); V[1] <- sum(D^2) / (Nn * Tn)
  ev <- eigen(D %*% t(D), symmetric = TRUE)
  for (k in 1:5) {
    Fh <- ev$vectors[, 1:k, drop = FALSE] * sqrt(Tn)
    U <- D - Fh %*% t(t(D) %*% Fh / Tn)
    V[k + 1] <- sum(U^2) / (Nn * Tn)
  }
  ic <- log(V) + (0:5) * (Nn + Tn) / (Nn * Tn) * log(Nn * Tn / (Nn + Tn))
  expect_equal(which.min(ic) - 1, 0)
})

test_that("one factor is found when one is present", {
  dat <- sim_panel(25, 100, 1, lambda_sd = 3, seed = 4)
  r <- suppressMessages(xtdhcoint(y ~ x1, dat, id = "id", time = "t"))
  expect_equal(r$nf, 1L)
})

test_that("missing values are rejected with count and positions", {
  dat <- sim_panel(10, 30, 1, seed = 6)
  dn <- dat; dn$y[137] <- NA; dn$x1[200] <- NA
  expect_error(xtdhcoint(y ~ x1, dn, id = "id", time = "t"),
               "2 row\\(s\\).*rows 137, 200")
  dn2 <- dat; dn2$t[5] <- NA
  expect_error(xtdhcoint(y ~ x1, dn2, id = "id", time = "t"), "time.*rows 5")
})

test_that("unbalanced panels are aligned on the time index", {
  dat <- sim_panel(10, 50, 1, seed = 7)
  # unit 1 observed only for t = 11..50
  du <- dat[!(dat$id == 1 & dat$t <= 10), ]
  expect_message(ru <- xtdhcoint(y ~ x1, du, id = "id", time = "t", nfactors = 1),
                 "common time window 11 to 50")
  expect_equal(ru$TT, 40L)
  expect_equal(ru$n_dropped, 90L)
  expect_equal(unname(ru$time_window), c(11, 50))
  # same as running the test on the balanced common window
  rref <- suppressMessages(xtdhcoint(y ~ x1, dat[dat$t > 10, ], id = "id",
                                     time = "t", nfactors = 1))
  expect_equal(ru$dhg, rref$dhg)
  expect_equal(ru$dhp, rref$dhp)
  # row order does not matter
  set.seed(15)
  dsh <- du[sample(nrow(du)), ]
  rsh <- suppressMessages(xtdhcoint(y ~ x1, dsh, id = "id", time = "t", nfactors = 1))
  expect_equal(rsh$dhg, ru$dhg)
  # a gap inside the common window is an error
  dg <- dat[-which(dat$id == 2 & dat$t == 25), ]
  expect_error(xtdhcoint(y ~ x1, dg, id = "id", time = "t"), "gaps at time 25")
  # duplicate time stamps are an error
  dd <- rbind(dat, dat[3, ])
  expect_error(xtdhcoint(y ~ x1, dd, id = "id", time = "t"), "Duplicate time")
  # too short a common window
  ds <- dat[!(dat$id == 1 & dat$t <= 45), ]
  expect_error(xtdhcoint(y ~ x1, ds, id = "id", time = "t"), "common time window has 5")
})

test_that("criterion and penalty combinations are validated", {
  dat <- sim_panel(10, 30, 1, seed = 8)
  expect_error(xtdhcoint(y ~ x1, dat, id = "id", time = "t", criterion = "bic", penalty = 2),
               "penalty does not apply")
  expect_error(xtdhcoint(y ~ x1, dat, id = "id", time = "t", criterion = "aic", penalty = 1),
               "penalty does not apply")
  expect_error(xtdhcoint(y ~ x1, dat, id = "id", time = "t", penalty = 4), "penalty must be")
  rb <- suppressMessages(xtdhcoint(y ~ x1, dat, id = "id", time = "t", criterion = "bic"))
  expect_true(is.na(rb$penalty))
  ri <- suppressMessages(xtdhcoint(y ~ x1, dat, id = "id", time = "t", criterion = "ic", penalty = 3))
  expect_equal(ri$penalty, 3L)
})

test_that("bandwidth is validated and the -1 rule is floor(4 (T/100)^(2/9))", {
  dat <- sim_panel(10, 50, 1, seed = 9)
  r <- suppressMessages(xtdhcoint(y ~ x1, dat, id = "id", time = "t", nfactors = 1))
  expect_equal(r$bandwidth, 3L)
  r0 <- suppressMessages(xtdhcoint(y ~ x1, dat, id = "id", time = "t", nfactors = 1, bandwidth = 0))
  expect_equal(r0$bandwidth, 0L)
  expect_false(isTRUE(all.equal(r0$dhg, r$dhg)))
  expect_error(xtdhcoint(y ~ x1, dat, id = "id", time = "t", bandwidth = 0.5), "bandwidth must be")
  expect_error(xtdhcoint(y ~ x1, dat, id = "id", time = "t", bandwidth = 60), "bandwidth must be")
})

test_that("predet accepts a scalar or a vector of length K", {
  dat <- sim_panel(10, 30, 2, seed = 10)
  r1 <- suppressMessages(xtdhcoint(y ~ x1 + x2, dat, id = "id", time = "t", nfactors = 1, predet = 1))
  r2 <- suppressMessages(xtdhcoint(y ~ x1 + x2, dat, id = "id", time = "t", nfactors = 1, predet = c(1, 1)))
  expect_equal(r1$dhg, r2$dhg)
  expect_error(xtdhcoint(y ~ x1 + x2, dat, id = "id", time = "t", predet = c(1, 1, 1)), "predet")
  # hand construction of Remark 4 (known slopes c(1, 1)), one factor,
  # bandwidth floor(4 (30/100)^(2/9)) = 3, compared with the package
  M <- floor(4 * (30 / 100)^(2 / 9))
  h <- hand_dh(dat, 10, 30, 2, 1, M, slope = c(1, 1))
  expect_equal(r1$dhg, unname(h["dhg"]), tolerance = 1e-8)
  expect_equal(r1$dhp, unname(h["dhp"]), tolerance = 1e-8)
  # the slopes must actually matter: OLS slopes give different statistics
  r_ols <- suppressMessages(xtdhcoint(y ~ x1 + x2, dat, id = "id", time = "t", nfactors = 1))
  expect_false(isTRUE(all.equal(r1$dhg, r_ols$dhg)))
})

test_that("small N and N > T produce warnings", {
  dat <- sim_panel(4, 30, 1, seed = 12)
  expect_warning(suppressMessages(xtdhcoint(y ~ x1, dat, id = "id", time = "t", nfactors = 1)),
                 "N = 4 and T = 30")
  dat2 <- sim_panel(12, 10, 1, seed = 13)
  expect_warning(suppressMessages(xtdhcoint(y ~ x1, dat2, id = "id", time = "t", nfactors = 1)),
                 "N = 12 and T = 10")
  dat3 <- sim_panel(8, 30, 1, seed = 14)
  expect_warning(suppressMessages(xtdhcoint(y ~ x1, dat3, id = "id", time = "t")),
                 "tend to select the upper bound")
  expect_silent(suppressMessages(xtdhcoint(y ~ x1, dat3, id = "id", time = "t", nfactors = 1)))
})
