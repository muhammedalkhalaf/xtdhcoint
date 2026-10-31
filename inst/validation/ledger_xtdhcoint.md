# Derivation ledger: `xtdhcoint()` (xtdhcoint 1.0.5)

Source paper: Westerlund, J. (2008), "Panel cointegration tests of the Fisher effect", Journal of Applied Econometrics 23, 193-233 (page numbers below refer to the journal). Secondary paper: Bai, J. and Ng, S. (2002), "Determining the number of factors in approximate factor models", Econometrica 70(1), 191-221.

Code: `R/xtdhcoint.R` of this package (file:line below refer to version 1.0.5).

Origin: this ledger was produced by the audit of version 1.0.4 (October 2026) and updated when the audit findings were implemented in 1.0.5. "Matched at 1e-10" means the package statistic and an independent rebuild (lm.fit, eigen, cumsum) agree to better than 1e-10 on simulated data. The rebuild is reproduced in `tests/testthat/test-paper-construction.R`.

## A. First-step regression and defactoring

| Paper equation / page | Mathematical expression | Code file:line | Convention adopted | Source of convention | Verification status |
|---|---|---|---|---|---|
| Eq. (7), p. 199: i_it = alpha_i + beta_i pi_it + z_it | Levels model with unit-specific intercept | xtdhcoint.R:230 | The intercept column is removed from the model matrix; alpha_i vanishes on differencing, so no constant is estimated anywhere. The inert `constant` argument of 1.0.4 was removed. | Paper (eq. 12 has no intercept) | Verified: no intercept in any internal regression (1.0.5). |
| Eq. (12), p. 201: Delta z_it = Delta i_it - beta_hat_i Delta pi_it, "where beta_hat_i is obtained by regressing Delta i_it on Delta pi_it"; matrices are "(T-1) x K" and "(T-1) x N" | OLS of Delta y on Delta x, no intercept, t = 2..T (T-1 observations) | xtdhcoint.R:486-500 (`diff`, `solve(crossprod(dxi), crossprod(dxi, dyi))`) | T - 1 first differences, no padding | Paper | Fixed in 1.0.5 (1.0.4 prepended a zero row so the first "difference" was the level). Matched at 1e-10 against the lm.fit rebuild (max abs diff 6.8e-13 over six configurations, N in {10, 25}, T in {50, 100}, K in {1, 2}, nf in {1, 2}). Level invariance: adding 100 to y or subtracting 250 from x changes the statistics by at most 3.6e-13 (test "statistics are invariant to the level of y and x", tolerance 1e-10). |
| Eq. (12) regressors | K regressors, no intercept | xtdhcoint.R:496-499 | Exactly collinear first-differenced regressors stop with an error naming the unit (`qr(dxi)$rank < K`) | Package choice | Verified: clear error (1.0.5). |
| Remark 4, p. 205: replace Delta z_hat with Delta z_it = Delta i_it - beta_i Delta pi_it (known slope) | Predetermined slope | xtdhcoint.R:490-492 | `predet` is a scalar applied to every regressor or a vector of length K; documented | Package choice (paper has K = 1) | Verified: scalar and vector of equal entries give identical statistics (test "predet accepts a scalar or a vector of length K"). |
| p. 201: F_hat = sqrt(T-1) times the eigenvectors of the (T-1) x (T-1) matrix Delta z Delta z' for the k largest eigenvalues; Lambda_hat = F_hat' Delta z / (T-1) | Principal components | xtdhcoint.R:711-729 | SVD of e e' when N > T - 1 (F = sqrt(T-1) u, Lambda = e'F/(T-1)) or of e'e when N <= T - 1 (Lambda = sqrt(N) u, F = e Lambda / N); both give the same common component | Paper (the two routes are algebraically identical) | Matched at 1e-10 against the `eigen` rebuild (1.0.5 tests). |
| p. 201: Delta e_tilde_it = Delta z_hat_it - lambda_hat_i' F_hat_t | Defactored first differences | xtdhcoint.R:509-512 | `de - F_mat %*% t(Lambda)`; skipped when nf = 0 | Paper | Matched at 1e-10 |
| p. 202: e_tilde_it = sum_{j=2}^{t} Delta e_tilde_ij | Recumulation from zero at t = 1 | xtdhcoint.R:515 `rbind(0, apply(de, 2, cumsum))` | e_i1 = 0, T rows | Paper | Fixed in 1.0.5; matched at 1e-10. |

## B. Durbin-Hausman statistics (Section 3.2, pp. 202-203)

| Paper equation / page | Mathematical expression | Code file:line | Convention adopted | Source of convention | Verification status |
|---|---|---|---|---|---|
| Eq. (13), p. 202: e_tilde_it = phi_i e_tilde_it-1 + error | AR(1) without intercept on the cumulated residuals, t = 2..T | xtdhcoint.R:539-540 | `w0 = w[2:T]`, `wl = w[1:(T-1)]`, no intercept, no demeaning; wl[1] = e_i1 = 0 | Paper | Matched at 1e-10 |
| p. 202: phi_hat_i OLS; phi_tilde_i IV "obtained by simply instrumenting e_tilde_it-1 with e_tilde_it" | phi_hat = sum e_t e_t-1 / sum e_t-1^2; phi_tilde = sum e_t^2 / sum e_t e_t-1 | xtdhcoint.R:543, 546 | Same | Paper (and Appendix for the pooled versions) | Matched at 1e-10 |
| p. 202: omega_hat_i^2 = (1/(T-1)) sum_{j=-M_i}^{M_i} (1 - abs(j)/(M_i+1)) sum_{t=j+1}^{T} v_hat_it v_hat_it-j | Bartlett long-run variance of the OLS residuals of (13) | xtdhcoint.R:552-562, 744-756 | `s = sum(resid^2)/(T-1)`; `io = sum_j (1 - j/(M+1)) sum_t v_t v_t-j / (T-1)`; `omega2 = s + 2 io` | Paper | Matched at 1e-10 (the autocovariance denominator is the full residual length T - 1, as in the paper's formula) |
| p. 202: sigma_hat_i^2 "contemporaneous variance estimate" | sigma_hat_i^2 = (1/(T-1)) sum v_hat_it^2 | xtdhcoint.R:552 | Divides by the number of residuals | Paper | Matched at 1e-10 |
| p. 202: S_i = omega_hat_i^2 / sigma_hat_i^4 | Variance ratio | xtdhcoint.R:565-568 | `s <- s^2/sig; dhs <- (b2-b1)^2/(s*(1/sum(wl^2)))`, algebraically S_i (phi_tilde - phi_hat)^2 sum e_t-1^2 | Paper | Matched at 1e-10 |
| p. 203: DH_g = sum_{i=1}^{n} S_i (phi_tilde_i - phi_hat_i)^2 sum_{t=2}^{T} e_tilde_it-1^2 | Group-mean statistic | xtdhcoint.R:393-396 | n = N (all units) | Paper (Section 3.4: "we set n equal to N") | Matched at 1e-10 (test "DHg and DHp equal the hand rebuild", tolerance 1e-8; observed 6.8e-13) |
| p. 202: S_n = omega_hat_n^2 / sigma_hat_n^4, omega_hat_n^2 = (1/n) sum omega_hat_i^2, sigma_hat_n^2 = (1/n) sum sigma_hat_i^2 | Pooled variance ratio | xtdhcoint.R:606-631 | Residuals v_hat_it for the pooled ratio are taken from the pooled OLS (phi_hat) rather than from the unit-by-unit OLS | Package choice; the paper does not state which residuals enter the pooled ratio | Matched at 1e-10 given this convention; not verifiable against the paper text |
| p. 203: DH_p = S_n (phi_tilde - phi_hat)^2 sum_i sum_t e_tilde_it-1^2 | Panel statistic | xtdhcoint.R:597-631 | Pooled sums over all N(T-1) pairs | Paper | Matched at 1e-10 |
| Footnote 8, p. 202 and Section 3.4: M_i "the largest integer less than 4(T/100)^(2/9)" | Bandwidth rule | xtdhcoint.R:329-336 | `floor(4*(TT/100)^(2/9))` when `bandwidth = -1`; same M for all i; user values 0..T-2 used as given; documented as the Section 3.4 rule, not Newey and West (1994) | Paper (Section 3.4 simulation rule) | Matches the rule (T = 20: 2; 50: 3; 100: 4; 200: 4); documentation corrected in 1.0.5; `bandwidth = 0` no longer changed to 1 |

## C. Standardisation and moments (Theorem 1 and Remark 2, p. 204)

| Paper equation / page | Mathematical expression | Code file:line | Convention adopted | Source of convention | Verification status |
|---|---|---|---|---|---|
| Theorem 1, p. 204: n^{1/2}(n^{-1} DH_g - E(B_i)) converges in distribution to N(0, var(B_i)) | Z_g = sqrt(n) (DH_g / n - E(B_i)) / sqrt(var(B_i)) | xtdhcoint.R:407 | `sqrt(N)*(dhg_raw/N - mu_g)/sqrt(var_g)` | Paper | Matched (algebraic identity; test "standardisation uses the Remark 2 moments") |
| Theorem 1, p. 204: n^{1/2}(n^{-1} DH_p - E(C_i)^{-1}) converges in distribution to N(0, E(C_i)^{-4} var(C_i)) | Z_p = sqrt(n) (DH_p / n - 1/E(C_i)) / sqrt(var(C_i)/E(C_i)^4) | xtdhcoint.R:408 | `sqrt(N)*(dhp_raw/N - mu_p^(-1))/sqrt(mu_p^(-4)*var_p)` | Paper | Matched (algebraic identity) |
| Remark 2, p. 204: "The simulated values of E(B_i), var(B_i), E(C_i) and var(C_i) using this method are 5.5464, 36.7673, 0.5005 and 0.3348, respectively" | Moment constants | xtdhcoint.R:402-405 (methods.R repeats 5.5464 and 1/0.5005) | Four constants hard-coded | Paper | Matched to printed precision, cell by cell (4 of 4) |
| Remark 2: "the computed value should be compared to the right tail of the normal distribution" | p = 1 - Phi(Z) | xtdhcoint.R:410-411; methods.R | Right-tail p-values and critical values | Paper | Matched |

## D. Number of factors (Remark 5, p. 205 and Bai and Ng 2002)

| Paper equation / page | Mathematical expression | Code file:line | Convention adopted | Source of convention | Verification status |
|---|---|---|---|---|---|
| Westerlund Remark 5: k_hat = arg min_{0 <= k <= kmax} [log sigma_hat^2(k) + k ((N+T)/(NT)) log(NT/(N+T))], sigma_hat^2 = (1/NT) sum_i sum_{t=2}^{T} Delta e_tilde_it^2; Bai-Ng Theorem 2: "k_hat = arg min_{0 <= k <= kmax} PC(k)" | Search range includes k = 0 | xtdhcoint.R:653-692 (`V[1L]` is V(0); `ks <- 0:kmax`) | Search over k = 0..kmax; the smallest k is taken on ties; k = 0 means no defactoring | Paper | Fixed in 1.0.5. Test "the factor search includes k = 0": on a no-factor DGP (N = 20, T = 100) the package returns 0 and the hand IC_p1 over k = 0..5 has its minimum at 0. Monte Carlo (Table I cells, R = 1000): with no factor the package now selects k = 0 in 100 percent of draws; with one factor it selects k = 1 in 99.8 to 100 percent (mean 1.00 to 1.01). |
| Bai-Ng eq. (9): IC_p1 = ln V(k) + k (N+T)/(NT) ln(NT/(N+T)); IC_p2 = ln V(k) + k (N+T)/(NT) ln C_NT^2; IC_p3 = ln V(k) + k ln(C_NT^2)/C_NT^2, C_NT^2 = min(N, T) | Information criteria | xtdhcoint.R:670-680 | V(k) = sum of squared residuals / (N (T-1)); T is the number of rows of the differenced matrix | Bai and Ng (2002) | Matched at 1e-10 in the 1.0.4 audit for k = 1..kmax (2400 of 2400 selections); k = 0 term verified by the 1.0.5 test |
| Bai-Ng p. 201: PC_p1 = V(k) + k sigma_hat^2 (N+T)/(NT) ln(NT/(N+T)); PC_p2 with ln C_NT^2; PC_p3 = V(k) + k sigma_hat^2 ln(C_NT^2)/C_NT^2; sigma_hat^2 "can be replaced by V(kmax)" | Panel Cp criteria | xtdhcoint.R:666, 680 | sigma_hat^2 = V(kmax) | Bai and Ng (2002), explicitly allowed | Matched at 1e-10 (1.0.4 audit) |
| Bai-Ng p. 202: AIC3 = V(k) + k sigma_hat^2 2(N+T-k)/(NT); BIC3 = V(k) + k sigma_hat^2 (N+T-k) ln(NT)/(NT) | AIC/BIC panel variants | xtdhcoint.R:681-687 | `criterion = "aic"` is AIC3 and `"bic"` is BIC3; `penalty` must be NULL for both (error otherwise) | Bai and Ng (2002) define exactly these two | Matched at 1e-10 for the two formulas (1.0.4 audit); the silent switch of 1.0.4 (`aic` with penalty 2 or 3 computing BIC3) removed in 1.0.5 |
| Bai-Ng: "Let kmax be a bounded integer such that r <= kmax"; Westerlund Section 3.4: "we set kmax to five" | kmax convention | xtdhcoint.R:341-349 | Default kmax = 5; capped at min(N, T-1) - 1 with a message | Paper | Verified |
| Bai-Ng p. 203: "When min(N, T) is 40 or larger, the proposed tests give precise estimates of the number of factors. Since our theory is based on large N and T, it is not surprising that for very small N or T, the proposed criteria are inadequate."; Table I (p. 204, r = 1, kmax = 8), rows N = 10, T = 50 and 100: every criterion except BIC3 averages 8.00 (the upper bound); BIC3 averages 7.18 and 5.88 | Small-N guidance | xtdhcoint.R:370-377 warning when N < 10 and nfactors is NULL; `nf_at_kmax` flag whenever nf == kmax | Package choice informed by Bai-Ng | Verified in the 1.0.4 audit: with N = 6 all four criteria select kmax = 5 in 100 percent of 200 draws |
| Bai-Ng p. 203: "each series is demeaned and standardized to have unit variance" | Pre-standardisation in the Bai-Ng simulations | not implemented | Residual series enter PCA raw | Westerlund (2008) does not mention standardisation | Not a discrepancy with Westerlund |

## E. Sample handling

| Paper equation / page | Mathematical expression | Code file:line | Convention adopted | Source of convention | Verification status |
|---|---|---|---|---|---|
| Section 3.1-3.2: balanced panel, t = 1..T for all i, factors F_t common across i at the same t; Remark 5 sums t = 2..T for every i | Time alignment is essential for principal components | xtdhcoint.R:271-312 | Common time window (periods observed by every unit) kept, observations outside it dropped with a message; gaps inside the window, fewer than 10 common periods, or duplicate time stamps within a unit are errors | Package choice consistent with the paper's balanced design | Fixed in 1.0.5 (1.0.4 kept the first Tmin rows by position). Test "unbalanced panels are aligned on the time index": unit 1 observed for t = 11..50 gives the same statistics as the balanced panel restricted to t = 11..50; shuffled rows give identical results. |
| (none) | Missing values | xtdhcoint.R:238-257 | `na.pass` in `model.frame`; any NA in the response, regressors, id or time stops with an error reporting the count and row positions | Package choice (the paper's method defines no treatment of missing values) | Fixed in 1.0.5 (1.0.4 crashed with "subscript out of bounds"); test "missing values are rejected with count and positions" |
| Theorem 1: N and T grow with N/T going to zero | Asymptotic range | xtdhcoint.R:361-376 | Warning when N < 5 or N > T quoting the null rejection rates of a Monte Carlo study of this version (nominal 5 percent, nfactors = 1, 1000 replications for each of two seed sets): DHp 38 to 41 percent at N = 2 (T = 50 and 100), 23 to 24 percent at N = 5 (T = 50 and 100), DHg 57 to 60 percent at N = 25 and T = 20. The 1.0.4 audit rates (padded statistic) are no longer quoted. | Package choice | Verified by test "small N and N > T produce warnings"; raw rates in `xtdhcoint_mc/warning_cells_1.0.5*.csv` of the fix directory. |

## F. Monte Carlo after the fix (1.0.5)

Same draws as the 1.0.4 audit (scripts 05 and the Table I cells of script 02), nominal 5 percent level, DGP of Westerlund (2008, Section 3.4), bandwidth floor(4 (T/100)^(2/9)).

Package against the independent paper construction, nf = 1 fixed, R = 500: identical rejection rates in all 8 cells and max abs difference of the z-statistics 2.0e-13 (1.0.4: rates 0.2 to 6.6 percent against 11 to 23 percent for the paper construction).

Table I cells (kmax = 5, IC_p1, R = 1000; 2 s.e. of the difference of two rates = 0.019):

| Case | T | N | DHg 1.0.5 | DHg 1.0.4 | DHg paper | DHp 1.0.5 | DHp 1.0.4 | DHp paper | mean nf 1.0.5 (1.0.4) |
|---|---|---|---|---|---|---|---|---|---|
| 1 | 100 | 10 | 0.116 | 0.138 | 0.113 | 0.133 | 0.154 | 0.122 | 0.00 (1.02) |
| 1 | 100 | 20 | 0.149 | 0.156 | 0.126 | 0.112 | 0.117 | 0.101 | 0.00 (1.00) |
| 1 | 200 | 10 | 0.083 | 0.091 | 0.111 | 0.123 | 0.148 | 0.114 | 0.00 (1.00) |
| 1 | 200 | 20 | 0.093 | 0.078 | 0.086 | 0.112 | 0.130 | 0.077 | 0.00 (1.00) |
| 2 | 100 | 10 | 0.138 | 0.127 | 0.120 | 0.167 | 0.187 | 0.105 | 1.01 (2.37) |
| 2 | 100 | 20 | 0.150 | 0.156 | 0.121 | 0.135 | 0.131 | 0.078 | 1.00 (1.99) |
| 2 | 200 | 10 | 0.093 | 0.093 | 0.087 | 0.145 | 0.131 | 0.123 | 1.00 (1.74) |
| 2 | 200 | 20 | 0.093 | 0.098 | 0.087 | 0.113 | 0.116 | 0.088 | 1.00 (1.92) |
| 3 | 100 | 10 | 0.138 | 0.127 | 0.104 | 0.178 | 0.182 | 0.094 | 1.01 (2.42) |
| 3 | 100 | 20 | 0.159 | 0.149 | 0.109 | 0.137 | 0.143 | 0.072 | 1.00 (1.99) |
| 3 | 200 | 10 | 0.089 | 0.091 | 0.095 | 0.150 | 0.143 | 0.111 | 1.00 (1.75) |
| 3 | 200 | 20 | 0.094 | 0.101 | 0.108 | 0.111 | 0.110 | 0.083 | 1.00 (1.92) |

The number of factors is now selected correctly in every cell. DHg is within 2 s.e. of the paper's Table I in 7 of 12 cells and DHp in 3 of 12; DHp remains 2 to 8 percentage points above the paper's rates. The remaining gap cannot be attributed from the published text alone (the paper's own code is not available; the choice of residuals in the pooled variance ratio S_n is not stated in the paper, see Section B).
