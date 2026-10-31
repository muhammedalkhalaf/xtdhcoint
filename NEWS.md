# xtdhcoint 1.0.5

## Breaking changes

* The first-difference regression of Westerlund (2008, eq. 12) is now run over t = 2, ..., T on T - 1 first differences, the principal components are extracted from the (T - 1) x N residual matrix, and the defactored differences are recumulated from zero (e_i1 = 0). The previous code prepended a zero row before differencing, so the first "difference" was the level y_1 - x_1'b. All values of `dhg`, `dhp`, `dhg_z`, `dhp_z` and the p-values change; the statistics are now invariant to adding a constant to the response or the regressors.
* The number of factors is selected over k = 0, 1, ..., kmax instead of k = 1, ..., kmax (Westerlund 2008, Remark 5; Bai and Ng 2002, Theorem 2). `nf = 0` means that no factor is removed. `nfactors = 0` is accepted.
* Unbalanced panels are aligned on the `time` variable: the common time window (periods at which every unit is observed) is kept and observations outside it are dropped with a message. The previous code kept the first Tmin rows of each unit regardless of calendar time. A common window with internal gaps, or with fewer than 10 periods, is an error. Duplicate time values within a unit are an error.
* The `constant` argument has been removed. It had no effect: the intercept was dropped in both branches, and the method has no constant to estimate.
* `penalty` defaults to `NULL` (meaning 1 for `criterion = "ic"` and `"pc"`). Supplying `penalty` with `criterion = "aic"` or `"bic"` is an error; before, `criterion = "aic", penalty = 2` or `3` computed BIC3 and `criterion = "bic"` overrode the supplied penalty. The returned `penalty` is `NA` for `"aic"` and `"bic"`.
* `bandwidth = 0` is used as given (contemporaneous variance only); before it was changed to 1. Values other than -1 or an integer between 0 and T - 2 are an error.
* `kmax` is reduced to min(N, T - 1) - 1 (before: min(N, T) - 1) because the principal components matrix has T - 1 rows.
* `nf_at_kmax` is `TRUE` whenever the criterion selects `kmax`, including `kmax = 1`.

## Other changes

* Missing values in the response, the regressors, `id` or `time` stop the function with an error that reports their count and row positions. Before, an interior NA gave "subscript out of bounds".
* Exactly collinear first-differenced regressors stop the function with an error naming the unit.
* `predet` accepts a vector of length K; a scalar is applied to every regressor.
* A warning is issued when N < 5 or N > T, quoting the null rejection rates of a Monte Carlo study of this version (nominal 5 percent, one factor fixed): 38 to 41 percent for DHp with N = 2, 23 to 24 percent for DHp with N = 5 and 57 to 60 percent for DHg with N = 25 and T = 20. The warning is issued when N < 10 and the number of factors is selected automatically (Bai and Ng 2002, p. 203). The N < 10 note was a message before.
* The documentation of `bandwidth = -1` now states the rule used, floor(4 (T/100)^(2/9)) from Westerlund (2008, Section 3.4).
* New result elements `time_window` and `n_dropped`; `print` shows the common time window when observations were dropped and marks `nf = 0`.
* Unused imports `arima.sim` and `rnorm` removed.
* inst/CITATION updated to version 1.0.5 and to the author name used in DESCRIPTION.
* New tests: hand rebuild of DHg and DHp with `lm.fit`, `eigen` and `cumsum` (tolerance 1e-8), level invariance, k = 0 selection on a no-factor DGP, NA error, unbalanced-panel alignment, argument validation. The 1.0.4 tests that expected a message for N < 10 and an error for `nfactors = 0` were changed accordingly.
* Validation ledger added in inst/validation/ledger_xtdhcoint.md.

# xtdhcoint 1.0.4

* The message printed when K > 1 regressors are supplied ("Simulated moments are derived for K=1 ... consider running separate bivariate tests") was wrong and has been replaced. Westerlund (2008, Theorem 1 and Remark 3) shows that the limiting distributions of DHg and DHp do not depend on the regressors, so the simulated moments of Remark 2 apply for any K and there is a single set of critical values. The new note only recalls Assumption 1(d): the regressors must not be cointegrated among themselves. No numerical change.
* New argument `nfactors` fixes the number of common factors and bypasses the Bai and Ng (2002) information criteria.
* With fewer than 10 cross-sectional units the function now notes that the information criteria are unreliable and reports whether the selected number of factors hit the upper bound `kmax` (new result elements `nf_at_kmax` and `nfactors`; shown by `print`). With N = 6 the IC criterion selected `kmax` in every configuration tried, and the standardized statistics changed substantially with the number of factors.
* Added testthat tests covering the three points above.

# xtdhcoint 1.0.3

* Removed an empty item from the author list of `xtdhcoint-package.Rd`, which caused an HTML validation NOTE ("trimming empty <li>"). No changes to code.

# xtdhcoint 1.0.2

* Corrected the DOI of Westerlund (2008) to 10.1002/jae.967 in DESCRIPTION, README, R and Rd files.
* Authors@R updated; a former contributor entry was removed.

# xtdhcoint 1.0.0

* Initial CRAN release.
* Main function `xtdhcoint()` for Durbin-Hausman panel cointegration tests.
* DHg (group-mean) and DHp (panel) test statistics.
* Automatic factor number selection via information criteria (IC, PC, AIC, BIC).
* Long-run variance estimation using Bartlett kernel.
* Cross-sectional dependence handled through common factor extraction.
* Example dataset `fisher_panel` for testing Fisher effect.
