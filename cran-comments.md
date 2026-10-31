## xtdhcoint 1.0.5

CRAN version: 1.0.4.

### Changes

* First-difference regression over t = 2..T, principal components on the (T - 1) x N residual matrix, recumulation from zero (Westerlund 2008, eq. 12 and p. 202); statistics now level-invariant.
* Factor search over k = 0..kmax; nfactors = 0 allowed.
* Unbalanced panels aligned on the time variable (common window); gaps and duplicates are errors.
* Missing values rejected with an error reporting count and row positions.
* `constant` argument removed (it had no effect).
* `penalty` validated against `criterion`; `bandwidth` validated; `bandwidth = 0` used as given.
* `predet` accepts a vector of length K.
* Warnings for N < 5, N > T and for automatic factor selection with N < 10.
* Documentation of `bandwidth = -1` corrected; Bai and Ng (2002) and Newey and West (1994) added to the references.
* Unused imports removed; CITATION updated; validation ledger added; new tests.

### Test environments

* Ubuntu Linux, R 4.3.3 (x86_64-pc-linux-gnu). R-devel was not available in the build environment; the package was checked on R 4.3.3 only.

### R CMD check --as-cran --no-manual results

Checked on R 4.3.3, Linux (x86_64), with `R CMD build` and `R CMD check --as-cran --no-manual` run with `_R_CHECK_SYSTEM_CLOCK_=0` and `LC_ALL=C.UTF-8`.

Result: 0 errors, 0 warnings, 1 NOTE.

NOTE, CRAN incoming feasibility: "Found the following (possibly) invalid URLs: https://github.com/muhammedalkhalaf/xtdhcoint and https://github.com/muhammedalkhalaf/xtdhcoint/issues (Status: 403)". The check machine's network proxy returns 403 for github.com; the URLs are unchanged from version 1.0.4; the build environment could not verify them.
