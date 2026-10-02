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
