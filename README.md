# xtdhcoint

## Overview

The `xtdhcoint` package implements the Durbin-Hausman panel cointegration tests 
of Westerlund (2008). The tests are robust to cross-sectional dependence through 
common factor extraction using principal components.

## Installation

Install from CRAN:

```r
install.packages("xtdhcoint")
```
Or install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("muhammedalkhalaf/xtdhcoint")
```

## Usage

```r
library(xtdhcoint)

# Load example data
data(fisher_panel)

# Run Durbin-Hausman panel cointegration test
result <- xtdhcoint(inflation ~ interest, data = fisher_panel,
                    id = "country", time = "year")

# Print results
print(result)

# Detailed summary with decision table
summary(result)
```

## Test Statistics

The package provides two test statistics:

- **DHg (Group-mean statistic)**: Tests the null of no cointegration against 
  the heterogeneous alternative that at least some units are cointegrated.

- **DHp (Panel statistic)**: Tests the null of no cointegration against the 
  homogeneous alternative that all units are cointegrated with a common 
  autoregressive parameter.

Both statistics are right-tailed tests. Large positive values indicate evidence 
of cointegration.

## Options

- `kmax`: Maximum number of common factors (default: 5)
- `criterion`: Information criterion for factor selection ("ic", "pc", "aic", "bic")
- `penalty`: Penalty type (1, 2, or 3)
- `bandwidth`: Kernel bandwidth for long-run variance (-1 for automatic)

## References

Westerlund, J. (2008). Panel cointegration tests of the Fisher effect. 
*Journal of Applied Econometrics*, 23(2), 193–233. 
[doi:10.1002/jae.963](https://doi.org/10.1002/jae.963)

## License
GPL-3
