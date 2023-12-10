
<!-- README.md is generated from README.Rmd. Please edit that file -->

# npboottprmFBar

<!-- badges: start -->
<!-- badges: end -->

The goal of ‘npboottprmFBar’ is to implement the nonparametric bootstrap
test with pooled resampling method, as presented in Dwivedi,
Mallawaarachchi, and Alvarado (2017), for informative hypothesis
testing, as implemented in ‘restriktor’ and outlined in Vanbrabant and
Rosseel (2020).

## Installation

To install the development version of ‘npboottprmFBar’ from GitHub, use
the [devtools](https://devtools.r-lib.org/) package:

``` r
# install.packages("devtools")
devtools::install_github("mightymetrika/npboottprmFBar")
```

## Nonparametric bootstrap t-test

The following example demonstrates how to use the bootFbar() function to
conduct an informative hypothesis test.

``` r
library(npboottprmFBar)

res <- bootFbar(data = iris, formula = Sepal.Length ~ -1 + Species,
                grp = "Species",
                constraints = 'Speciessetosa < Speciesversicolor < Speciesvirginica',
                nboot = 10, conf.level = 0.95, seed = NULL, na_rm = FALSE)

paste0("Type B Test: ", res$pvalueB)
#> [1] "Type B Test: 1"
paste0("Type A Test: ", res$pvalueA)
#> [1] "Type A Test: 0"
```

The non-significant Type B test followed by the significant Type A test
is evidence in favor the order-constrained hypothesis

## References

Dwivedi AK, Mallawaarachchi I, Alvarado LA (2017). “Analysis of small
sample size studies using nonparametric bootstrap test with pooled
resampling method.” Statistics in Medicine, 36 (14), 2187-2205.

Vanbrabant, L., & Rosseel, Y. (2020). An Introduction to Restriktor:
Evaluating informative hypotheses for linear models. In R. van de Schoot
& M. Miocevic (Eds.), Small Sample Size Solutions: A Guide for Applied
Researchers and Practitioners (1st ed., pp. 157 -172). Routledge.
<https://doi.org/10.4324/9780429273872-14>
