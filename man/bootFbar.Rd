% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bootFbar.R
\name{bootFbar}
\alias{bootFbar}
\title{Nonparametric Bootstrap Test with Pooled Resampling for Informative Hypothesis Testing}
\usage{
bootFbar(
  data,
  formula,
  grp,
  constraints,
  nboot = 1000,
  conf.level = 0.95,
  seed = NULL,
  na_rm = FALSE
)
}
\arguments{
\item{data}{A data frame containing the dataset for analysis.}

\item{formula}{An object of class \code{"formula"} (or one that can be coerced to that class):
a symbolic description of the model to be fitted.}

\item{grp}{A string specifying the grouping variable in the data.}

\item{constraints}{A matrix or data frame of constraints for the hypothesis test.}

\item{nboot}{An integer indicating the number of bootstrap resamples (default is 1000).}

\item{conf.level}{A numeric value specifying the confidence level for the interval
(default is 0.95).}

\item{seed}{An optional integer setting the seed for random number generation (default is NULL).}

\item{na_rm}{A logical value indicating whether NA values should be removed (default is FALSE).}
}
\value{
A list containing the following components:

-pvalueA, pvalueB, pvalueF: P-values for the different test types.

-ciA, ciB, ciF: Confidence intervals for the test statistics.

-TsA, TsB, TsF: Bootstrap test statistics.

-modelo: The linear model object.

-ihto: The initial iht computation result.
}
\description{
Performs a nonparametric bootstrap test with pooled resampling based on the methods
described by Dwivedi et al. (2017) and using the \code{restriktor::iht} function as
proposed by Vanbrabant and Rosseel (2020).
}
\examples{
bootFbar(data = iris, formula = Sepal.Length ~ -1 + Species,
         grp = "Species",
         constraints = 'Speciessetosa < Speciesversicolor < Speciesvirginica',
         nboot = 10, conf.level = 0.95, seed = NULL, na_rm = FALSE)

}
\references{
Dwivedi, A. K., Mallawaarachchi, I., & Alvarado, L. A. (2017). Analysis of
small sample size studies using nonparametric bootstrap test with pooled
resampling method. Statistics in Medicine, 36(14), 2187–2205.
https://doi.org/10.1002/sim.7263

Vanbrabant, L., & Rosseel, Y. (2020). An Introduction to Restriktor: Evaluating
informative hypotheses for linear models. In R. van de Schoot & M. Miocevic
(Eds.), Small Sample Size Solutions: A Guide for Applied Researchers and
Practitioners (1st ed., pp. 157 -172). Routledge. https://doi.org/10.4324/9780429273872-14
}
