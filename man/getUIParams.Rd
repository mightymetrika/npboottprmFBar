% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/persimon_app_helpers.R
\name{getUIParams}
\alias{getUIParams}
\title{Generate UI Parameters for persimon_app Simulation}
\usage{
getUIParams()
}
\value{
A list of 'shiny' input elements including:
\item{M1, M2, M3}{Numeric inputs for means of the three groups}
\item{S1, S2, S3}{Numeric inputs for standard deviations of the three groups}
\item{Sk1, Sk2, Sk3}{Numeric inputs for skewness of the three groups}
\item{n1, n2, n3}{Text inputs for sample sizes of the three groups}
\item{n_simulations}{Numeric input for number of simulation iterations}
\item{nboot}{Numeric input for number of bootstrap iterations}
\item{conf.level}{Numeric input for confidence level}
}
\description{
This internal function creates a list of 'shiny' input elements for the
simulation parameters. It defines the UI components for means, standard
deviations, skewness, sample sizes, number of simulations, number of bootstrap
iterations, and confidence level.
}
\details{
This function is used internally by the 'shiny' app to generate the
UI elements for parameter input.
}
\keyword{internal}
