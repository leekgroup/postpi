% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/postpi_der.R
\name{postpi_der}
\alias{postpi_der}
\title{postpi_der function provides the corrected inference result table using a derivation approach for continuous outcomes}
\usage{
postpi_der(relation_dat, yobs, ypred, valid_dat, inf_formula)
}
\arguments{
\item{relation_dat}{testing set that contains observed outcomes and predicted outcomes (continuous data) or probabilities of predicted outcomes (categorical data)}

\item{yobs}{name of the continuous observed outcome}

\item{ypred}{name of the continuous predicted outcome}

\item{valid_dat}{validation set that contains predicted outcomes and covariates}

\item{inf_formula}{inference formula for fitting predicted outcomes ~ covariates, eg. yp ~ x1}
}
\value{
tidytable a tidy table for inference results. It contains conlumns: term, estimate, std.error, statistic, p.value
}
\description{
This function is required to take in two data sets (i.e. testing set and validation set), names for observed and predicted continuous outcomes, and an inference formula.
The function relates continuous observed and predicted outcomes in the testing set and quantify biases
and then corrects the inference results in the validation set based on the input inference formula
}
\examples{
data(RINdata,package="postpi")

testing    <- RINdata[1:2000,]
validation <- RINdata[2001:nrow(RINdata),]

relation_dat <- data.frame(actual = testing$actual, pred = testing$predictions)
inf_results  <- postpi_der(relation_dat, actual, pred, validation, predictions ~ region_1)

}
