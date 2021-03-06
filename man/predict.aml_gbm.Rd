% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trees.R
\name{predict.aml_gbm}
\alias{predict.aml_gbm}
\title{AML GBM predict method}
\usage{
\method{predict}{aml_gbm}(gbm, data, n_trees = NULL)
}
\arguments{
\item{gbm}{Ensemble of trees trained by `aml_gbm`}

\item{data}{Data.frame row of size `1 x p` for prediction}

\item{n_trees}{Number of trees used in prediction, uses all trees by default}
}
\value{
Prediction value
}
\description{
Returns prediction for a GBM when given a row of data.
}

