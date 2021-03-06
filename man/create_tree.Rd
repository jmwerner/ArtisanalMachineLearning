% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trees.R
\name{create_tree}
\alias{create_tree}
\title{AML CART}
\usage{
create_tree(data, response, evaluation_criterion = sum_of_squares,
  min_obs = 5, max_depth = 8)
}
\arguments{
\item{data}{Input data.frame dimension n x p for training decision tree}

\item{response}{Response vector of size nx1 corresponding to the training
data}

\item{evaluation_criterion}{Function that calculates error criterion for
fitting, defaults to sum of squares}

\item{min_obs}{Minimum observations allowed to end up in a single node,
defaults to 5}

\item{max_depth}{Maximum number of successive splits allowed to happen
in the tree, defaults to 8}
}
\value{
Single decision tree in list format of class aml_tree
}
\description{
Produces a single decision tree.
}

