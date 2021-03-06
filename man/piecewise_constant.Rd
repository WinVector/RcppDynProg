% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vtreat_coders.R
\name{piecewise_constant}
\alias{piecewise_constant}
\title{Piecewise constant fit.}
\usage{
piecewise_constant(varName, x, y, w = NULL)
}
\arguments{
\item{varName}{character, name of variable to work on.}

\item{x}{numeric, input values.}

\item{y}{numeric, values to estimate.}

\item{w}{numeric, weights.}
}
\description{
\code{vtreat} custom coder based on \code{RcppDynProg::solve_for_partition()}.
}
\examples{

piecewise_constant("x", 1:8, c(-1, -1, -1, -1, 1, 1, 1, 1))

}
