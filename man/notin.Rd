% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/operators.R
\name{notin}
\alias{notin}
\alias{\%!in\%}
\title{Value matching}
\usage{
x \%!in\% table
}
\arguments{
\item{x}{Vector with the values to be matched.}

\item{table}{Vector with the values to be matched against.}
}
\value{
A logical vector indicating which values are not in \code{table}.
}
\description{
Value matching
}
\examples{
x <- 8:12
x \%!in\% 1:10
}
\seealso{
\code{\link[base:match]{match()}}.
}
