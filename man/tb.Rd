% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{tb}
\alias{tb}
\title{Capture a backtrace}
\usage{
tb(...)
}
\arguments{
\item{...}{Passed to \code{\link[rlang:trace_back]{rlang::trace_back()}}.}
}
\value{
An object of class \code{rlang_trace}.
}
\description{
Captures the sequence of calls that lead to the current function. It is just
a wrapper to \code{\link[rlang:trace_back]{rlang::trace_back()}}.
}
