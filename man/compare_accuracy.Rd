% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_accuracy.R
\name{compare_accuracy}
\alias{compare_accuracy}
\title{Compare models accuracy}
\usage{
compare_accuracy(..., rank_by = NULL, quiet = FALSE)
}
\arguments{
\item{...}{A list of models. The models can be of the same or of different
classes, including \code{lvmisc_cv} class.}

\item{rank_by}{A character string with the name of an accuracy index to rank
the models by.}

\item{quiet}{A logical indicating whether or not to show any warnings. If
\code{FALSE} (the default) no warnings are shown.}
}
\value{
A \code{data.frame} with a model per row and an index per column.
}
\description{
Computes some common model accuracy indices of several different models at
once, allowing model comparison.
}
\examples{
m1 <- lm(Sepal.Length ~ Species, data = iris)
m2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
m3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
compare_accuracy(m1, m2, m3)

if (require(lme4, quietly = TRUE)) {
  mtcars <- tibble::as_tibble(mtcars, rownames = "cars")
  m1 <- lm(Sepal.Length ~ Species, data = iris)
  m2 <- lmer(
    Sepal.Length ~ Sepal.Width + Petal.Length + (1 | Species), data = iris
  )
  m3 <- lm(disp ~ mpg * hp, mtcars)
  cv3 <- loo_cv(m3, mtcars, cars)
  compare_accuracy(m1, m2, cv3, rank_by = "AIC")
}
}
