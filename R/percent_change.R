library(tidyverse)

# Create a test data frame
df <- tibble(
  a = sample(1:10, 10, replace = TRUE),
  b = a * 1.3
)

percent_change <- function(x, ...) {
  UseMethod("percent_change")
}

percent_change.default <- function(baseline, followup) {
  percent_change <- ((followup - baseline) / baseline) * 100
  percent_change
}

percent_change.data.frame <- function(df, baseline, followup) {
  df$percent_change <- ((df[[followup]] - df[[baseline]]) / df[[baseline]]) * 100
  df
}

percent_change(df$a, df$b)
percent_change(df, "a", "b")
