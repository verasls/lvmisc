plot_scatter <- function(data, x, y, ...) {
  ggplot2::ggplot(data, ggplot2::aes(x = {{ x }}, y = {{ y }}, ...)) +
    ggplot2::geom_point()
}

plot_line <- function(data, x, y, ...) {
  ggplot2::ggplot(data, ggplot2::aes(x = {{ x }}, y = {{ y }}, ...)) +
    ggplot2::geom_line()
}

plot_hist <- function(data, x, bin_width = NULL, ...) {
  p <- ggplot2::ggplot(data, ggplot2::aes(x = {{ x }}, ...))
  if (is.null(bin_width)) {
    p + ggplot2::geom_histogram()
  } else {
    type <- get_type(bin_width)
    return(type)
    p + ggplot2::geom_histogram(binwidth = get_bin_width(type))
  }
}

plot_qq <- function(data, sample, ...) {
  ggplot2::ggplot(data, ggplot2::aes(sample = {{ sample }}, ...)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line()
}

get_bin_width <- function(type) {
  fun <- switch(
    type,
    Sturges = nclass.Sturges,
    scott = nclass.scott,
    FD = nclass.FD
  )

  function(x) {
    (max(x) - min(x)) / fun(x)
  }
}

get_type <- function(type) {
  if (grepl("scott", type, ignore.case = TRUE)) {
    "scott"
  } else if (grepl("Sturges", type, ignore.case = TRUE)) {
    "Sturges"
  } else if (grepl("FD", type, ignore.case = TRUE)) {
    "FD"
  }
}
