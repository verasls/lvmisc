#' Quick plotting
#'
#' These functions are intended to be used to quickly generate simple
#'   exploratory plots using the package \code{ggplot2}.
#'
#' @name plots
#'
#' @param data A data frame.
#' @param x,y x and y aesthetics as the bare (unquoted) name of a column in
#'   \code{data}.
#' @param ... Additional arguments to be passed to the \code{ggplot2::aes()}
#'   function.
#' @param bin_width The width of the bins in a histogram. When \code{NULL}
#'   (default), it uses the number of bins in \code{bins} (defaults to 30).
#'   You can also use one of the character strings \code{"Sturges"},
#'   \code{"scott"} or \code{"FD"} to use one of the methods to determine the
#'   bin width as in \href{https://rdrr.io/r/grDevices/nclass.html}{\code{grDevices::nclass.*()}}
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' plot_scatter(mtcars, disp, mpg, color = factor(cyl))
#' plot_line(Orange, age, circumference, colour = Tree)
#' plot_hist(iris, Petal.Width, bin_width = "FD")
#' plot_qq(mtcars, mpg)
NULL

#' @rdname plots
#' @export
plot_scatter <- function(data, x, y, ...) {
  data_name <- rlang::as_string(rlang::ensym(data))
  x_col_name <- rlang::as_string(rlang::ensym(x))
  y_col_name <- rlang::as_string(rlang::ensym(y))
  check_args_plots(data, data_name, x_col_name, y_col_name)

  ggplot2::ggplot(data, ggplot2::aes(x = {{ x }}, y = {{ y }}, ...)) +
    ggplot2::geom_point()
}

#' @rdname plots
#' @export
plot_line <- function(data, x, y, ...) {
  data_name <- rlang::as_string(rlang::ensym(data))
  x_col_name <- rlang::as_string(rlang::ensym(x))
  y_col_name <- rlang::as_string(rlang::ensym(y))
  check_args_plots(data, data_name, x_col_name, y_col_name)

  ggplot2::ggplot(data, ggplot2::aes(x = {{ x }}, y = {{ y }}, ...)) +
    ggplot2::geom_line()
}

#' @rdname plots
#' @export
plot_hist <- function(data, x, bin_width = NULL, ...) {
  data_name <- rlang::as_string(rlang::ensym(data))
  x_col_name <- rlang::as_string(rlang::ensym(x))
  y_col_name <- "not applicable"
  check_args_plots(data, data_name, x_col_name, y_col_name)
  check_args_bin_width(bin_width)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = {{ x }}, ...))
  if (is.null(bin_width)) {
    p + ggplot2::geom_histogram()
  } else {
    type <- get_type(bin_width)
    p + ggplot2::geom_histogram(binwidth = get_bin_width(type))
  }
}

#' @rdname plots
#' @export
plot_qq <- function(data, x, ...) {
  data_name <- rlang::as_string(rlang::ensym(data))
  x_col_name <- rlang::as_string(rlang::ensym(x))
  y_col_name <- "not applicable"
  check_args_plots(data, data_name, x_col_name, y_col_name)

  ggplot2::ggplot(data, ggplot2::aes(sample = {{ x }}, ...)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line()
}

check_args_plots <- function(data, data_name, x_col_name, y_col_name) {
  if (!is.data.frame(data)) {
    abort_argument_type(arg = "data", must = "be data.frame", not = data)
  }
  if (x_col_name %!in% names(data)) {
    abort_column_not_found(data = data_name, col_name = x_col_name)
  }
  if (y_col_name != "not applicable" & y_col_name %!in% names(data)) {
    abort_column_not_found(data = data_name, col_name = y_col_name)
  }
}

check_args_bin_width <- function(bin_width) {
  valid_values1 <- c("Sturges", "scott", "FD")
  valid_values2 <- c("sturges", "Scott", "fd")
  valid_values <- c(valid_values1, valid_values2)
  if (!is.null(bin_width)) {
    if (bin_width %!in% valid_values) {
      abort_argument_value(arg = "bin_width", valid_values1)
    }
  }
}

get_bin_width <- function(type) {
  fun <- switch(
    type,
    Sturges = grDevices::nclass.Sturges,
    scott = grDevices::nclass.scott,
    FD = grDevices::nclass.FD
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
