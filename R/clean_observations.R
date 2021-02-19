#' Clean observations
#'
#' Replace valid observations by \code{NA}s when a given subject has more then
#'   \code{max_na} missing values.
#'
#' @param data A data frame, or data frame extension (e.g. a tibble).
#' @param id The bare (unquoted) name of the column that identifies each
#'   subject.
#' @param var The bare (unquoted) name of the column to be cleaned.
#' @param max_na An integer indicating the maximum number of \code{NA}s per
#'   subject.
#'
#' @return The original \code{data} with the \code{var} observations matching
#'   the \code{max_na} criterion replaced by \code{NA}.
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom rlang :=
#'
#' @examples
#' set.seed(10)
#'
#' data <- data.frame(
#'   id = rep(1:5, each = 4),
#'   time = rep(1:4, 5),
#'   score = sample(c(1:5, rep(NA, 2)), 20, replace = TRUE)
#' )
#'
#' clean_observations(data, id, score, 1)
clean_observations <- function(data, id, var, max_na) {
  id_col_name <- rlang::as_string(rlang::ensym(id))
  var_col_name <- rlang::as_string(rlang::ensym(var))
  data_name <- rlang::as_string(rlang::ensym(data))

  if (id_col_name %!in% names(data)) {
    abort_column_not_found(data = data_name, col_name = id_col_name)
  }
  if (var_col_name %!in% names(data)) {
    abort_column_not_found(data = data_name, col_name = var_col_name)
  }
  if (max_na %% 1 != 0) {
    abort_argument_type(arg = "max_na", must = "be interger", not = max_na)
  }

  na_count <- dplyr::summarise(
    dplyr::group_by(data, {{ id }}),
    na = sum(is.na({{ var }})),
    .groups = "drop"
  )
  na_count <- dplyr::filter(na_count, .data$na > max_na)

  exclude <- rlang::eval_tidy(rlang::enquo(id), na_count)

  dplyr::mutate(data, {{ var }} := replace({{ var }}, id %in% exclude, NA))
}
