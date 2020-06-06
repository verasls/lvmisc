#' Clean observations
#'
#' @param data A data frame, or data frame extension (e.g. a tibble).
#'
#' @param id The bare (unquoted) name of the column that identifies each
#'   subject
#'
#' @param var The bare (unquoted) name of the column to be cleaned.
#'
#' @param max_na An interger indicating the maximum number of \code{NA}s per 
#'   subject
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
#'   score = sample(c(1:5, rep(NA, 5)), 20, replace = TRUE)
#' )
#' 
#' clean_observations(data, id, score, 1)
clean_observations <- function(data, id, var, max_na) {
  na_count <- dplyr::summarise(
    dplyr::group_by(data, {{ id }}), 
    na = sum(is.na({{ var }})), 
    .groups = "drop"
  )
  na_count <- dplyr::filter(na_count, .data$na > max_na)

  exclude <- rlang::eval_tidy(rlang::enquo(id), na_count)

  dplyr::mutate(data, {{ var }} := replace({{ var }}, id %in% exclude, NA))
}
