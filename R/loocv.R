#' Leave-one-out cross-validation
#'
#' @param model A model object.
#' @param data A data frame.
#' @param id The bare (unquoted) name of the column which identifies subjects.
#' @param keep A character string indicating which variables to keep.
#'
#' @export
loocv <- function(model, data, id, keep = "used") {
  UseMethod("loocv")
}

#' @rdname loocv
#' @export
loocv.lm <- function(model, data, id, keep = "used") {
  if (length(class(model)) > 1) {
    classes <- class(model)[class(model) != "lm"]
    abort_class_not_implemented("loocv", classes)
  }

  id <- rlang::enquo(id)
  formula <- stats::formula(model)
  outcome <- as.character(rlang::f_lhs(formula))

  loocv_split <- rsample::group_vfold_cv(data, group = id)
  training_data <- purrr::map(loocv_split$splits, get_training_data)
  testing_data <- purrr::map(loocv_split$splits, get_testing_data)

  trained_models <- purrr::map(training_data, ~ lm(formula, data = .x))
  predicted <- purrr::map2(
    trained_models, testing_data,
    ~ stats::predict(.x, newdata = .y, allow.new.levels = TRUE)
  )
  predicted <- unname(purrr::as_vector(predicted))
  actual <- purrr::as_vector(purrr::map(testing_data, outcome))

  if (keep == "used") {
    id <- rlang::as_string(rlang::ensym(id))
    tibble::tibble(data[id], actual, predicted)
  } else if (keep == "none") {
    tibble::tibble(actual, predicted)
  } else if (keep == "all") {
    tibble::as_tibble(cbind(data, actual, predicted))
  }
}

get_training_data <- function(x) rsample::analysis(x)
get_testing_data <- function(x) rsample::assessment(x)
