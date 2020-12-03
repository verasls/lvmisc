#' Perform a leave-one-out cross-validation
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
loocv.default <- function(model, data, id, keep = "used") {
  abort_class_not_implemented("loocv", class(model))
}

#' @rdname loocv
#' @export
loocv.lm <- function(model, data, id, keep = "used") {
  id_col_name <- rlang::as_string(rlang::ensym(id))
  data_name <- rlang::as_string(rlang::ensym(data))
  check_args_loocv(model, data, id, keep, id_col_name, data_name)

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
    new_loocv(tibble::tibble(data[id], actual, predicted))
  } else if (keep == "none") {
    new_loocv(tibble::tibble(actual, predicted))
  } else if (keep == "all") {
    new_loocv(tibble::as_tibble(cbind(data, actual, predicted)))
  }
}

#' @rdname loocv
#' @export
loocv.lmerMod <- function(model, data, id, keep = "used") {
  requireNamespace("lme4", quietly = TRUE)
  id_col_name <- rlang::as_string(rlang::ensym(id))
  data_name <- rlang::as_string(rlang::ensym(data))
  check_args_loocv(model, data, id, keep, id_col_name, data_name)

  id <- rlang::enquo(id)
  formula <- stats::formula(model)
  outcome <- as.character(rlang::f_lhs(formula))

  loocv_split <- rsample::group_vfold_cv(data, group = id)
  training_data <- purrr::map(loocv_split$splits, get_training_data)
  testing_data <- purrr::map(loocv_split$splits, get_testing_data)

  trained_models <- purrr::map(
    training_data,
    ~ lme4::lmer(
      formula, data = .x,
      REML = grepl("REML", summary(model)$methTitle)
    )
  )
  predicted <- purrr::map2(
    trained_models, testing_data,
    ~ stats::predict(.x, newdata = .y, allow.new.levels = TRUE)
  )
  predicted <- unname(purrr::as_vector(predicted))
  actual <- purrr::as_vector(purrr::map(testing_data, outcome))

  if (keep == "used") {
    id <- rlang::as_string(rlang::ensym(id))
    new_loocv(tibble::tibble(data[id], actual, predicted))
  } else if (keep == "none") {
    new_loocv(tibble::tibble(actual, predicted))
  } else if (keep == "all") {
    new_loocv(tibble::as_tibble(cbind(data, actual, predicted)))
  }
}

get_training_data <- function(x) rsample::analysis(x)
get_testing_data <- function(x) rsample::assessment(x)

check_args_loocv <- function(model,
                             data,
                             id,
                             keep = "used",
                             id_col_name,
                             data_name) {
  if (length(class(model)) > 1) {
    classes <- class(model)[class(model) != "lm"]
    abort_class_not_implemented("loocv", classes)
  }
  if (!is.data.frame(data)) {
    abort_argument_type(arg = "data", must = "be data.frame", not = data)
  }
  if (id_col_name %!in% names(data)) {
    abort_column_not_found(data = data_name, col_name = id_col_name)
  }
}

#' Constructor for loocv object
#'
#' @param x A data.frame.
#' @keywords internal
new_loocv <- function(x) {
  stopifnot(is.data.frame(x))
  stopifnot("actual" %in% names(x))
  stopifnot("predicted" %in% names(x))
  n_rows <- nrow(x)
  tibble::new_tibble(x, nrow = n_rows, class = "loocv")
}
