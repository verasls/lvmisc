#' Perform a leave-one-out cross-validation
#'
#' @param model A model object.
#' @param data A data frame.
#' @param id The bare (unquoted) name of the column which identifies subjects.
#' @param keep A character string indicating which variables to keep.
#'
#' @export
loocv <- function(model, data, id, keep = "all") {
  UseMethod("loocv")
}

#' @rdname loocv
#' @export
loocv.default <- function(model, data, id, keep = "all") {
  msg <- glue::glue(
    "If you would like it to be implemented, please file an issue at\\
    https://github.com/verasls/lvmisc/issues."
  )
  abort_no_method_for_class("loocv", class(model), msg)
}

#' @rdname loocv
#' @export
loocv.lm <- function(model, data, id, keep = "all") {
  id_col_name <- rlang::as_string(rlang::ensym(id))
  data_name <- rlang::as_string(rlang::ensym(data))
  check_args_loocv(model, data, id, keep, id_col_name, data_name)

  id <- rlang::enquo(id)
  formula <- stats::formula(model)
  outcome <- as.character(rlang::f_lhs(formula))

  splits <- split_data(data, id)
  trained_models <- purrr::map(splits$training_data, ~ lm(formula, data = .x))
  cv_values <- compute_cv_values(
    data, id_col_name, splits$testing_data, trained_models, outcome
  )
  get_loocv_object(cv_values, id, keep)
}

#' @rdname loocv
#' @export
loocv.lmerMod <- function(model, data, id, keep = "all") {
  requireNamespace("lme4", quietly = TRUE)
  id_col_name <- rlang::as_string(rlang::ensym(id))
  data_name <- rlang::as_string(rlang::ensym(data))
  check_args_loocv(model, data, id, keep, id_col_name, data_name)

  id <- rlang::enquo(id)
  formula <- stats::formula(model)
  outcome <- as.character(rlang::f_lhs(formula))

  splits <- split_data(data, id)
  trained_models <- purrr::map(
    splits$training_data,
    ~ lme4::lmer(
      formula, data = .x,
      REML = grepl("REML", summary(model)$methTitle)
    )
  )
  cv_values <- compute_cv_values(
    data, id_col_name, splits$testing_data, trained_models, outcome
  )
  get_loocv_object(cv_values, id, keep)
}

check_args_loocv <- function(model,
                             data,
                             id,
                             keep,
                             id_col_name,
                             data_name) {
  if (length(class(model)) > 1) {
    classes <- class(model)[class(model) %!in% c("lm", "lmerMod")]
    msg <- glue::glue(
      "If you would like it to be implemented, please file an issue at\\
      https://github.com/verasls/lvmisc/issues."
    )
    abort_no_method_for_class("loocv", classes, msg)
  }
  if (!is.data.frame(data)) {
    abort_argument_type(arg = "data", must = "be data.frame", not = data)
  }
  if (id_col_name %!in% names(data)) {
    abort_column_not_found(data = data_name, col_name = id_col_name)
  }
  valid_values <- c("all", "used", "none")
  if (keep %!in% valid_values) {
    abort_argument_value(arg = "keep", valid_values)
  }
}

split_data <- function(data, id) {
  loocv_split <- rsample::group_vfold_cv(data, group = id)
  training_data <- purrr::map(loocv_split$splits, get_training_data)
  testing_data <- purrr::map(loocv_split$splits, get_testing_data)
  list(training_data = training_data, testing_data = testing_data)
}

get_training_data <- function(x) rsample::analysis(x)
get_testing_data <- function(x) rsample::assessment(x)

compute_cv_values <- function(data, id, testing_data, trained_models, outcome) {
  predicted <- purrr::map2(
    trained_models, testing_data,
    ~ stats::predict(.x, newdata = .y, allow.new.levels = TRUE)
  )
  predicted <- unname(
    purrr::as_vector(purrr::map_dfr(predicted, tibble::as_tibble))
  )
  testing_data <- purrr::map_dfr(testing_data, rbind)
  cv_values <- tibble::add_column(
    testing_data,
    ".actual" = testing_data[[outcome]],
    ".predicted" = predicted
  )
  arrange_values(cv_values, data, id)
}

arrange_values <- function(cv_values, data, id) {
  data <- data[id]
  dplyr::full_join(data, cv_values)
}

get_loocv_object <- function(cv_values, id, keep) {
  if (keep == "all") {
    new_loocv(cv_values)
  } else if (keep == "used") {
    vars <- c(
      rlang::as_string(rlang::ensym(id)),
      ".actual", ".predicted"
    )
    new_loocv(cv_values[vars])
  } else if (keep == "none") {
    new_loocv(cv_values[c(".actual", ".predicted")])
  }
}

#' Constructor for loocv object
#'
#' @param x A data.frame.
#' @keywords internal
new_loocv <- function(x) {
  stopifnot(is.data.frame(x))
  stopifnot(".actual" %in% names(x))
  stopifnot(".predicted" %in% names(x))
  n_rows <- nrow(x)
  tibble::new_tibble(x, nrow = n_rows, class = "loocv")
}
