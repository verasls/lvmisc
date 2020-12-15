compare_accuracy <- function(..., quiet = FALSE) {
  models <- list(...)
  models_name <- as.character(match.call(expand.dots = FALSE)$`...`)

  models_accuracy <- purrr::map(models, accuracy)
  models_class <- purrr::map(models_accuracy, attributes)
  models_class <- purrr::map_chr(models_class, "model_class")

  models_accuracy <- purrr::map_dfr(models_accuracy, cbind)
  models_info <- data.frame(Model = models_name, Class = models_class)

  if (isFALSE(quiet)) {
    warn_compare_accuracy(models, models_class)
  }

  cbind(models_info, models_accuracy)
}

warn_compare_accuracy <- function(models, models_class) {
  models_response <- purrr::map2(models, models_class, get_model_response)
  same_response <- all(
    purrr::map_lgl(models_response, ~ identical(.x, models_response[[1]]))
  )
  if (isFALSE(same_response)) {
    rlang::warn("Not all models have the same response variable.")
  }

  same_class <- all(
    purrr::map_lgl(models_class, ~ identical(.x, models_class[[1]]))
  )
  if (isFALSE(same_class)) {
    rlang::warn("Not all models are of the same class.")
  }
}

get_model_response <- function(model, model_class) {
  if (grepl("lvmisc_cv_model", model_class)) {
    model <- attributes(model)$lvmisc_cv_model
  }

  formula <- stats::formula(model)
  outcome <- as.character(rlang::f_lhs(formula))

  model_data <- stats::model.frame(model)
  model_data[[outcome]]
}
