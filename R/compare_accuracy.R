compare_accuracy <- function(..., rank_by = NULL, quiet = FALSE) {
  if (!is.logical(quiet)) {
    abort_argument_type("quiet", must = "be logical", not = quiet)
  }

  models <- list(...)
  models_name <- as.character(match.call(expand.dots = FALSE)$`...`)

  needs_refit <- purrr::map_lgl(models, get_fit_method)
  models <- purrr::map2(needs_refit, models, refit_lmerMod)

  models_accuracy <- purrr::map(models, accuracy)
  models_class <- purrr::map(models_accuracy, attributes)
  models_class <- purrr::map_chr(models_class, "model_class")

  models_accuracy <- purrr::map_dfr(models_accuracy, cbind)
  models_info <- data.frame(Model = models_name, Class = models_class)

  if (isFALSE(quiet)) {
    warn_compare_accuracy(models, models_class, needs_refit)
  }

  compare <- cbind(models_info, models_accuracy)
  compare <- dplyr::select(
    compare,
    Model, Class,
    tidyselect::starts_with("R2"), tidyselect::everything()
  )
  if (!is.null(rank_by)) {
    if (rank_by %in% names(compare)) {
      dplyr::arrange(compare, .data[[rank_by]])
    } else {
      abort_argument_value("rank_by", names(compare))
    }
  } else {
    compare
  }
}

warn_compare_accuracy <- function(models, models_class, needs_refit) {
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

  if (any(needs_refit)) {
    rlang::warn("Some models were refit using maximum likelihood.")
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

get_fit_method <- function(model) {
  if ("lmerMod" %in% class(model)) {
    REML <- grepl("REML", summary(model)$methTitle)
  } else {
    REML <- FALSE
  }
  REML
}

refit_lmerMod <- function(needs_refit, model) {
  if (isTRUE(needs_refit)) {
    model_formula <- stats::formula(model)
    model_data <- stats::model.frame(model)

    lme4::lmer(model_formula, model_data, REML = FALSE)
  } else {
    model
  }
}
