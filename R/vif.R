#' Variance inflation factor
#'
#' Computes the variance inflation facotr (VIF). The VIF is a measure of how
#'   much the variance of a regression coefficient is increased due to
#'   collinearity.
#'
#' @param model An object containing a model.
#'
#' @return It returns a \code{data.frame} with three columns: the name of the
#'   model term, the VIF value and its classification (see "Details").
#'
#' @details
#' ## VIF interpretation
#'   As a rule of thumb for the interpretation of the VIF value, a VIF
#'   less than 5 indicates a low correlation of a given model term with the
#'   others, a VIF between 5 and 10 indicates a moderate correlation and a
#'   VIF greater than 10 indicates a high correlation.
#'
#' @references
#'   \itemize{
#'   \item James, G., Witten, D., Hastie, T., & Tibshirani, R. (eds.). (2013).
#'   An introduction to statistical learning: with applications in R. New York:
#'   Springer.
#'   }
#'
#' @export
vif <- function(model) {
  UseMethod("vif")
}

#' @rdname vif
#' @export
vif.default <- function(model) {
  msg <- glue::glue(
    "If you would like it to be implemented, please file an issue at \\
    https://github.com/verasls/lvmisc/issues."
  )
  abort_no_method_for_class("vif", class(model), msg)
}

#' @rdname vif
#' @export
vif.lm <- function(model) {
  compute_vif(model)
}

#' @rdname vif
#' @export
vif.lmerMod <- function(model) {
  compute_vif(model)
}

compute_vif <- function(model) {
  model_terms <- labels(stats::terms(model))
  check_args_vif(model, model_terms)
  var_cov <- as.matrix(stats::vcov(model))
  model_assign <- attributes(stats::model.matrix(model))[["assign"]]

  if (has_intercept(model)) {
    var_cov <- var_cov[- 1, - 1]
    model_assign <- model_assign[- 1]
  } else {
    rlang::warn("`model` has no intercept; VIFs may not be sensible.")
  }

  r <- stats::cov2cor(var_cov)
  det_r <- det(r)

  vif <- purrr::map_dbl(
    seq_along(model_terms),
    ~ compute_term_vif(.x, model_assign, r, det_r)
  )

  data.frame(
    Term = model_terms,
    VIF = vif,
    Classification = purrr::map_chr(vif, classify_vif)
  )
}

check_args_vif <- function(model, model_terms) {
  if (length(class(model)) > 1) {
    classes <- class(model)[class(model) %!in% c("lm", "lmerMod")]
    msg <- glue::glue(
      "If you would like it to be implemented, please file an issue at \\
      https://github.com/verasls/lvmisc/issues."
    )
    abort_no_method_for_class("vif", classes, msg)
  }
  if (length(model_terms) < 2) {
    rlang::abort(
      "Not enought terms in `model` to check for multicollinearity."
    )
  }
}

has_intercept <- function(model) {
  UseMethod("has_intercept")
}

has_intercept.lm <- function(model) {
  names(stats::coefficients(model))[1] == "(Intercept)"
}

has_intercept.lmerMod <- function(model) {
  requireNamespace("lme4", quietly = TRUE)
  names(lme4::fixef(model))[1] == "(Intercept)"
}

compute_term_vif <- function(term, model_assign, r, det_r) {
  subs <- which(model_assign == term)
  det(as.matrix(r[subs, subs])) * det(as.matrix(r[- subs, - subs]))  / det_r
}

classify_vif <- function(vif) {
  if (vif < 5) {
    "Low"
  } else if (vif >= 5 & vif < 10) {
    "Moderate"
  } else if (vif >= 10) {
    "High"
  }
}
