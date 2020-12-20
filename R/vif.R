vif <- function(model) {
  var_cov <- stats::vcov(model)
  model_assign <- attributes(stats::model.matrix(model))[["assign"]]

  if (has_intercept(model)) {
    var_cov <- var_cov[- 1, - 1]
    model_assign <- model_assign[- 1]
  } else {
    rlang::warn("Model has no intercept; VIFs may not be sensible.")
  }

  model_terms <- labels(stats::terms(model))
  if (length(model_terms) < 2) {
    rlang::abort(
      "Not enought terms in the model to check for multicollinearity."
    )
  }

  r <- stats::cov2cor(var_cov)
  r_det <- det(r)

  vif <- purrr::map_dbl(
    seq_along(model_terms),
    ~ compute_vif(.x, model_assign, r, r_det)
  )
  names(vif) <- model_terms

  vif
}

has_intercept <- function(model) {
  names(stats::coefficients(model))[1] == "(Intercept)"
}

compute_vif <- function(term, model_assign, r, r_det) {
  subs <- which(model_assign == term)
  det(as.matrix(r[subs, subs])) * det(as.matrix(r[- subs, - subs]))  / r_det
}
