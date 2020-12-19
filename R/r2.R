r2 <- function(model) {
  UseMethod("r2")
}

r2.default <- function(model) {
  msg <- glue::glue(
    "If you would like it to be implemented, please file an issue at \\
    https://github.com/verasls/lvmisc/issues."
  )
  abort_no_method_for_class("r2", class(model), msg)
}

r2.lm <- function(model) {
  check_args_r2(model)
  R2 <- summary(model)[["r.squared"]]
  R2_adj <- summary(model)[["adj.r.squared"]]

  data.frame(R2, R2_adj)
}

r2.lmerMod <- function(model) {
  check_args_r2(model)
  model_matrix <- stats::model.matrix(model)

  var_mod <- unclass(lme4::VarCorr(model))
  var_fix <- stats::var(as.vector(lme4::fixef(model) %*% t(model_matrix)))
  var_ran <- sum(
    purrr::map_dbl(var_mod, ~ compute_var_ran(.x, model_matrix))
  )
  var_res <- attributes(lme4::VarCorr(model))$sc ^ 2

  R2_marg <- var_fix / (var_fix + var_ran + var_res)
  R2_cond <- (var_fix + var_ran) / (var_fix + var_ran + var_res)

  data.frame(R2_marg, R2_cond)
}

compute_var_ran <- function(var_mod, model_matrix) {
  z <- as.matrix(model_matrix[, rownames(var_mod), drop = FALSE])
  sum(rowSums((z %*% var_mod) * z)) / nrow(model_matrix)
}

check_args_r2 <- function(model) {
  if ("lvmisc_cv" %!in% class(model) & length(class(model)) > 1) {
    classes <- class(model)[class(model) %!in% c("lm", "lmerMod")]
    msg <- glue::glue(
      "If you would like it to be implemented, please file an issue at \\
      https://github.com/verasls/lvmisc/issues."
    )
    abort_no_method_for_class("accuracy", classes, msg)
  }
}
