r2 <- function(model) {
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
