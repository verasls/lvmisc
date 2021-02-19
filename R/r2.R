#' Compute R squared
#'
#' Returns the R squared values according to the model class.
#'
#' @param model An object containing a model.
#'
#' @return If the model is a linear model, it returns a \code{data.frame}
#'   with the R squared and adjusted R squared values. If the model is a
#'   linear mixed model it return a \code{data.frame} with the marginal and
#'   conditional R squared values as described by Nakagawa and Schielzeth
#'   (2013). See the formulas for the computations in "Details".
#'
#' @details R squared computations.
#' @section R squared:
#'   \deqn{R^2 = \frac{var(\hat{y})}{var(\epsilon)}}
#'   Where \eqn{var(\hat{y})} is the variance explained by the model and
#'   \eqn{var(\epsilon)} is the residual variance.
#' @section Adjusted R squared:
#'   \deqn{R_{adj}^{2} = 1 - (1 - R^2)\frac{n - 1}{n - p - 1}}
#'   Where \eqn{n} is the number of data points and \eqn{p} is the number of
#'   predictors in the model.
#' @section Marginal R squared:
#'   \deqn{R_{marg}^{2} = \frac{var(f)}{var(f) + var(r) + var(\epsilon)}}
#'   Where \eqn{var(f)} is the variance of the fixed effects, \eqn{var(r)} is
#'   the variance of the random effects and \eqn{var(\epsilon)} is the
#'   residual variance.
#' @section Conditional R squared:
#'   \deqn{R_{cond}^{2} = \frac{var(f) + var(r)}{var(f) + var(r) + var(\epsilon)}}
#'
#' @references \itemize{
#'   \item Nakagawa, S., & Schielzeth, H. (2013). A general and simple method
#'   for obtaining R2 from generalized linear mixed-effects models. Methods
#'   in Ecology and Evolution, 4(2), 133–142.
#'   \doi{10.1111/j.2041-210x.2012.00261.x}.
#'  }
#'
#' @export
#'
#' @examples
#' m1 <- lm(Sepal.Length ~ Species, data = iris)
#' r2(m1)
#' if (require(lme4, quietly = TRUE)) {
#'   m2 <- lmer(
#'     Sepal.Length ~ Sepal.Width + Petal.Length + (1 | Species), data = iris
#'   )
#'   r2(m2)
#' }
r2 <- function(model) {
  UseMethod("r2")
}

#' @rdname r2
#' @export
r2.default <- function(model) {
  msg <- glue::glue(
    "If you would like it to be implemented, please file an issue at \\
    https://github.com/verasls/lvmisc/issues."
  )
  abort_no_method_for_class("r2", class(model), msg)
}

#' @rdname r2
#' @export
r2.lm <- function(model) {
  check_args_r2(model)
  R2 <- summary(model)[["r.squared"]]
  R2_adj <- summary(model)[["adj.r.squared"]]

  data.frame(R2, R2_adj)
}

#' @rdname r2
#' @export
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
    abort_no_method_for_class("r2", classes, msg)
  }
}
