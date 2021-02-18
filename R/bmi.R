#' Compute body mass index (BMI)
#'
#' \code{bmi} calculates the BMI in kilograms per meter squared.
#'
#' @param mass,height A numerical vector with body mass and height data. `mass`
#'   unit must be kilograms and `height` unit must be meters. If the `height`
#'   unit is centimeters, it is converted to meters before BMI computation and
#'   a warning is shown.
#'
#' @return Returns a double vector with the element-wise body mass index (BMI).
#'
#' @export
#'
#' @seealso \code{\link[=bmi_cat]{bmi_cat()}}
#'
#' @examples
#' mass <- sample(50:100, 20)
#' height <- rnorm(20, mean = 1.7, sd = 0.2)
#'
#' bmi(mass, height)
bmi <- function(mass, height) {
  if (!is.numeric(mass)) {
    abort_argument_type("mass", must = "be numeric", not = mass)
  }
  if (!is.numeric(height)) {
    abort_argument_type("height", must = "be numeric", not = height)
  }
  if (length(mass) != length(height)) {
    abort_argument_diff_length("mass", "height")
  }

  # Deal with height in centimeters case
  cm_thrsh <- 3
  mean_height <- mean(height, na.rm = TRUE)
  if (mean_height > cm_thrsh) {
    msg <- paste(
      "`height` unit is probably centimeters;",
      "converted to meters before computation."
    )
    rlang::warn(msg)
    height <- height / 100
  }

  bmi <- mass / (height ^ 2)
  bmi
}

#' Classify body mass index (BMI) category
#'
#' \code{bmi_cat} returns the element-wise BMI category as factor with 6 levels:
#' \itemize{
#'   \item Underweight (18.5 < BMI)
#'   \item Normal weight (18.5 \eqn{\le} BMI < 25)
#'   \item Overweight (25 \eqn{\le} BMI < 30)
#'   \item Obesity class I (30 \eqn{\le} BMI < 35)
#'   \item Obesity class II (35 \eqn{\le} BMI < 40)
#'   \item Obesity class III (BMI \eqn{\ge} 40)
#' }
#'
#' @param bmi A numeric vector with BMI data. `BMI` unit must be meters per
#'   square meter.
#'
#' @return A vector of class \code{factor} with 6 levels: "Underweight",
#'   "Normal weight", "Overweight", "Obesity class I", "Obesity class II"
#'   and "Obesity class III".
#'
#' @export
#'
#' @seealso \code{\link[=bmi]{bmi()}}
#'
#' @examples
#' mass <- sample(50:100, 20)
#' height <- rnorm(20, mean = 1.7, sd = 0.2)
#' bmi <- bmi(mass, height)
#'
#' bmi_cat(bmi)
bmi_cat <- function(bmi) {
  if (!is.numeric(bmi)) {
    abort_argument_type("bmi", must = "be numeric", not = bmi)
  }

  bmi_cat <- dplyr::case_when(
    bmi < 18.5 ~ "Underweight",
    bmi >= 18.5 & bmi < 25 ~ "Normal weight",
    bmi >= 25 & bmi < 30 ~ "Overweight",
    bmi >= 30 & bmi < 35 ~ "Obesity class I",
    bmi >= 35 & bmi < 40 ~ "Obesity class II",
    bmi >= 40 ~ "Obesity class III"
  )
  lvls <- c(
    "Underweight", "Normal weight", "Overweight",
    "Obesity class I", "Obesity class II", "Obesity class III"
  )
  factor(bmi_cat, lvls)
}
