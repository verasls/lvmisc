#' Compute body mass index (BMI)
#'
#' \code{bmi} returns the row-wise BMI in kilograms per meter squared.
#'
#' @param data The data frame or tibble with the data to be computed.
#'
#' @param mass,height The bare (unquoted) names of the columns with body mass
#'   and height data. `mass` unit must be kilograms and `height` unit must be
#'   meters. If the `height` unit is centimeters, it is converted to meters
#'   before BMI computation and a warning is shown.
#'
#' @param category If TRUE, the \code{\link[=bmi_cat]{bmi_cat()}} function is called to
#'   classify the BMI category, returning the classification in another column
#'   as a factor; otherwise (the default) BMI category is not classified.
#'
#' @export
#'
#' @seealso \code{\link[=bmi_cat]{bmi_cat()}}
#'
#' @examples
#' df <- dplyr::starwars %>% 
#'   # Converting height to meters
#'   dplyr::mutate(height = height / 100)
#' 
#' bmi(df, mass, height, category = TRUE)
bmi <- function(data, mass, height, category = FALSE) {
  mass <- rlang::enquo(mass)
  height <- rlang::enquo(height)

  if (!is.numeric(rlang::eval_tidy(mass, data))) {
    abort_argument_type(
      "mass",
      must = "be numeric",
      not = rlang::eval_tidy(mass, data)
    )
  }
  if (!is.numeric(rlang::eval_tidy(height, data))) {
    abort_argument_type(
      "height", 
      must = "be numeric",
      not = rlang::eval_tidy(height, data)
    )
  }

  # Deal with heigh in centimeters case
  cm_thrsh <- 3
  mean_height <- mean(rlang::eval_tidy(height, data), na.rm = TRUE)
  if (mean_height > cm_thrsh) {
    msg <- paste(
      "`height` unit is probably centimeters;",
      "converted to meters before computation."
    )
    rlang::warn(msg)
    data <- dplyr::mutate(
      data,
      !! height := !! height / 100
    )
  }

  data <- dplyr::mutate(
    data,
    bmi = !! mass / (!! height * !! height)
  )

  if (mean_height > cm_thrsh) {
    data <- dplyr::mutate(data, !! height := !! height * 100)
  }

  if (category == TRUE) {
    data <- bmi_cat(data, bmi)
  }

  data
}

#' Classify body mass index (BMI) category
#'
#' \code{bmi_cat} returns the row-wise BMI category as factor with 6 levels:
#' \itemize{
#'   \item Underweight (18.5 < BMI)
#'   \item Normal weight (18.5 \eqn{\le} BMI < 25)
#'   \item Overweight (25 \eqn{\le} BMI < 30)
#'   \item Obesity class I (30 \eqn{\le} BMI < 35)
#'   \item Obesity class II (35 \eqn{\le} BMI < 40)
#'   \item Obesity class III (40 \eqn{\le} BMI)
#' }
#'
#' @param data The data frame or tibble with the data to be computed.
#' 
#' @param bmi The bare (unquoted) name of the column with BMI data.
#' 
#' @export
#'
#' @seealso \code{\link[=bmi]{bmi()}}
#'
#' @examples
#' df <- dplyr::starwars %>% 
#'   # Converting height to meters
#'   dplyr::mutate(height = height / 100) %>% 
#'   bmi(mass, height)
#' 
#' bmi_cat(df, bmi)
bmi_cat <- function(data, bmi) {
  bmi <- rlang::enquo(bmi)

  if (!is.numeric(rlang::eval_tidy(bmi, data))) {
    abort_argument_type(
      "bmi",
      must = "be numeric",
      not = rlang::eval_tidy(bmi, data)
    )
  }

  data <- dplyr::mutate(
    data,
    bmi_cat = dplyr::case_when(
      bmi < 18.5 ~ "Underweight",
      bmi >= 18.5 & bmi < 25 ~ "Normal weight",
      bmi >= 25 & bmi < 30 ~ "Overweight",
      bmi >= 30 & bmi < 35 ~ "Obesity class I",
      bmi >= 35 & bmi < 40 ~ "Obesity class II",
      bmi >= 40 ~ "Obesity class III"
    )
  )
  lvls <- c(
    "Underweight", "Normal weight", "Overweight",
    "Obesity class I", "Obesity class II", "Obesity class III"
  )
  data[["bmi_cat"]] <- factor(data[["bmi_cat"]], lvls)
  data
}
