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
