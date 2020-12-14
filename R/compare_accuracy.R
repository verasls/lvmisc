compare_accuracy <- function(...) {
  models <- list(...)
  model_names <- as.character(match.call(expand.dots = FALSE)$`...`)

  model_accuracy <- purrr::map(models, accuracy)
  model_classes <- purrr::map(model_accuracy, attributes)
  model_classes <- purrr::map_chr(model_classes, "model_class")

  model_accuracy <- purrr::map_dfr(model_accuracy, cbind)
  model_info <- data.frame(Model = model_names, Class = model_classes)

  cbind(model_info, model_accuracy)
}
