compare_accuracy <- function(...) {
  models <- list(...)
  return(models)

  model_names <- as.character(match.call(expand.dots = FALSE)$`...`)
  model_classes <- purrr::map_chr(models, class)
  model_info <- data.frame(Model = model_names, Class = model_classes)
  model_accuracy <- purrr::map_dfr(models, accuracy)

  cbind(model_info, model_accuracy)
}
