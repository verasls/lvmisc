create_proj <- function(path) {
  if (dir.exists(path)) {
    usethis::ui_stop("{usethis::ui_path(path)} already exists")
  } else if (file.exists(path)) {
    usethis::ui_stop(
      "{usethis::ui_path(path)} already exists but is not a directory"
    )
  }
  dir.create(path)
  dir.create(paste0(path, "/code"))
  dir.create(paste0(path, "/data"))
  dir.create(paste0(path, "/docs"))
  dir.create(paste0(path, "/figs"))
  dir.create(paste0(path, "/tables"))
  usethis::ui_done("Creating path")
}
