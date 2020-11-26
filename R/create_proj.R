create_proj <- function(path) {
  if (dir.exists(path)) {
    usethis::ui_stop("{usethis::ui_path(path)} already exists")
  } else if (file.exists(path)) {
    usethis::ui_stop(
      "{usethis::ui_path(path)} already exists but is not a directory"
    )
  }
  dir.create(path)
  usethis::ui_done(
    "Creating project top-level directory {usethis::ui_path(path)}"
  )

  sub_dirs <- c("code/", "data/", "docs/", "figs/", "tabs/")
  purrr::walk(sub_dirs, ~ create_sub_dir(path, .x))
}

create_sub_dir <- function(main_dir, sub_dir) {
  dir.create(paste0(main_dir, "/", sub_dir))
  usethis::ui_done("Creating sub-directory {usethis::ui_path(sub_dir)}")
}
