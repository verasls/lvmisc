create_proj <- function(path, sub_dirs = "default") {
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

  if (sub_dirs == "default") {
    sub_dirs <- c("code/", "data/", "docs/", "figs/", "tabs/")
  }
  purrr::walk(sub_dirs, ~ create_sub_dir(path, .x))
}

create_sub_dir <- function(main_dir, sub_dir) {
  sub_dir <- clean_sub_dir(sub_dir)
  dir.create(paste0(main_dir, "/", sub_dir))
  usethis::ui_done("Creating sub-directory {sub_dir}")
}

clean_sub_dir <- function(sub_dir) {
  if (!grepl("/", sub_dir)) {
    sub_dir <- paste0(sub_dir, "/")
  } else if (grepl("^/", sub_dir) & !grepl("/$", sub_dir)) {
    sub_dir <- paste0(substr(sub_dir, 2, nchar(sub_dir)), "/")
  } else if (!grepl("^/", sub_dir) & grepl("/$", sub_dir)) {
    sub_dir <- sub_dir
  } else if (grepl("^/", sub_dir) & grepl("/$", sub_dir)) {
    sub_dir <- substr(sub_dir, 2, nchar(sub_dir))
  }
}
