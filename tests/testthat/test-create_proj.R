test_that("error handling works", {
  expect_error(
    suppressMessages(create_proj(1:10)),
    "`path` must be character; not integer.",
    class = "error_argument_type"
  )
  expect_error(
    suppressMessages(create_proj("path/", sub_dirs = FALSE)),
    "`sub_dirs` must be character; not logical.",
    class = "error_argument_type"
  )
  expect_error(
    suppressMessages(create_proj("path/", use_git = "yes")),
    "`use_git` must be logical; not character.",
    class = "error_argument_type"
  )
  expect_error(
    suppressMessages(create_proj("path/", use_gitignore = TRUE)),
    "`use_gitignore` must be character; not logical.",
    class = "error_argument_type"
  )
  expect_error(
    suppressMessages(create_proj("path/", use_readme = 1)),
    "`use_readme` must be logical; not double.",
    class = "error_argument_type"
  )
})

test_that("create_proj() creates the project top-level directory", {
  proj_dir <- suppressMessages(create_proj(tempfile()))
  expect_true(fs::is_dir(proj_dir))
  fs::dir_delete(proj_dir)
})

test_that("create_proj() throws an error if `path` already exists", {
  proj_dir <- tempfile()
  suppressMessages(create_proj(proj_dir))
  expect_error(suppressMessages(create_proj(proj_dir)))
  fs::dir_delete(proj_dir)
})

test_that(
  "create_proj() throws an error if `path` already exists but is
  not a directory", {
    proj_dir <- tempfile()
    file.create(proj_dir)
    expect_error(suppressMessages(create_proj(proj_dir)))
    fs::file_delete(proj_dir)
  }
)

test_that("create_proj() initialises a git repo", {
  proj_dir <- tempfile()
  suppressMessages(create_proj(proj_dir))
  expect_true(".git" %in% list.files(proj_dir, all.files = TRUE))
  fs::file_delete(proj_dir)

  suppressMessages(create_proj(proj_dir, use_git = FALSE))
  expect_false(".git" %in% list.files(proj_dir, all.files = TRUE))
  fs::file_delete(proj_dir)
})

test_that(".gitignore is correctly handled", {
  proj_dir <- tempfile()
  suppressMessages(create_proj(proj_dir))
  expect_true(".gitignore" %in% list.files(proj_dir, all.files = TRUE))
  fs::file_delete(proj_dir)

  suppressMessages(create_proj(proj_dir, use_gitignore = NULL))
  expect_false(".gitignore" %in% list.files(proj_dir, all.files = TRUE))
  fs::file_delete(proj_dir)

  suppressMessages(create_proj(proj_dir, use_git = FALSE))
  expect_false(".gitignore" %in% list.files(proj_dir, all.files = TRUE))
  fs::file_delete(proj_dir)
})

test_that("create_proj() writes a README.md file", {
  proj_dir <- tempfile()
  suppressMessages(create_proj(proj_dir))
  expect_true("README.md" %in% list.files(proj_dir, all.files = TRUE))
  fs::file_delete(proj_dir)

  suppressMessages(create_proj(proj_dir, use_readme = FALSE))
  expect_false("README.md" %in% list.files(proj_dir, all.files = TRUE))
  fs::file_delete(proj_dir)
})

test_that("clean_sub_dir() works", {
  l <- list("/subdir", "subdir/", "subdir", "/subdir/")
  purrr::map(
    l,
    ~ expect_equal(clean_sub_dir(.x), "subdir/")
  )
})
