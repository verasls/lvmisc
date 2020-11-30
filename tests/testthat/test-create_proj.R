test_that("error handling works", {
  expect_error(
    create_proj(1:10),
    "`path` must be character; not integer.",
    class = "error_argument_type"
  )
  expect_error(
    create_proj("path/", sub_dirs = FALSE),
    "`sub_dirs` must be character; not logical.",
    class = "error_argument_type"
  )
  expect_error(
    create_proj("path/", use_git = "yes"),
    "`use_git` must be logical; not character.",
    class = "error_argument_type"
  )
  expect_error(
    create_proj("path/", use_gitignore = TRUE),
    "`use_gitignore` must be character; not logical.",
    class = "error_argument_type"
  )
  expect_error(
    create_proj("path/", use_readme = 1),
    "`use_readme` must be logical; not double.",
    class = "error_argument_type"
  )
})

test_that("project top-level directory is created", {
  proj_dir <- create_proj(tempfile())
  fs::is_dir(proj_dir)
  fs::dir_delete(proj_dir)
})
