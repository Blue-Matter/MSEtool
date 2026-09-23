test_that("%||NA% falls back on NULL and on a length-1 NA, otherwise returns x", {
  `%||NA%` <- MSEtool:::`%||NA%`

  expect_identical(NULL %||NA% "default", "default")
  expect_identical(NA %||NA% "default", "default")
  expect_identical(NA_real_ %||NA% 1, 1)
  expect_identical(NA_character_ %||NA% "default", "default")

  expect_identical(5 %||NA% 1, 5)
  expect_identical("value" %||NA% "default", "value")
  expect_identical(FALSE %||NA% TRUE, FALSE)
})

test_that("%||NA% does not coalesce a vector of length > 1, even if it contains NA", {
  `%||NA%` <- MSEtool:::`%||NA%`

  x <- c(NA_real_, 2, 3)
  expect_identical(x %||NA% 0, x)
})

test_that("%||NA% treats list x like the documented behavior of is.na(), with no warning", {
  `%||NA%` <- MSEtool:::`%||NA%`

  expect_no_warning(out <- list() %||NA% "default")
  expect_identical(out, list())

  expect_no_warning(out <- list(NA) %||NA% "default")
  expect_identical(out, "default")

  expect_no_warning(out <- list(1, 2) %||NA% "default")
  expect_identical(out, list(1, 2))
})

test_that("%||NA% never calls is.na() on an S4 object and returns it unchanged", {
  `%||NA%` <- MSEtool:::`%||NA%`

  obj <- Ages(MaxAge = 10, MinAge = 0)
  expect_true(isS4(obj))
  expect_identical(length(obj), 1L)

  expect_no_warning(out <- obj %||NA% "default")
  expect_identical(out, obj)
})

test_that("%||NA% never calls is.na() on a closure or environment", {
  `%||NA%` <- MSEtool:::`%||NA%`

  fn <- function(x) x
  expect_no_warning(out <- fn %||NA% "default")
  expect_identical(out, fn)

  env <- new.env()
  expect_no_warning(out <- env %||NA% "default")
  expect_identical(out, env)
})
