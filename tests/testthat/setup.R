# `skip_on_cran()` skips whenever NOT_CRAN is not exactly "true", which is the
# case for a bare `testthat::test_dir()` as well as on CRAN. The integration
# tests are the only coverage of Simulate/Project, the effort solvers and
# implementation error, so that run reports all-green having tested none of it.
# `devtools::test()` and `devtools::check()` set NOT_CRAN; a plain `R CMD check`
# does not.
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  message(
    "\n",
    "------------------------------------------------------------------\n",
    "NOT_CRAN is not set: the Simulate/Project integration tests will be\n",
    "SKIPPED. A pass here does not exercise the projection loop.\n",
    "Run them with devtools::test(), or:\n",
    "  NOT_CRAN=true Rscript -e 'testthat::test_dir(\"tests/testthat\")'\n",
    "------------------------------------------------------------------\n"
  )
}

options(warnPartialMatchDollar = TRUE, warnPartialMatchArgs = TRUE)
