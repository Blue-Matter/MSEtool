testthat::context("OM_init_doc")

library('MSEtool')
name <- 'TEST'

wd <- getwd()

dir.create(name)
setwd(name)

testthat::test_that("OM_init works", {
  testthat::expect_error(OMinit(name, overwrite = TRUE), NA)
  testthat::expect_error(OMinit(name, MSEtool::Albacore, overwrite = TRUE), NA)
  testthat::expect_error(OMinit(name, MSEtool::Generic_DecE, overwrite = TRUE), NA)
  testthat::expect_error(OMinit(name, MSEtool::Generic_Obs, overwrite = TRUE), NA)
  testthat::expect_error(OMinit(name, MSEtool::Overages, overwrite = TRUE), NA)
  testthat::expect_error(OMinit(name, MSEtool::testOM, overwrite = TRUE), NA)
})


testthat::test_that("OM_doc works from XL", {
  testthat::expect_error(OMdoc(name, openFile=FALSE, quiet=TRUE), NA)
})

testthat::test_that("XL2OM works", {
  testthat::expect_error(OM <<- XL2OM(name), NA)
})


testthat::test_that("OM_doc works with OM", {
  testthat::expect_error(OMdoc(OM, openFile=FALSE, quiet=TRUE), NA)
})

setwd(wd)

# file clean-up
unlink(name, recursive = TRUE)
