testthat::context("Import Data objects")

wd <- getwd()

if (basename(wd) == 'MSEtool') {
  path <- "build_tools/Objects/Data"
} else {
  path <- "../../../build_tools/Objects/Data"
}

fls <- list.files(path)

for (fl in fls) {
  testthat::test_that(paste("new('Data') works with ", fl), {
    testthat::expect_error(Data <- new("Data", file.path(path, fl)), NA)
    testthat::expect_error(summary(Data, wait=FALSE), NA)
  })
}
