library(MSEtool)

test.results <- paste0(Sys.Date(), 'MSEtool_V', packageVersion("MSEtool"), '.xml')

options(testthat.output_file = test.results)

testthat::test_dir('tests/manual/test-code', reporter = "junit")

# remove ESC symbol from xml
sed -i -r "s/\x1b/''/g" test.xml   ## get rid of escape symbol


# build test report

test.file <- '2020-06-12DLMtool_V5.4.5.xml'
test.file <- 'test.xml'
rmarkdown::render(input='tests/manual/Test_Report.Rmd', params=list(results.file=test.file))



options(testthat.output_file = NULL)
testthat::test_file("tests/manual/test-code/test-checkPopDynamics.R") # ok

testthat::test_file("tests/manual/test-code/test-Data2csv.R") # ok

testthat::test_file("tests/manual/test-code/test-Data_Functions.R") # ok

testthat::test_file("tests/manual/test-code/test-Data_Plotting.R") # ok

testthat::test_file("tests/manual/test-code/test-Fease_Functions.R") # ok

testthat::test_file("tests/manual/test-code/test-Import_Data.R") # ok

testthat::test_file("tests/manual/test-code/test-MPs.R") # OK

testthat::test_file("tests/manual/test-code/test-MSE_functions.R") # ok

testthat::test_file("tests/manual/test-code/test-MSE_Plotting.R") # cosewic and VOI errors

testthat::test_file("tests/manual/test-code/test-OM_functions.R") # ok

testthat::test_file("tests/manual/test-code/test-OM_init_doc.R") # ok

testthat::test_file("tests/manual/test-code/test-OM_Plotting.R") # ok

testthat::test_file("tests/manual/test-code/test-PMobjects.R") # Ok

testthat::test_file("tests/manual/test-code/test-RealIndices.R") # ok


testthat::test_file("tests/manual/test-code/test-runMSE.R")

testthat::test_file("tests/manual/test-code/test-slotDescription.R") # OK

