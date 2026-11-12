library(testthat)
library(MSEtool)

# MSETOOL_TEST_TIER=smoke: CRAN-level tests plus the full smoke files below
if (identical(Sys.getenv("MSETOOL_TEST_TIER"), "smoke")) {
  Sys.setenv(NOT_CRAN = "false")
  test_check("MSEtool")
  Sys.setenv(NOT_CRAN = "true")
  test_check("MSEtool", filter = "^(integration-simulate-project|fleet-controls|tac-unreachable)$")
} else {
  test_check("MSEtool")
}
