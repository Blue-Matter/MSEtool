# Validates the `MSYRefsCpp` SimControl() option (R/constructor-simcontrol.R,
# wired into R/simulate-om.R): with it TRUE (default), Simulate() computes MSY
# reference points via CalcMSYCpp() instead of CalcMSY(). Unit- and
# CalcMSY()-vs-CalcMSYCpp()-level parity is already covered by
# test-calc-msy-cpp.R; this checks the option is actually wired through the
# public Simulate() entry point, across every built-in `om`-class example
# object.

skip_on_cran()

test_that("MSYRefsCpp defaults to TRUE", {
  expect_true(SimControl()$MSYRefsCpp)
})

test_that("Simulate() gives the same Reference@MSY via MSYRefsCpp = TRUE and FALSE, for all built-in om objects", {
  skip_on_cran()

  om_battery <- list(
    SingleStockOM     = 3,
    MultiStockOM      = 3,
    ComplexOM         = 3,
    HermOM            = 2,
    SeasonalSpatialOM = 2,
    TwoFleetOM        = 3
  )

  minimal_control <- function(use_cpp) {
    SimControl(
      DynamicUnfished = FALSE,
      ConditionObs    = FALSE,
      GenerateData    = FALSE,
      MSYRefs         = TRUE,
      MSYRefsCpp      = use_cpp,
      RefPoints       = FALSE,
      MGT             = FALSE,
      BLow            = FALSE
    )
  }

  for (nm in names(om_battery)) {
    data(list = nm, envir = environment())
    om <- get(nm, envir = environment())
    om@nSim <- om_battery[[nm]]

    set.seed(1)
    hist_r <- Simulate(om, silent = TRUE, control = minimal_control(FALSE))

    set.seed(1)
    hist_cpp <- Simulate(om, silent = TRUE, control = minimal_control(TRUE))

    expect_equal(hist_r@Reference@MSY, hist_cpp@Reference@MSY,
                tolerance = 1e-6, info = nm)
  }
})
