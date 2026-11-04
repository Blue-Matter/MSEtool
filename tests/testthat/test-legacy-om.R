test_that("legacy OM is built from Stock, Fleet, Obs and Imp objects", {
  OM <- new("OM", Albacore, Generic_Fleet, Generic_Obs, Perfect_Imp)
  expect_s4_class(OM, "OM")
  expect_equal(OM@M, Albacore@M)
  expect_equal(OM@L5, Generic_Fleet@L5)
  expect_equal(OM@Cobs, Generic_Obs@Cobs)
  expect_equal(OM@TACFrac, Perfect_Imp@TACFrac)
})

test_that("SubOM extracts and Replace swaps each OM component", {
  parts <- list(Stock = Bluefin_tuna, Fleet = DecE_NDom, Obs = Imprecise_Biased, Imp = Overages)
  for (type in names(parts)) {
    obj <- SubOM(testOM, type)
    expect_s4_class(obj, type)

    OMnew <- suppressMessages(Replace(testOM, parts[[type]], type, silent = TRUE))
    newobj <- SubOM(OMnew, type)
    keep <- setdiff(slotNames(newobj), c("Name", "Species", "Region", "Agency",
                                         "Latitude", "Longitude", "Source", "Common_Name"))
    for (nm in keep)
      expect_equal(slot(newobj, nm), slot(parts[[type]], nm), info = paste(type, nm))

    other <- setdiff(names(parts), type)[1]
    expect_equal(SubOM(OMnew, other), SubOM(testOM, other), info = paste(type, other))
  }
})

test_that("SubCpars subsets simulations in cpars", {
  OM <- .LegacyOM(nsim = 4)
  OM@cpars <- list(M = c(0.2, 0.3, 0.4, 0.5), D = c(0.2, 0.3, 0.4, 0.5))
  sub <- suppressMessages(SubCpars(OM, sims = c(2, 4)))
  expect_equal(sub@nsim, 2)
  expect_equal(sub@cpars$M, c(0.3, 0.5))
  expect_equal(sub@cpars$D, c(0.3, 0.5))
})

test_that("SampleStockPars and SampleFleetPars return per-simulation parameters", {
  OM <- .LegacyOM(nsim = 5)
  set.seed(OM@seed)
  SP <- SampleStockPars(OM, OM@nsim, OM@nyears, OM@proyears, msg = FALSE)
  nAge <- OM@maxage + 1
  allyears <- OM@nyears + OM@proyears

  for (nm in c("M", "R0", "hs", "D", "Linf", "K"))
    expect_length(SP[[nm]], OM@nsim)
  expect_true(all(SP$M >= OM@M[1] & SP$M <= OM@M[2]))
  expect_true(all(SP$D >= OM@D[1] & SP$D <= OM@D[2]))
  expect_equal(dim(SP$Len_age), c(OM@nsim, nAge, allyears))
  expect_equal(dim(SP$Mat_age), c(OM@nsim, nAge, allyears))
  expect_true(all(SP$Mat_age >= 0 & SP$Mat_age <= 1))

  FP <- SampleFleetPars(SubOM(OM, "Fleet"), Stock = SP, OM@nsim, OM@nyears, OM@proyears, msg = FALSE)
  expect_equal(dim(FP$V), c(OM@nsim, nAge, allyears))
  expect_true(all(FP$V >= 0 & FP$V <= 1 + 1e-8))
  expect_equal(dim(FP$Find), c(OM@nsim, OM@nyears))
})

test_that("SampleStockPars is reproducible and honours cpars", {
  OM <- .LegacyOM(nsim = 3)
  set.seed(1)
  SP1 <- SampleStockPars(OM, OM@nsim, OM@nyears, OM@proyears, msg = FALSE)
  set.seed(1)
  SP2 <- SampleStockPars(OM, OM@nsim, OM@nyears, OM@proyears, msg = FALSE)
  expect_identical(SP1$M, SP2$M)
  expect_identical(SP1$Linf, SP2$Linf)

  cpars <- list(M = c(0.21, 0.22, 0.23), Linf = c(101, 102, 103))
  SP3 <- SampleStockPars(OM, OM@nsim, OM@nyears, OM@proyears, cpars = cpars, msg = FALSE)
  expect_equal(SP3$M, cpars$M)
  expect_equal(SP3$Linf, cpars$Linf)
})

test_that("SampleObsPars and SampleImpPars return per-simulation parameters", {
  OM <- .LegacyOM(nsim = 4)
  SP <- SampleStockPars(OM, OM@nsim, OM@nyears, OM@proyears, msg = FALSE)
  ObsPars <- SampleObsPars(OM, OM@nsim, Stock = SP, nyears = OM@nyears, proyears = OM@proyears)
  expect_length(ObsPars$Csd, OM@nsim)
  expect_true(all(ObsPars$Csd >= OM@Cobs[1] & ObsPars$Csd <= OM@Cobs[2]))

  ImpPars <- SampleImpPars(OM, OM@nsim, nyears = OM@nyears, proyears = OM@proyears)
  expect_length(ImpPars$TACFrac, OM@nsim)
  expect_true(all(ImpPars$TACFrac >= OM@TACFrac[1] & ImpPars$TACFrac <= OM@TACFrac[2]))
})

test_that("validcpars lists the valid custom parameters", {
  vc <- validcpars("Stock", show = FALSE)
  expect_s3_class(vc, "data.frame")
  expect_true(all(c("M", "Linf", "D") %in% vc[["Var."]]))
})

test_that("CheckOM_legacy accepts a complete OM and rejects missing slots", {
  expect_s4_class(CheckOM_legacy(testOM, msg = FALSE), "OM")
  OM <- testOM
  OM@M <- numeric(0)
  expect_error(CheckOM_legacy(OM, msg = FALSE))
})

test_that("tinyErr removes observation and implementation error", {
  OM <- suppressMessages(tinyErr(testOM, silent = TRUE))
  for (nm in c("Cobs", "Cbiascv", "Iobs", "Btobs"))
    expect_equal(slot(OM, nm), slot(Perfect_Info, nm), info = nm)
  expect_equal(OM@TACSD, c(0, 0))
  expect_equal(OM@TACFrac, c(1, 1))
  expect_equal(OM@Perr, c(0, 0))
})
