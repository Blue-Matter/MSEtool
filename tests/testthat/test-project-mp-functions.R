test_that("SetMPArgs() bakes argument values and keeps class and attributes", {
  Variant <- SetMPArgs(IndexRate, CalibYears = 5, IndexWeight = c(2, 1), TACRange = NULL)
  expect_s3_class(Variant, 'mp')
  expect_identical(formals(Variant)$CalibYears, 5)
  expect_identical(formals(Variant)$IndexWeight, c(2, 1))
  expect_true('TACRange' %in% names(formals(Variant)))
  expect_null(formals(Variant)$TACRange)

  Ref <- SetMPArgs(refFMSY75)
  expect_identical(attr(Ref, 'Interval'), 1)

  WithAttr <- IndexTarget
  attr(WithAttr, 'Interval') <- 3
  WithAttr <- SetMPArgs(WithAttr, Responsiveness = 0.5)
  expect_identical(attr(WithAttr, 'Interval'), 3)
  expect_s3_class(WithAttr, 'mp')

  SelfContained <- .MakeSelfContained(Variant)
  expect_identical(formals(SelfContained)$CalibYears, 5)

  expect_error(SetMPArgs(IndexRate, NotAnArg = 1), 'not an argument')
  expect_error(SetMPArgs(IndexRate, 5), 'must be named')
})

test_that(".ResolveMPs() accepts names, named functions, and a mix", {
  Variant <- SetMPArgs(IndexTarget, Responsiveness = 0.5)

  Res <- .ResolveMPs(c('IndexTarget', 'CurrentCatch'))
  expect_named(Res, c('IndexTarget', 'CurrentCatch'))

  Res <- .ResolveMPs(list(IT05 = Variant, 'CurrentCatch', CC = 'CurrentCatch'))
  expect_named(Res, c('IT05', 'CurrentCatch', 'CC'))
  expect_identical(formals(Res$IT05)$Responsiveness, 0.5)

  expect_error(.ResolveMPs(list(Variant)), 'must be named')
  expect_error(.ResolveMPs(list(A = Variant, A = IndexTarget)), 'Duplicated')
  expect_error(.ResolveMPs(list(A = function(Data) Advice())), 'class')
  expect_error(.ResolveMPs(Variant), 'named list')
  expect_error(.ResolveMPs('NoSuchMPFunction'), 'not found')
})

test_that("Project() with MP functions matches name-based projection and passes advice timing", {
  skip_on_cran()
  OM <- SingleStockOM
  OM@nSim <- 3
  OM@DataLag <- 1
  OM@Interval <- 2
  Hist <- Simulate(OM, silent = TRUE)

  Spy <- function(Data) {
    Advice(TAC = LastTAC(Data),
           Misc = list(Seen = c(Data@Misc$AdviceYear, Data@Misc$Interval, max(Data@Years))))
  }
  class(Spy) <- 'mp'

  ByName <- Project(Hist, MPs = 'IndexTarget', silent = TRUE)
  ByFun  <- Project(Hist, MPs = list(IndexTarget = IndexTarget, Spy = Spy), silent = TRUE)

  expect_identical(names(ByFun@MPs), c('IndexTarget', 'Spy'))

  L1 <- Landings(ByName, df = TRUE)
  L2 <- Landings(ByFun, df = TRUE)
  L1 <- L1[L1$Period == 'Projection', ]
  L2 <- L2[L2$Period == 'Projection' & L2$MP == 'IndexTarget', ]
  expect_equal(L1$Value, L2$Value)

  Seen <- ByFun@Misc$Advice$Spy
  YearsProj <- Years(OM, 'Projection')
  First <- Seen[[as.character(YearsProj[1])]][[1]][[1]]@Misc$Seen
  expect_equal(First[1], YearsProj[1])
  expect_equal(First[2], 2)
  expect_equal(First[3], YearsProj[1] - 2)
})
