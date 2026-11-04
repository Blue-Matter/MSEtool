test_that("ConvertObs() observes every legacy life-history component", {
  obs <- ConvertObs(testOM, silent = TRUE)
  expect_identical(obs@LifeHistory@Weight, list(Bias = 0))
  expect_true(length(obs@LifeHistory@Length$Bias) > 0)
  expect_equal(obs@CPUE@Ref, testOM@Irefbiascv)
  expect_equal(obs@Survey@Ref, testOM@Irefbiascv)

  Perfect <- Perfect_Info
  Perfect@Mbiascv <- 0
  obs <- ConvertObs(Perfect, silent = TRUE)
  expect_identical(obs@LifeHistory@NaturalMortality, list(Bias = 0))
})

test_that("simulated data converted to legacy Data carry the slots legacy MPs read", {
  skip_on_cran()
  om <- Convert(testOM, silent = TRUE)
  om@nSim <- 2
  hist <- Simulate(om, silent = TRUE)
  expect_identical(length(hist@Data[[1]][[1]]@Reference@FMSY), 0L)
  Data <- MSEtool:::.AddLegacyReference(hist@Data[[1]][[1]], hist, 1, names(hist@Data[[1]])[1])

  expect_true(length(Data@Reference@FMSY) > 0)
  expect_true(length(Data@Reference@Misc$SB0) > 0)
  MSY <- hist@Reference@MSY
  expect_equal(as.numeric(Data@Reference@FMSY), as.numeric(MSY@FMSY[1, 1, dim(MSY@FMSY)[3]]))

  Legacy <- Convert(Data, silent = TRUE)
  for (sl in c('wla', 'wlb', 'Mort', 'FMSY_M', 'BMSY_B0', 'Cref', 'Iref', 'AvC', 'Dt'))
    expect_true(is.finite(slot(Legacy, sl)[1]), info = sl)
  expect_equal(Legacy@FMSY_M * Legacy@Mort, as.numeric(Data@Reference@FMSY))
  expect_equal(Legacy@CV_Mort, 0.2)
  expect_equal(Legacy@CV_wla, 0.1)
  expect_false(anyNA(Legacy@CV_Cat))
  expect_equal(ncol(Legacy@ML), length(Legacy@Year))
  expect_true(any(is.finite(Legacy@ML)))

  NeedsFMSY <- function(x, Data, reps = 1, plot = FALSE) {
    if (is.na(Data@FMSY_M[x])) stop('FMSY_M is missing')
    Rec <- new('Rec')
    Rec@TAC <- 100
    Rec
  }
  class(NeedsFMSY) <- 'MP'
  assign(".NeedsFMSY", NeedsFMSY, envir = globalenv())
  on.exit(rm(".NeedsFMSY", envir = globalenv()), add = TRUE)
  mse <- Project(hist, MPs = '.NeedsFMSY', silent = TRUE)
  expect_false(any(is.na(mse@Landings)))
  expect_length(Filter(MSEtool:::.IsLogEntry, mse@Log$error), 0)
})

test_that("legacy length statistics follow the catch-at-length", {
  Data <- new('Data')
  Data@CAL_mids <- c(10, 20, 30, 40)
  Data@CAL <- array(c(1, 0, 3, 0, 2, 0, 1, 0), dim = c(1, 2, 4))
  Data <- MSEtool:::.LegacyLengthStats(Data)
  n <- c(1, 3, 2, 1)
  expect_equal(Data@ML[1, 1], sum(n * Data@CAL_mids) / sum(n))
  expect_equal(Data@Lc[1, 1], 20)
  expect_equal(Data@Lbar[1, 1], sum(n[2:4] * c(20, 30, 40)) / sum(n[2:4]))
  expect_true(is.na(Data@ML[1, 2]))
})
