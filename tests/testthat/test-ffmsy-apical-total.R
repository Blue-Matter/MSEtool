skip_on_cran()

# Two fleets with clearly separated apical ages: young-fish dome and old-fish asymptotic
.SplitApicalOM <- function(nSim = 4, pYear = 3) {
  om <- TwoFleetOM
  om@nSim  <- nSim
  om@pYear <- pYear
  om@Fleet[[1]][[1]]@Selectivity@Pars <- list(L5 = 0.2, LFS = 0.35, Vmaxlen = 0.05)
  om@Fleet[[1]][[2]]@Selectivity@Pars <- list(L5 = 0.9, LFS = 1.3, Vmaxlen = 1)
  om
}

.Deterministic1Area <- function(om) {
  om@Stock[[1]]@SRR@SD <- 0
  om@Stock[[1]]@Spatial <- new('spatial')
  om@Stock[[1]]@Depletion@Final <- 0.4
  om
}

# Apical F of the summed, number-weighted F-at-age per stock [Sim, Stock, Year(, MP)]
.ExpectedApicalF <- function(object, nSim) {
  FApical <- purrr::map2(object@FDeadArea, object@Number, \(FDA, N) {
    FD <- apply(ExtendSims(FDA, nSim), setdiff(names(dimnames(FDA)), 'Fleet'), sum)
    N  <- ExtendSims(N, nSim)
    keep <- setdiff(names(dimnames(N)), 'Area')
    FAge <- apply(FD * N, keep, sum) / apply(N, keep, sum)
    apply(FAge, setdiff(keep, 'Age'), max)
  })
  simplify2array(FApical) |> aperm(c(1, length(dim(FApical[[1]])) + 1, 2:length(dim(FApical[[1]]))))
}

# FMSY [Sim, Year] aligned to the numerator's years
.FMSYAligned <- function(object, Years, nSim) {
  .AlignDenomYears(ExtendSims(object@Reference@MSY@FMSY, nSim), Years)[, 1, ]
}

.ProjFFMSY <- function(mse, MP, Years) {
  df <- F_FMSY(mse, Reduce = FALSE)
  df <- df[df$Period == 'Projection' & df$MP == MP & df$Year %in% Years, ]
  as.vector(tapply(df$Value, df$Sim, mean))
}

test_that("F_FMSY numerator is the apical F of the fleet-total F-at-age", {
  om <- .SplitApicalOM()
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  mse  <- Project(hist, MPs = c('CurrentEffort', 'refFMSY'), parallel = FALSE, silent = TRUE)

  FD  <- ExtendSims(hist@FDeadArea[[1]], om@nSim)
  nYr <- dim(FD)[3]
  ApicalAge <- apply(FD[, , nYr, , 1], c(1, 3), which.max)
  expect_true(all(ApicalAge[, 1] < ApicalAge[, 2]))

  Years <- dimnames(FD)$Year
  FMSY <- .FMSYAligned(hist, Years, om@nSim)
  Exp  <- .ExpectedApicalF(hist, om@nSim)[, 1, ] / FMSY
  FF   <- F_FMSY(hist, df = FALSE)
  expect_equal(unname(FF[, 1, ]), unname(Exp), tolerance = 1e-10)

  OldSum <- ExtendSims(SumOverFleet(hist@FDead), om@nSim)[, 1, ] / FMSY
  Fished <- OldSum > 0
  expect_true(all(FF[, 1, ][Fished] < OldSum[Fished] * 0.999))

  df <- F_FMSY(hist, Reduce = FALSE)
  expect_equal(df$Value[order(df$Year, df$Sim)], as.vector(FF[, 1, ]))

  ExpProj <- .ExpectedApicalF(mse, om@nSim)[, 1, , ] /
    as.vector(.FMSYAligned(hist, dimnames(mse@FDead)$Year, om@nSim))
  FFProj  <- F_FMSY(mse, df = FALSE)
  expect_equal(as.vector(FFProj[, 1, , ]), as.vector(ExpProj), tolerance = 1e-10)
  expect_equal(unname(hist@Reference@MSY@Misc$FCurrent[, 1, 1]),
               unname(.ExpectedApicalF(hist, om@nSim)[, 1, nYr]), tolerance = 1e-10)
})

test_that("F_FMSY handles collapsed Sim dimensions and NA from failed MPs", {
  om <- .SplitApicalOM(nSim = 3)
  set.seed(2)
  hist <- Simulate(om, silent = TRUE)
  mse  <- Project(hist, MPs = c('CurrentEffort', 'refFMSY'), parallel = FALSE, silent = TRUE)

  Single <- mse
  Single@FDeadArea <- purrr::map(mse@FDeadArea, \(a) a[1, , , , , , drop = FALSE])
  Single@Number    <- purrr::map(mse@Number, \(a) a[1, , , , , drop = FALSE])
  FF <- F_FMSY(Single, df = FALSE)
  expect_equal(unname(dim(FF)[1]), 3)
  FMSY <- .FMSYAligned(mse, dimnames(FF)$Year, 3)
  Exp  <- .ExpectedApicalF(Single, 3)[, 1, , ] / as.vector(FMSY)
  expect_equal(unname(FF[, 1, , ]), unname(Exp), tolerance = 1e-10)

  Failed <- mse
  Failed@FDeadArea[[1]][2, , , , , 2] <- NA
  Failed@Number[[1]][2, , , , 2]      <- NA
  FF <- F_FMSY(Failed, df = FALSE)
  expect_true(all(is.na(FF[2, 1, , 2])))
  expect_false(anyNA(FF[-2, 1, , ]))
  expect_false(anyNA(FF[, 1, , 1]))
})

test_that("refFMSY holds F/FMSY at 1 on a deterministic 1-area two-fleet OM", {
  om <- .Deterministic1Area(.SplitApicalOM(nSim = 3, pYear = 20))
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  mse  <- Project(hist, MPs = 'refFMSY', parallel = FALSE, silent = TRUE)
  LastYrs <- max(Years(hist, 'P')) - 4:0
  expect_equal(.ProjFFMSY(mse, 'refFMSY', LastYrs), rep(1, 3), tolerance = 1e-3)
})

test_that("refFMSY holds annual F/FMSY at 1 on a seasonal 1-area two-fleet OM", {
  om <- .Deterministic1Area(.SplitApicalOM(nSim = 2, pYear = 10))
  om@Seasons <- 4
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  mse  <- Project(hist, MPs = 'refFMSY', parallel = FALSE, silent = TRUE)
  LastYrs <- max(floor(Years(hist, 'P'))) - 2:0
  expect_equal(.ProjFFMSY(mse, 'refFMSY', LastYrs), rep(1, 2), tolerance = 1e-3)
})

test_that("F_FMSY of a multi-stock complex uses the controlling stock's apical F of the fleet total", {
  om <- ComplexOM
  om@nSim <- 2
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  FMSY <- .FMSYAligned(hist, dimnames(hist@FDeadArea[[1]])$Year, 2)
  Exp  <- apply(.ExpectedApicalF(hist, 2), c(1, 3), max) / FMSY
  FF   <- F_FMSY(hist, df = FALSE)
  expect_identical(dimnames(FF)$Stock, names(om@Complexes))
  expect_equal(unname(FF[, 1, ]), unname(Exp), tolerance = 1e-10)
})

test_that("FDead/FInteract/FRetain with byFleet = FALSE sum F-at-age over fleets before the apical", {
  om <- .SplitApicalOM(nSim = 3)
  om@Seasons <- 4
  set.seed(3)
  hist <- Simulate(om, silent = TRUE)
  mse  <- Project(hist, MPs = c('CurrentEffort', 'refFMSY'), parallel = FALSE, silent = TRUE)

  .Arr <- function(df, MP = FALSE) {
    df <- df[order(if (MP) match(df$MP, c('CurrentEffort', 'refFMSY')) else 0 * df$Sim, df$Year, df$Sim), ]
    df$Value
  }
  Exp <- .ExpectedApicalF(hist, 3)[, 1, ]
  df  <- FDead(hist, byAge = FALSE, byFleet = FALSE, Reduce = FALSE)
  expect_false('Fleet' %in% names(df))
  expect_equal(.Arr(df), as.vector(Exp), tolerance = 1e-10)
  expect_true(all(.Arr(df) <= .Arr(.SumOverFleet(FDead(hist, byAge = FALSE, byFleet = TRUE, Reduce = FALSE))) + 1e-12))

  for (sl in c('FInteract', 'FRetain')) {
    Area <- hist
    Area@FDeadArea <- slot(hist, paste0(sl, 'Area'))
    df <- do.call(sl, list(hist, byAge = FALSE, byFleet = FALSE, Reduce = FALSE))
    expect_equal(.Arr(df), as.vector(.ExpectedApicalF(Area, 3)[, 1, ]), tolerance = 1e-10)
  }

  df <- FDead(mse, byAge = FALSE, byFleet = FALSE, Reduce = FALSE)
  df <- df[df$Period == 'Projection', ]
  expect_equal(.Arr(df, MP = TRUE), as.vector(.ExpectedApicalF(mse, 3)[, 1, , ]), tolerance = 1e-10)

  ByFleet <- FDead(hist, byAge = FALSE, byFleet = TRUE, Reduce = FALSE)
  expect_equal(ByFleet$Value[order(ByFleet$Fleet, ByFleet$Year, ByFleet$Sim)],
               as.vector(ExtendSims(hist@FDead, 3)))

  expect_s3_class(PlotF(hist, byFleet = FALSE), 'ggplot')
})
