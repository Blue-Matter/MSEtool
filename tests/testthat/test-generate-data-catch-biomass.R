.CatchBiomassOM <- function(DiscardUnits = "Biomass") {
  data(TwoFleetOM, envir = environment())
  om <- TwoFleetOM
  om@nSim  <- 3
  om@pYear <- 3
  nAge <- nAge(om@Stock[[1]])
  om@Fleet[[1]][[2]]@Retention <- Retention(Pars = list(RA50 = 5, RA50_95 = 2, MaxRet = 1))
  om@Fleet[[1]][[2]]@DiscardMortality <- DiscardMortality(MeanAtAge = rep(0.5, nAge))
  for (fl in seq_along(om@Obs[[1]])) {
    for (type in c("Landings", "Discards")) {
      slot(om@Obs[[1]][[fl]], type)@CV   <- 0
      slot(om@Obs[[1]][[fl]], type)@Bias <- rep(1, om@nSim)
    }
    om@Obs[[1]][[fl]]@Discards@Units <- DiscardUnits
  }
  om
}

.SimCatchData <- function(DataList, type, fl, Years) {
  sapply(seq_along(DataList), \(x) slot(DataList[[x]][[1]], type)@Value[as.character(Years), fl])
}

.EngineCatch <- function(object, type, fl, Years, MP = NULL) {
  arr <- slot(object, type)
  arr <- if (is.null(MP)) arr[, , as.character(Years), fl, drop = FALSE] else
    arr[, , as.character(Years), fl, MP, drop = FALSE]
  unname(t(apply(arr, c(1, 3), sum)))
}

test_that("error-free simulated catch biomass matches engine landings and discards", {
  skip_on_cran()
  set.seed(1)
  hist <- Simulate(.CatchBiomassOM(), silent = TRUE)
  mse  <- Project(hist, MPs = "CurrentCatch", parallel = FALSE, silent = TRUE)
  HistYears <- Years(hist, "H")
  ProjYears <- Years(hist, "P")
  PPD <- mse@PPD[[1]]

  expect_true(all(hist@Discards[, , as.character(HistYears)[-1], 2] > 0))

  # size-dependent retention: discards are lighter than the average selected fish
  WSel <- hist@OM@Fleet[[1]][[2]]@WeightFleetSelected
  DN   <- apply(hist@DiscardsAtAge[[1]][, , , 2, , drop = FALSE], 1:3, sum)
  SelW <- t(apply(ArrayMultiply(DN, .ArraySubsetYear(WSel, HistYears)), c(1, 3), sum))
  expect_gt(max(abs(SelW / .EngineCatch(hist, "Discards", 2, HistYears) - 1), na.rm = TRUE), 0.01)

  for (type in c("Landings", "Discards")) {
    for (fl in 1:2) {
      expect_equal(unname(.SimCatchData(hist@Data, type, fl, HistYears)),
                   .EngineCatch(hist, type, fl, HistYears), tolerance = 1e-10,
                   label = paste(type, "hist fleet", fl))
      expect_equal(unname(.SimCatchData(PPD, type, fl, ProjYears)),
                   .EngineCatch(mse, type, fl, ProjYears, MP = 1), tolerance = 1e-10,
                   label = paste(type, "proj fleet", fl))
    }
  }
})

test_that("error-free simulated discards in numbers match engine discards-at-age", {
  skip_on_cran()
  set.seed(1)
  hist <- Simulate(.CatchBiomassOM(DiscardUnits = "Number"), silent = TRUE)
  mse  <- suppressMessages(Project(hist, MPs = "CurrentCatch", parallel = FALSE, silent = TRUE))
  HistYears <- as.character(Years(hist, "H"))
  ProjYears <- as.character(Years(hist, "P"))

  expect_true(all(hist@Data[[1]][[1]]@Discards@Units == "Number"))
  HistN <- unname(t(apply(hist@DiscardsAtAge[[1]][, , HistYears, 2, , drop = FALSE], c(1, 3), sum)))
  ProjN <- unname(t(apply(mse@DiscardsAtAge[[1]][, , ProjYears, 2, , 1, drop = FALSE], c(1, 3), sum)))
  expect_equal(unname(.SimCatchData(hist@Data, "Discards", 2, HistYears)), HistN, tolerance = 1e-10)
  expect_equal(unname(.SimCatchData(mse@PPD[[1]], "Discards", 2, ProjYears)), ProjN, tolerance = 1e-10)
})

test_that("discard bias is conditioned against engine discard biomass", {
  skip_on_cran()
  set.seed(1)
  hist <- Simulate(.CatchBiomassOM(), silent = TRUE)
  HistYears <- Years(hist, "H")
  ProjYears <- Years(hist, "P")
  FleetNames <- FleetNames(hist)

  Observed <- sapply(1:2, \(fl) .EngineCatch(hist, "Discards", fl, HistYears)[, 2])
  dimnames(Observed) <- list(Year = HistYears, Fleet = FleetNames)
  FisheryData <- new("data")
  FisheryData@Discards <- CatchData(Name = FleetNames, Value = Observed)

  for (fl in 1:2) {
    hist@OM@Obs[[1]][[fl]]@Discards@Misc$UserSupplied <- NULL
    hist@OM@Obs[[1]][[fl]]@Discards@Error <- NULL
  }
  cond <- .ConditionObsCatch(hist, FisheryData, HistYears, ProjYears, stocks = 1, i = 1,
                             type = "Discards")
  obs <- cond@OM@Obs[[1]][[2]]@Discards
  Years <- as.character(HistYears)[-1]
  expect_equal(unname(obs@Bias[2]), 1, tolerance = 1e-10)
  expect_equal(unname(obs@Error[2, Years]), rep(1, length(Years)), tolerance = 1e-10)
})
