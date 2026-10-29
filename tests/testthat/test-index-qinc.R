.SetIndexqInc <- function(om, fl, type, qInc) {
  io <- slot(om@Obs[[1]][[fl]], type)
  io@qInc <- qInc
  slot(om@Obs[[1]][[fl]], type) <- io
  om
}

.RunIndexqInc <- function(om, fl, type, qInc, MPs = "CurrentEffort") {
  om <- .SetIndexqInc(om, fl, type, qInc)
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  set.seed(2)
  mse <- Project(hist, MPs = MPs, parallel = FALSE, silent = TRUE)
  list(hist = hist, mse = mse)
}

# [DataYear x Sim] projected index values of the first MP
.ProjIndexValues <- function(mse, fl, type, Years) {
  sapply(seq_along(mse@PPD[[1]]), \(s)
    slot(mse@PPD[[1]][[s]][[1]], type)@Value[as.character(Years), fl])
}

.ExpectIndexqInc <- function(With, Without, fl, type, qInc) {
  hist   <- With$hist
  ObsW   <- slot(hist@OM@Obs[[1]][[fl]], type)
  ObsWO  <- slot(Without$hist@OM@Obs[[1]][[fl]], type)
  for (sl in c("Efficiency", "Beta", "Error", "Ref", "AC", "CV", "Stats"))
    expect_identical(slot(ObsW, sl), slot(ObsWO, sl), label = sl)
  expect_identical(hist@Data, Without$hist@Data)
  expect_equal(as.numeric(ObsW@qInc), rep_len(qInc, nSim(hist)))

  LastHist  <- max(Years(hist, "H"))
  DataYears <- utils::head(Years(hist, "P"), -1)
  Elapsed   <- DataYears - LastHist
  Expected  <- outer(Elapsed, rep_len(qInc, nSim(hist)), \(t, q) (1 + q / 100)^t)

  Ratio <- .ProjIndexValues(With$mse, fl, type, DataYears) /
    .ProjIndexValues(Without$mse, fl, type, DataYears)
  expect_equal(unname(Ratio), Expected, tolerance = 1e-10)

  # value / (Error * Efficiency) is nominal^Beta * multiplier
  Err  <- ObsW@Error[, as.character(DataYears), drop = FALSE]
  Nom  <- t(.ProjIndexValues(Without$mse, fl, type, DataYears)) / Err / as.numeric(ObsW@Efficiency)
  Adj  <- t(.ProjIndexValues(With$mse, fl, type, DataYears)) / Err / as.numeric(ObsW@Efficiency)
  expect_equal(unname(Adj / Nom), unname(t(Expected)), tolerance = 1e-10)

  TrueW  <- .BuildIndexFitDF(With$mse, type, Fleets = fl) |> dplyr::filter(.data$Series == "True")
  TrueWO <- .BuildIndexFitDF(Without$mse, type, Fleets = fl) |> dplyr::filter(.data$Series == "True")
  q <- rep_len(qInc, nSim(hist))[TrueW$Sim]
  Mult <- ifelse(TrueW$Period == "Projection", (1 + q / 100)^(TrueW$Year - LastHist), 1)
  expect_equal(TrueW$Value / TrueWO$Value, Mult, tolerance = 1e-10)
}

test_that("PopulateIndexObs() expands qInc to one value per sim", {
  Pop <- \(qInc, nSim = 4)
    PopulateIndexObs(IndicesObs(CV = 0.2, qInc = qInc), nSim, 2001:2010, 2011:2015)@qInc

  expect_equal(as.numeric(Pop(2)), rep(2, 4))
  expect_equal(dimnames(Pop(2)), list(Sim = as.character(1:4)))
  expect_equal(as.numeric(Pop(c(-1, 0, 2, 3))), c(-1, 0, 2, 3))
  set.seed(1)
  Drawn <- Pop(c(0, 3))
  expect_length(Drawn, 4)
  expect_true(all(Drawn >= 0 & Drawn <= 3))
  expect_null(Pop(0))
  expect_null(Pop(NULL))
  expect_error(IndicesObs(qInc = -100), "qInc")

  io <- IndicesObs(CV = 0.2, qInc = 2)
  expect_equal(qInc(io), 2)
  qInc(io) <- c(0, 3)
  expect_equal(io@qInc, c(0, 3))
  expect_error(qInc(io) <- -150, "qInc")
  expect_error(qInc(CompObs()), "x")

  Populated <- PopulateIndexObs(IndicesObs(CV = 0.2, qInc = c(0, 3)), 4, 2001:2010, 2011:2015)
  RePop <- PopulateIndexObs(Populated, 4, 2001:2010, 2011:2015)
  expect_identical(RePop@qInc, Populated@qInc)
})

test_that("index qInc scales projected index values per sim and leaves the historical period unchanged", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  om <- UpdateObject(SingleStockOM)
  om@nSim <- 4
  om@pYear <- 6
  fl <- FleetNames(om)[1]
  qInc <- c(-2, 0, 1.5, 3)

  With    <- .RunIndexqInc(om, fl, "Survey", qInc)
  Without <- .RunIndexqInc(om, fl, "Survey", NULL)
  .ExpectIndexqInc(With, Without, fl, "Survey", qInc)
})

test_that("index qInc leaves conditioning on partial-coverage real data unchanged", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  om <- UpdateObject(SingleStockOM)
  om@nSim <- 3
  om@pYear <- 5
  fl <- FleetNames(om)[1]

  set.seed(1)
  hist0 <- Simulate(om, silent = TRUE)
  HistYears <- Years(hist0, "H")
  NomIndex <- .CalcNomIndex(
    Number_List = hist0@Number[1], object = hist0, stocks = 1, fleet = fl,
    IndexObs = IndicesObs(), Years = HistYears
  )
  set.seed(3)
  Val <- as.numeric(NomIndex[1, ]) / mean(NomIndex[1, ]) * rlnorm(length(HistYears), 0, 0.2)
  Val[c(seq_len(10), seq(12, length(Val), by = 3))] <- NA
  om@Data <- list(Data(
    Years  = c(HistYears, Years(hist0, "P")),
    Survey = IndicesData(Name = fl, Value = matrix(Val, ncol = 1,
                                                   dimnames = list(as.character(HistYears), fl)))
  ))

  qInc <- c(1, -3, 2.5)
  With    <- .RunIndexqInc(om, fl, "Survey", qInc)
  Without <- .RunIndexqInc(om, fl, "Survey", NULL)
  expect_false(is.null(slot(With$hist@OM@Obs[[1]][[fl]], "Survey")@Stats))
  .ExpectIndexqInc(With, Without, fl, "Survey", qInc)
})

test_that("index qInc compounds per calendar year in a seasonal OM", {
  skip_on_cran()
  data(TwoFleetOM, envir = environment())
  om <- UpdateObject(TwoFleetOM)
  om@nSim <- 3
  om@pYear <- 3
  om@Seasons <- 4
  fl <- FleetNames(om)[1]
  om@Obs[[1]][[fl]]@CPUE <- IndicesObs(CV = 0.2)
  qInc <- c(2, 0, -4)

  With    <- .RunIndexqInc(om, fl, "CPUE", qInc)
  Without <- .RunIndexqInc(om, fl, "CPUE", NULL)
  .ExpectIndexqInc(With, Without, fl, "CPUE", qInc)

  # one calendar year after the last historical time step
  LastHist <- max(Years(With$hist, "H"))
  Year1 <- LastHist + 1
  Ratio <- .ProjIndexValues(With$mse, fl, "CPUE", Year1) /
    .ProjIndexValues(Without$mse, fl, "CPUE", Year1)
  expect_equal(as.numeric(Ratio), 1 + qInc / 100)
})

test_that("index qInc is subset by sim and projects the same in sim chunks", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  om <- UpdateObject(SingleStockOM)
  om@nSim <- 5
  om@pYear <- 4
  fl <- FleetNames(om)[1]
  qInc <- c(-1, 0, 1, 2, 3)
  om <- .SetIndexqInc(om, fl, "Survey", qInc)
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)

  Sub <- Subset(hist, Sims = c(4, 1))
  expect_equal(as.numeric(Sub@OM@Obs[[1]][[fl]]@Survey@qInc), qInc[c(4, 1)])

  Ref <- Project(hist, MPs = "CurrentEffort", parallel = FALSE, silent = TRUE)
  hist@OM@Control$ProjectChunks <- 2
  Chunked <- Project(hist, MPs = "CurrentEffort", silent = TRUE)
  expect_identical(Chunked@PPD, Ref@PPD)
})
