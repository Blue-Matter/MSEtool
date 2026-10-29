# User-supplied `OM@Data` whose `Years` span historical + projection years must
# still get projection data appended each year (R/generate-data-proj.R).

.RunCaptureDataMP <- function(hist) {
  assign(".projDataLog", list(), envir = globalenv())
  on.exit(rm(".projDataLog", envir = globalenv()), add = TRUE)

  captureMP <- function(Data) {
    log <- get(".projDataLog", envir = globalenv())
    log[[length(log) + 1]] <- Data
    assign(".projDataLog", log, envir = globalenv())
    Advice(Effort = 1, EffType = 'Rel')
  }
  class(captureMP) <- 'mp'
  assign("captureMP", captureMP, envir = globalenv())
  on.exit(rm("captureMP", envir = globalenv()), add = TRUE)

  Project(hist, MPs = "captureMP", parallel = FALSE, silent = TRUE)
  get(".projDataLog", envir = globalenv())
}

test_that("projection data is generated when OM@Data@Years includes projection years", {
  skip_on_cran()
  om <- TwoFleetOM
  om@nSim <- 2
  om@pYear <- 5
  fl <- FleetNames(om)[1]
  om@Obs[[1]][[fl]]@Survey <- IndicesObs()

  set.seed(1)
  hist0 <- Simulate(om, silent = TRUE)
  HistYears <- Years(hist0, 'H')
  ProjYears <- Years(hist0, 'P')
  AllYears  <- c(HistYears, ProjYears)

  NomIndex <- .CalcNomIndex(
    Number_List = hist0@Number[1], object = hist0, stocks = 1, fleet = fl,
    IndexObs = IndicesObs(), Years = HistYears
  )
  synthetic <- as.numeric(NomIndex[1, ]) / mean(NomIndex[1, ])
  Val <- matrix(synthetic, ncol = 1, dimnames = list(as.character(HistYears), fl))

  om@Data <- list(Data(Years = AllYears, Survey = IndicesData(Name = fl, Value = Val)))
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  expect_equal(max(hist@Data[[1]][[1]]@Years), max(HistYears))
  expect_equal(hist@Data[[1]][[1]]@YearLH, max(HistYears))

  log <- .RunCaptureDataMP(hist)
  expect_length(log, om@nSim * length(ProjYears))

  last <- log[[length(log)]]
  expectedYear <- ProjYears[length(ProjYears) - 1]
  expect_equal(max(last@Years), expectedYear)
  expect_equal(max(as.numeric(rownames(last@Survey@Value))), expectedYear)
  expect_equal(max(as.numeric(rownames(last@Landings@Value))), expectedYear)
  projRows <- as.character(ProjYears[-length(ProjYears)])
  expect_true(all(is.finite(last@Survey@Value[projRows, fl])))
})
