# Survey observed in season 1 with a seasonal selectivity array (1 in season 1, 0.01 otherwise),
# for a two-stock complex
.SeasonalSelIndexHist <- function(Units, SameAges = TRUE) {
  data(MultiStockOM, envir = environment())
  om <- MultiStockOM
  om@Seasons <- 4
  om@nSim <- 2
  om@pYear <- 2
  om@Complexes <- list(Both = 1:2)
  om@Obs <- list()
  om@Imp <- list()
  if (SameAges) Ages(om@Stock[[2]]) <- Ages(om@Stock[[1]])

  set.seed(1)
  h0 <- Simulate(om, silent = TRUE)
  HY <- Years(om, "Historical")
  S1 <- as.character(HY[HY %% 1 == 0])
  Total <- if (Units == "Number") {
    Reduce(`+`, lapply(h0@Number, \(a) apply(a[1, , , , drop = FALSE], "Year", sum)))
  } else {
    colSums(h0@Biomass[1, , ])
  }
  Value <- stats::setNames(Total[S1] * exp(stats::rnorm(length(S1), 0, 0.05)), S1)

  Sel <- lapply(om@Stock, \(st) {
    Ages <- st@Ages@Classes
    array(rep(ifelse(HY %% 1 == 0, 1, 0.01), each = length(Ages)),
          c(length(Ages), length(HY)), list(Age = Ages, Year = HY))
  })
  names(Sel) <- StockNames(om)

  om <- AddIndex(om, Value, Name = "Acoustic", Units = Units, Selectivity = Sel)
  set.seed(2)
  Simulate(om, silent = TRUE)
}

test_that("projected seasonal-selectivity index continues the historical scale", {
  skip_on_cran()
  for (Units in c("Number", "Biomass")) {
    hist <- .SeasonalSelIndexHist(Units)
    LastHist <- max(Years(hist@OM, "Historical"))
    mse <- suppressWarnings(Project(hist, MPs = "CurrentEffort", silent = TRUE))
    for (x in seq_len(nSim(hist))) {
      v <- mse@PPD[[1]][[x]][[1]]@Survey@Value[, "Acoustic"]
      yr <- as.numeric(names(v))
      Hist <- v[yr == floor(LastHist)]
      Proj <- v[yr == floor(LastHist) + 1]
      expect_true(is.finite(Proj), label = paste(Units, "sim", x, "projected value"))
      expect_true(Proj / Hist > 0.5 && Proj / Hist < 2,
                  label = sprintf("%s sim %d: projected/historical %.3f", Units, x, Proj / Hist))
    }
  }
})

test_that(".ExtendOM() repeats the seasonal pattern of reduced index selectivity for stocks with different ages", {
  skip_on_cran()
  hist <- .SeasonalSelIndexHist("Number", SameAges = FALSE)
  AllYears <- Years(hist@OM)
  om <- .ExtendOM(hist@OM, Years = AllYears, silent = TRUE)
  for (sel in om@Obs[[1]][["Acoustic"]]@Survey@Selectivity) {
    yr <- as.numeric(dimnames(sel)$Year)
    expect_equal(yr, AllYears)
    expect_true(all(sel[, yr %% 1 == 0] == 1))
    expect_true(all(sel[, yr %% 1 != 0] == 0.01))
  }
})
