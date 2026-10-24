# per-sim [Sim x Year] historical nominal index of a complex
.HistNomIndex <- function(hist, cx, fl, type) {
  .CalcNomIndex(
    Number_List = hist@Number[hist@OM@Complexes[[cx]]], object = hist,
    stocks = hist@OM@Complexes[[cx]], fleet = fl,
    IndexObs = slot(hist@OM@Obs[[cx]][[fl]], type), Years = Years(hist, "H")
  )
}

.HistIndexValues <- function(hist, cx, fl, type) {
  t(sapply(seq_len(nSim(hist)), \(s) slot(hist@Data[[s]][[cx]], type)@Value[, fl]))
}

# index value at the summed BMSY of the complex's stocks
.ExpectedIndexRef <- function(hist, cx, fl, type) {
  stocks <- hist@OM@Complexes[[cx]]
  io     <- slot(hist@OM@Obs[[cx]][[fl]], type)
  HY     <- as.character(Years(hist, "H"))
  Nom    <- .HistNomIndex(hist, cx, fl, type)
  B      <- apply(hist@Biomass[, stocks, HY, drop = FALSE], c(1, 3), sum)
  BMSY   <- apply(hist@Reference@MSY@BMSY[, stocks, , drop = FALSE], c(1, 3), sum)
  sapply(seq_len(nSim(hist)), \(s) {
    Beta <- .SimValues(io@Beta %||% 1, s)[s]
    adj  <- mean(Nom[s, ] / B[s, ])
    unname(io@Efficiency[s] * (mean(BMSY[min(s, nrow(BMSY)), ]) * adj)^Beta)
  })
}

test_that("AR(1) index observation error has mean 1 and keeps the log-scale SD", {
  CV    <- 0.3
  sigma <- sdconv(1, CV)
  for (ac in c(0, -0.5, 0.5, 0.9)) {
    set.seed(1)
    io <- PopulateIndexObs(IndicesObs(CV = CV, AC = ac), nSim = 2000,
                           HistYears = 1:30, ProjYears = 31:60)
    e <- io@Error[, 21:60]
    expect_equal(mean(e), 1, tolerance = 0.01, label = paste("mean, AC =", ac))
    expect_equal(sd(log(e)), sigma, tolerance = 0.02, label = paste("log SD, AC =", ac))
    expect_equal(mean(log(e)), mconv(1, CV), tolerance = 0.05, label = paste("log mean, AC =", ac))
    if (ac != 0) {
      lag1 <- mean(sapply(seq_len(200), \(s) acf(log(io@Error[s, ]), plot = FALSE)$acf[2]))
      expect_equal(lag1, ac, tolerance = 0.15, label = paste("lag-1, AC =", ac))
    }
  }
})

test_that("index observation AC must be within (-1, 1)", {
  Pop <- \(AC, nSim = 3) PopulateIndexObs(IndicesObs(CV = 0.2, AC = AC), nSim, 1:10, 11:15)
  expect_error(Pop(1), "AC")
  expect_error(Pop(-1), "AC")
  expect_error(Pop(c(0, 1), nSim = 4), "AC")
  expect_error(Pop(c(0.2, 0.5, 1)), "AC")
  expect_no_error(Pop(0.99))
  expect_no_error(Pop(c(-0.5, 0.5), nSim = 4))
  expect_error(ApplyAC(matrix(1, 2, 3), AC = c(0, 1), LastError = c(NA_real_, NA_real_)), "AC")
  expect_no_error(ApplyAC(matrix(1, 2, 3), AC = c(0, 0.9), LastError = c(NA_real_, NA_real_)))
})

test_that("simulated index values continue the same scale from historical to projection years", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  om <- UpdateObject(SingleStockOM)
  om@nSim <- 5
  om@pYear <- 6
  fl <- FleetNames(om)[1]
  om@Obs[[1]][[fl]]@Survey <- IndicesObs(CV = c(0.2, 0.4), AC = c(0.5, 0.9), Beta = 0.8)

  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  io <- hist@OM@Obs[[1]][[fl]]@Survey
  HY <- as.character(Years(hist, "H"))

  # historical value / (nominal^Beta * error) is Efficiency for every year
  Nom <- .HistNomIndex(hist, 1, fl, "Survey")
  Val <- .HistIndexValues(hist, 1, fl, "Survey")
  Beta <- .SimValues(io@Beta, nSim(hist))
  Ratio <- Val / (Nom^Beta * io@Error[, HY])
  expect_equal(unname(Ratio), matrix(as.numeric(io@Efficiency), nSim(hist), length(HY)),
               tolerance = 1e-10)
  expect_equal(unname(rowMeans(Val)), rep(1, nSim(hist)), tolerance = 1e-10)

  set.seed(2)
  mse <- Project(hist, MPs = "CurrentEffort", parallel = FALSE, silent = TRUE)
  df  <- .BuildIndexFitDF(mse, "Survey", Fleets = fl)
  m   <- merge(df[df$Series == "Index", ], df[df$Series == "True", ],
               by = c("Sim", "Year", "Period"), suffixes = c(".Index", ".True"))
  m$Error <- io@Error[cbind(m$Sim, match(as.character(m$Year), colnames(io@Error)))]
  m$Ratio <- m$Value.Index / (m$Value.True * m$Error)
  expect_setequal(unique(m$Period), c("Historical", "Projection"))
  expect_true(length(unique(m$Sim)) == nSim(hist))
  expect_equal(m$Ratio, rep(1, nrow(m)), tolerance = 1e-10)
})

test_that("index Ref is the index value at BMSY of the complex's stocks", {
  skip_on_cran()
  data(ComplexOM, envir = environment())
  om <- ComplexOM
  om@nSim <- 4
  fl <- FleetNames(om)[1]
  om@Obs[[1]][[fl]]@Survey <- IndicesObs(CV = 0.2, AC = 0.5, Ref = 0, Beta = 0.7,
                                         Selectivity = "Biomass")
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)

  Ref <- sapply(seq_len(nSim(hist)), \(s) hist@Data[[s]][[1]]@Survey@Ref)
  expect_equal(Ref, .ExpectedIndexRef(hist, 1, fl, "Survey"), tolerance = 1e-8)
})

test_that("index Ref uses the stocks of each complex when complexes are not in stock order", {
  skip_on_cran()
  data(MultiStockOM, envir = environment())
  om <- MultiStockOM
  om@nSim <- 3
  om@Complexes <- list(B = 2, A = 1)
  om@Obs <- stats::setNames(om@Obs[c(2, 1)], c("B", "A"))
  fl <- FleetNames(om)[1]
  for (cx in 1:2)
    om@Obs[[cx]][[fl]]@Survey <- IndicesObs(CV = 0.2, Ref = 0, Beta = 0.8,
                                            Selectivity = "Biomass")
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)

  for (cx in 1:2) {
    Ref <- sapply(seq_len(nSim(hist)), \(s) {
      d <- slot(hist@Data[[s]][[cx]], "Survey")
      d@Ref[match(fl, d@Name)]
    })
    expect_equal(Ref, .ExpectedIndexRef(hist, cx, fl, "Survey"), tolerance = 1e-8,
                 label = paste("complex", cx))
  }
})

test_that("index Ref falls back to the index value at half of unfished biomass", {
  skip_on_cran()
  data(ComplexOM, envir = environment())
  om <- ComplexOM
  om@nSim <- 3
  fl <- FleetNames(om)[1]
  om@Obs[[1]][[fl]]@Survey <- IndicesObs(CV = 0.2, Ref = 0, Beta = 0.7,
                                         Selectivity = "Biomass")
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  hist@Reference@MSY@BMSY <- array(numeric(0))

  stocks <- om@Complexes[[1]]
  HY  <- Years(hist, "H")
  Nom <- .HistNomIndex(hist, 1, fl, "Survey")
  B   <- apply(hist@Biomass[, stocks, as.character(HY), drop = FALSE], c(1, 3), sum)
  B0  <- apply(hist@Unfished@Equilibrium@Biomass[, stocks, , drop = FALSE], c(1, 3), sum)
  for (s in seq_len(nSim(hist))) {
    IndexData <- .GenHistDataIndices(s, new("data"), hist, HY, 1, stocks,
                                     StockNames(hist), nArea(hist), type = "Survey")
    io   <- IndexData@Misc$IndexObs[[fl]]
    Beta <- .SimValues(io@Beta, s)[s]
    RefB <- B[s, which.min(abs(B[s, ] / B0[min(s, nrow(B0)), ] - 0.5))]
    Expected <- io@Efficiency * (RefB * mean(Nom[s, ] / B[s, ]))^Beta
    expect_equal(IndexData@Ref[match(fl, IndexData@Name)], as.numeric(Expected), tolerance = 1e-8)
  }
})
