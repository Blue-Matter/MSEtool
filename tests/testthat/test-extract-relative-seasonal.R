.DenomArray <- function(Years) {
  array(seq_along(Years), dim = c(1, 1, length(Years)),
        dimnames = list(Sim = 1, Stock = "S", Year = Years))
}

test_that(".AlignDenomYears maps projection steps to the same season of the last historical year", {
  Hist <- c("2020", "2020.2466", "2020.4959", "2020.7479",
            "2021", "2021.2466", "2021.4959", "2021.7479")
  Proj <- c("2022", "2022.2466", "2022.4959", "2022.7479",
            "2023", "2023.2466", "2023.4959", "2023.7479")
  out <- .AlignDenomYears(.DenomArray(Hist), Proj)
  expect_equal(dimnames(out)$Year, Proj)
  expect_equal(as.numeric(out), rep(5:8, 2))

  out <- .AlignDenomYears(.DenomArray(Hist), c(Hist[3:8], Proj[1:2]))
  expect_equal(as.numeric(out), c(3:8, 5:6))
})

test_that(".AlignDenomYears repeats the last value for annual models", {
  out <- .AlignDenomYears(.DenomArray(as.character(2018:2021)), as.character(2020:2024))
  expect_equal(as.numeric(out), c(3, 4, 4, 4, 4))

  out <- .AlignDenomYears(.DenomArray("2018"), c("2022", "2022.2466", "2022.4959"))
  expect_equal(as.numeric(out), c(1, 1, 1))
})

test_that("Projection denominators repeat the last historical year's seasonal values", {
  skip_on_cran()
  data(TwoFleetOM, envir = environment())
  om <- TwoFleetOM
  om@nSim <- 3
  om@pYear <- 3
  om@Seasons <- 4
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  mse <- Project(hist, MPs = "CurrentEffort", parallel = FALSE, silent = TRUE)

  Denom <- ExtendSims(mse@Unfished@Dynamic@SBiomass, 3)
  nHist <- dim(Denom)[3]
  LastYear <- Denom[, , (nHist - 3):nHist, drop = FALSE]
  expect_false(isTRUE(all.equal(as.numeric(LastYear[, , 1]), as.numeric(LastYear[, , 4]))))

  Ratio <- SB_SB0(mse, type = "Dynamic", df = FALSE)
  SB <- mse@SBiomass
  Implied <- SB[, , , 1] / Ratio[, , , 1]
  nProj <- length(dimnames(SB)$Year)
  expect_equal(nProj, 12)
  for (y in 0:2)
    expect_equal(unname(Implied[, 4 * y + 1:4]), unname(abind::adrop(LastYear, 2)))
})
