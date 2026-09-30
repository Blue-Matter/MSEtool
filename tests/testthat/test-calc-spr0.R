.SPR0Stock <- function() {
  Stock(Name = "s", Ages = Ages(MaxAge = 20),
        Length = Length(Pars = list(Linf = 120, K = 0.2, t0 = -0.1)),
        Weight = Weight(Pars = list(a = 0.008, b = 3)),
        NaturalMortality = NaturalMortality(Pars = list(M = c(0.15, 0.3))),
        Maturity = Maturity(Pars = list(L50 = 50, L50_95 = 10)),
        SRR = SRR(Pars = list(h = 0.7), R0 = 1000, SD = 0.4))
}

.SPR0Fleet <- function() {
  Fleet(Name = "F",
        Effort = Effort(Effort = data.frame(Year = c(2000, 2024), Lower = 0.2, Upper = 1, CV = 0.1)),
        Selectivity = Selectivity(Pars = list(SA50 = 4, SA50_95 = 2)))
}

test_that("CalcSPR0 does not need a Fleet", {
  args <- list(nSim = 5, CurrentYear = 2024, nYear = 25, pYear = 5)
  ref <- CalcSPR0(do.call(OM, c(list(Stock = .SPR0Stock(), Fleet = .SPR0Fleet()), args)), silent = TRUE)
  expect_equal(CalcSPR0(do.call(OM, c(list(Stock = .SPR0Stock()), args)), silent = TRUE), ref)
  expect_equal(CalcSPR0(do.call(OM, c(list(Stock = .SPR0Stock(), Fleet = Fleet(Name = "F")), args)),
                        silent = TRUE), ref)
})

test_that("CalcSPR0 accepts a Stock object", {
  args <- list(nSim = 5, CurrentYear = 2024, nYear = 25, pYear = 5)
  ref <- CalcSPR0(do.call(OM, c(list(Stock = .SPR0Stock()), args)), silent = TRUE)
  expect_equal(do.call(CalcSPR0, c(list(.SPR0Stock(), silent = TRUE), args)), ref)
})

test_that("CalcSPR0 is unchanged by dropping fleets from example OMs", {
  for (om in list(SingleStockOM, HermOM, MultiStockOM, SeasonalSpatialOM)) {
    ref <- CalcSPR0(om, silent = TRUE)
    om@Fleet <- NULL
    expect_equal(CalcSPR0(om, silent = TRUE), ref)
  }
})

test_that("CalcSPR0 from an OM matches CalcSPR0 from the simulated Hist", {
  skip_on_cran()
  for (om in list(SingleStockOM, HermOM, SeasonalSpatialOM)) {
    fromOM   <- CalcSPR0(om, silent = TRUE)
    fromHist <- CalcSPR0(Simulate(om, silent = TRUE), silent = TRUE)
    expect_equal(dimnames(fromOM), dimnames(fromHist))
    expect_equal(c(fromOM), c(fromHist))
  }
})
