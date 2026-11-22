testthat::skip_on_cran()

local({
  OM <- SingleStockOM
  nSim(OM) <- 3
  OM   <- Populate(OM, silent = TRUE)
  Hist <- Simulate(OM, silent = TRUE)
  HistYears <- as.numeric(Years(Hist, 'Historical'))
  ProjYears <- as.numeric(Years(Hist, 'Projection'))
  Late  <- utils::tail(HistYears, 10)
  Early <- utils::head(HistYears, 10)

  test_that("Project() rejects a hist subset by Years", {
    for (y in list(Late, Early, c(Late, ProjYears)))
      expect_error(Project(Subset(Hist, Years = y), MPs = 'CurrentCatch', silent = TRUE),
                   "summarised and plotted, but not projected")
  })

  test_that("TuneMP() rejects a hist subset by Years", {
    expect_error(.TuneHistList(Subset(Hist, Years = Late), 'Hist'),
                 "summarised and plotted, but not projected")
  })

  test_that("a hist subset by all its years still projects", {
    H <- Subset(Hist, Years = c(HistYears, ProjYears))
    expect_silent(.CheckHistYears(H))
    expect_s4_class(Project(H, MPs = 'CurrentCatch', silent = TRUE), 'mse')
  })

  test_that("Simulate() on an om subset by Years names Subset()", {
    expect_error(Simulate(Subset(OM, Years = Late), silent = TRUE), "Subset\\(Years = \\)")
  })

  test_that("Subset(hist, Years) does not impute historical values after the historical period", {
    H <- Subset(Hist, Years = c(Late, ProjYears))
    expect_equal(as.numeric(dimnames(H@SBiomass)$Year), Late)
    expect_equal(as.numeric(dimnames(H@Number[[1]])$Year), Late)
    SRR <- H@OM@Stock[[1]]@SRR
    expect_equal(as.numeric(dimnames(SRR@RecDevHist)$Year), Late)
    expect_equal(as.numeric(dimnames(SRR@RecDevProj)$Year), ProjYears)
    expect_equal(SBiomass(H)$Year |> unique() |> sort(), Late)
    expect_equal(H@SBiomass, Subset(Hist, Years = Late)@SBiomass)
  })

  test_that("Subset(hist, Years) keeps values for retained years and supports plotting", {
    H <- Subset(Hist, Years = Late)
    expect_equal(unname(H@SBiomass), unname(Hist@SBiomass[, , as.character(Late), drop = FALSE]))
    expect_no_error(PlotDepletion(H))
  })

  test_that("Subset(mse, Years) does not impute OM RecDevHist", {
    MSE <- Project(Hist, MPs = 'CurrentCatch', silent = TRUE)
    M   <- Subset(MSE, Years = c(Late, ProjYears[1:5]))
    expect_equal(as.numeric(dimnames(M@OM@Stock[[1]]@SRR@RecDevHist)$Year), Late)
    expect_equal(as.numeric(dimnames(M@Hist@SBiomass)$Year), Late)
    expect_no_error(SBiomass(M))
  })
})
