.SurvivalR <- function(S, S0, R0, zfrac, Beta = 1, RecFrac = 1) {
  z0   <- log(R0 / (RecFrac * S0))
  zmin <- z0 * (1 - zfrac)
  z    <- z0 + (zmin - z0) * (1 - (S / S0)^Beta)
  ifelse(S > 0, RecFrac * S * exp(z), 0)
}

test_that("SurvivalSRR passes through R0 at S0 and goes to 0 as S goes to 0", {
  S0 <- 4e4
  R0 <- 1.2e4
  expect_equal(SurvivalSRR(S0, S0, R0, zfrac = 0.4), R0)
  expect_equal(SurvivalSRR(S0, S0, R0, zfrac = 0.4, Beta = 2, RecFrac = 0.5), R0)
  expect_equal(SurvivalSRR(0, S0, R0, zfrac = 0.4), 0)
  expect_lt(SurvivalSRR(1e-6, S0, R0, zfrac = 0.4), 1e-6)
})

test_that("SurvivalSRR recruitment cannot exceed spawning production when phi0 > 1", {
  S <- seq(1, 8e4, length.out = 50)
  R <- SurvivalSRR(S, 4e4, 1.2e4, zfrac = 0.9, Beta = 3)
  expect_true(all(R <= S))
})

test_that("SurvivalSRR matches a hand-coded R version across a grid", {
  S0 <- 4e4
  R0 <- 1.2e4
  S  <- c(0, 10, 500, 5000, 2e4, 4e4, 6e4)
  grid <- expand.grid(zfrac = c(0, 0.2, 0.4, 0.8, 1), Beta = c(0.5, 1, 2, 5),
                      RecFrac = c(0.3, 1))
  for (i in seq_len(nrow(grid))) {
    g <- grid[i, ]
    expect_equal(SurvivalSRR(S, S0, R0, g$zfrac, g$Beta, g$RecFrac),
                 .SurvivalR(S, S0, R0, g$zfrac, g$Beta, g$RecFrac),
                 tolerance = 1e-12, info = paste(unlist(g), collapse = " "))
  }
})

test_that("SurvivalSRR rejects invalid parameters", {
  expect_error(SurvivalSRR(1, 10, 3, zfrac = -0.1))
  expect_error(SurvivalSRR(1, 10, 3, zfrac = 1.1))
  expect_error(SurvivalSRR(1, 10, 3, zfrac = 0.4, Beta = 0))
  expect_error(SurvivalSRR(1, 10, 3, zfrac = 0.4, RecFrac = 0))
  expect_error(SurvivalSRR_RelRec(list(zfrac = 0.4, Beta = 1), 0.5))
})

test_that("SurvivalSRR_RelRec is 1 at SPR = 1 and 0 below the replacement SPR", {
  Pars <- list(zfrac = 0.4, Beta = 1, SPR0 = 3.3)
  expect_equal(SurvivalSRR_RelRec(Pars, 1), 1)
  SPRcrash <- exp(-Pars$zfrac * log(Pars$SPR0))
  expect_equal(SurvivalSRR_RelRec(Pars, SPRcrash * 0.99), 0)
  expect_gt(SurvivalSRR_RelRec(Pars, SPRcrash * 1.01), 0)
  expect_equal(SurvivalSRR_RelRec(Pars, 0), 0)
})

test_that("SurvivalSRR_RelRec is consistent with SurvivalSRR at equilibrium", {
  R0 <- 1000
  for (Pars in list(list(zfrac = 0.4, Beta = 1, SPR0 = 3.3),
                    list(zfrac = 0.7, Beta = 2.5, SPR0 = 8),
                    list(zfrac = 0.6, Beta = 0.7, SPR0 = 6, RecFrac = 0.5))) {
    RecFrac <- Pars$RecFrac %||% 1
    S0 <- R0 * Pars$SPR0
    for (SPR in c(0.3, 0.5, 0.75, 0.9, 1)) {
      RelRec <- SurvivalSRR_RelRec(Pars, SPR)
      if (RelRec <= 0) next
      S <- RelRec * R0 * SPR * Pars$SPR0
      expect_equal(SurvivalSRR(S, S0, R0, Pars$zfrac, Pars$Beta, RecFrac) / R0, RelRec,
                   tolerance = 1e-10)
    }
  }
})

test_that("SRR(Model = 'SurvivalSRR') is found from Pars, ordered, and drives the engine", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  OM <- SingleStockOM
  OM@nSim <- 2
  OM@Stock[[1]]@SRR@Pars <- list(Beta = 1.5, zfrac = 0.6)
  OM@Stock[[1]]@SRR@Model <- "SurvivalSRR"
  OM@Stock[[1]]@SRR@SD <- 0.3
  Hist <- Simulate(OM, silent = TRUE)
  SRR <- Hist@OM@Stock[[1]]@SRR
  expect_identical(SRR@Model, "SurvivalSRR")
  expect_identical(names(SRR@Pars), c("zfrac", "Beta"))
  expect_true(all(.SRRRecFrac(Hist@OM@Stock)[[1]] == 1))

  SP  <- Hist@SProduction[1, 1, ]
  SP0 <- Hist@Unfished@Equilibrium@SProduction[1, 1, 1]
  R0  <- SRR@R0[1, ]
  Lag <- .RecLag(Hist@OM@Stock)[1]
  Dev <- SRR@RecDevHist[1, ]
  Rec <- apply(Hist@Number[[1]][1, 1, , , drop = FALSE], 3, sum)
  y   <- seq(5, length(SP) - Lag)
  Expected <- vapply(y, \(i) SurvivalSRR(SP[i], SP0, R0[i + Lag], 0.6, 1.5) * Dev[i + Lag],
                     numeric(1))
  expect_equal(unname(Rec[y + Lag]), unname(Expected), tolerance = 1e-8)
})

test_that("ImportSS imports the SS3 dogfish survival SRR and matches SS3", {
  skip_on_cran()
  dogfish_dir <- 'G:/My Drive/1_PROJECTS/openMSE_dev/CaseStudies/dogfish_A0'
  skip_if_not(dir.exists(dogfish_dir))
  replist <- ImportSSReport(dogfish_dir, silent = TRUE)[[1]]
  OM <- suppressWarnings(ImportSS(replist, silent = TRUE))
  for (st in OM@Stock) {
    expect_equal(as.numeric(st@SRR@Pars$zfrac), 0.4)
    expect_equal(as.numeric(st@SRR@Pars$Beta), 1)
    expect_null(st@SRR@Pars$RecFrac)
  }
  Hist <- Simulate(OM, silent = TRUE)
  expect_identical(Hist@OM@Stock[[1]]@SRR@Model, "SurvivalSRR")
  RecFrac <- .SRRRecFrac(Hist@OM@Stock)
  expect_true(all(abs(unlist(RecFrac) - 0.5) < 1e-12))
  Stocks <- Hist@OM@Stock
  Stocks[[2]]@SRR@R0 <- Stocks[[2]]@SRR@R0 / 2
  expect_equal(unname(.SRRRecFrac(Stocks)[[1]][1, 1]), 2 / 3)
  expect_equal(unname(.SRRRecFrac(Stocks)[[2]][1, 1]), 1 / 3)

  ts <- replist$timeseries |> dplyr::filter(Era == "TIME")
  # last year's late deviation is not imported
  ts <- ts[ts$Yr < max(ts$Yr), ]
  Yr <- as.character(ts$Yr)
  SP <- Hist@SProduction[1, "Female", Yr]
  Rec <- vapply(Hist@Number, \(N) apply(N[1, 1, Yr, , drop = FALSE], 3, sum), numeric(length(Yr))) |>
    rowSums()
  expect_lt(max(abs(SP / ts$SpawnBio - 1)), 1e-4)
  expect_lt(max(abs(Rec / ts$Recruit_0 - 1)), 1e-4)

  ey <- replist$equil_yield
  ey <- ey[is.finite(ey$SPR) & ey$SPR > 0.5 & ey$SPR < 1, ]
  Pars <- c(purrr::map(Hist@OM@Stock[[1]]@SRR@Pars, \(x) x[1, 1]),
            RecFrac = RecFrac[[1]][1, 1], SPR0 = Hist@Reference@SPR0[1, "Female", 1])
  RelRec <- vapply(ey$SPR, \(x) SurvivalSRR_RelRec(Pars, x), numeric(1))
  R0 <- replist$derived_quants$Value[replist$derived_quants$Label == "Recr_Virgin"]
  expect_equal(RelRec, ey$Recruits / R0, tolerance = 1e-5)
})
