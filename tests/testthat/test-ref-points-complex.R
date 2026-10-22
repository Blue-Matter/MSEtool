skip_on_cran()

.ref_hist <- function(name, nSim = 2) {
  data(list = name, envir = environment())
  om <- get(name, envir = environment())
  om@nSim <- nSim
  Simulate(om, silent = TRUE, control = SimControl(MSYRefs = FALSE))
}

# Per-stock lifetime yield per recruit on the complex F grid used by CalcRefPoints()
.ref_ypr <- function(hist) {
  yr <- utils::tail(Years(hist@OM, 'Historical'), 1)
  if (hist@OM@Seasons > 1) yr <- unique(floor(yr))
  Fgrid <- exp(seq(log(1e-4), log(hist@OM@maxF), length.out = 100))
  pr <- CalcPerRecruit(hist, apicalF = Fgrid, Years = yr)
  list(Fgrid = Fgrid,
       YPR = MSEtool:::.AddHermLifetime(pr@Removals, pr@Misc$HermCross, 'Removals'))
}

test_that("single-stock F0.1 and Fmax are unchanged by complex-level calculation", {
  hist <- .ref_hist("SingleStockOM")
  ypr  <- .ref_ypr(hist)
  expect_equal(as.numeric(F01(hist)),
               as.numeric(MSEtool:::.ApplyOverF(ypr$YPR, ypr$Fgrid, MSEtool:::.FindF01)))
  expect_equal(as.numeric(FMax(hist)),
               as.numeric(MSEtool:::.ApplyOverF(ypr$YPR, ypr$Fgrid, MSEtool:::.FindFmax)))
})

test_that("F0.1 and Fmax are calculated from the complex's R0-weighted yield per recruit", {
  hist <- .ref_hist("ComplexOM")
  expect_identical(dimnames(F01(hist))$Stock, names(Complexes(hist)))
  ypr <- .ref_ypr(hist)
  Y   <- .Aperm(ypr$YPR, c('Sim', 'Stock', 'Year', 'F'))
  R0  <- purrr::map_dbl(hist@OM@Stock, \(s) s@SRR@R0[1, 1])
  Complex <- (R0[1] * Y[, 1, 1, ] + R0[2] * Y[, 2, 1, ]) / sum(R0)
  expect_equal(as.numeric(F01(hist)),
               unname(apply(Complex, 1, MSEtool:::.FindF01, Fgrid = ypr$Fgrid)))
  expect_equal(as.numeric(FMax(hist)),
               unname(apply(Complex, 1, MSEtool:::.FindFmax, Fgrid = ypr$Fgrid)))
})

test_that("a Herm complex's F0.1 is the female lifetime F0.1 when all fish are born female", {
  hist <- .ref_hist("HermOM")
  ypr  <- .ref_ypr(hist)
  Female <- MSEtool:::.ApplyOverF(ypr$YPR, ypr$Fgrid, MSEtool:::.FindF01)
  Female <- .Aperm(Female, c('Stock', setdiff(names(dimnames(Female)), 'Stock')))
  expect_identical(dimnames(F01(hist))$Stock, "Grouper")
  expect_equal(as.numeric(F01(hist)), as.numeric(matrix(Female, nrow = 2)[1, ]))
})
