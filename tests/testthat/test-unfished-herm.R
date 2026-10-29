# .CalcUnfishedNumberHerm() (R/calc-unfished-number.R), wired in via
# CalcUnfishedNumber() when OM@Herm is non-empty. Ground-truth values below
# are hand-derived (not just "runs without error") - see the function's own
# roxygen for the algebra. Fixtures adapt the packaged MultiStockOM (2
# fully-populated stocks) with constant M/R0 overrides via the normal
# NaturalMortality()/SRR() constructors (raw array-field overrides get wiped
# out by CalcUnfishedSurvival()'s internal re-populate, so constructors are
# required here, not direct slot assignment).

.make_herm_fixture <- function(nStock = 2, PlusGroup = FALSE, nSim = 1) {
  data(MultiStockOM, envir = environment())
  om <- MultiStockOM
  om@nSim <- nSim

  if (nStock == 3) {
    stC <- om@Stock[[1]]
    Name(stC) <- "Stock C"
    om@Stock <- list(om@Stock[[1]], om@Stock[[2]], stC)
    om@Fleet <- c(om@Fleet, stats::setNames(list(om@Fleet[[1]]), "Stock C"))
    om@Obs   <- c(om@Obs,   stats::setNames(list(om@Obs[[1]]), "Stock C"))
    om@Imp   <- c(om@Imp,   stats::setNames(list(om@Imp[[1]]), "Stock C"))
  }

  Ages(om@Stock[[1]]) <- Ages(MaxAge = 3, PlusGroup = PlusGroup)
  NaturalMortality(om@Stock[[1]]) <- NaturalMortality(Pars = list(M = 0.25))
  SRR(om@Stock[[1]])@R0 <- 1000

  Ages(om@Stock[[2]]) <- Ages(MaxAge = 3, PlusGroup = PlusGroup)
  NaturalMortality(om@Stock[[2]]) <- NaturalMortality(Pars = list(M = 0.35))
  SRR(om@Stock[[2]])@R0 <- 300

  PopulateOM(om, silent = TRUE)
}

test_that("no plus group: joint recursion matches hand-derived values exactly", {
  om <- .make_herm_fixture(PlusGroup = FALSE)
  sn <- StockNames(om)

  # Frac -> hazard c(0, 0.3, 0.6, 1.0)
  Frac <- array(c(1, 0.7, 0.28, 0), dim = c(1, 4))
  expect_equal(as.numeric(.HermHazardRate(Frac)[1, ]), c(0, 0.3, 0.6, 1.0))

  om@Herm <- list(Herm(From = sn[1], To = sn[2], Frac = Frac))
  N <- CalcUnfishedNumber(om)

  M_A <- 0.25; M_B <- 0.35; R0_A <- 1000; R0_B <- 300
  hz <- c(0, 0.3, 0.6, 1.0)
  StepA <- exp(-M_A); StepB <- exp(-M_B)
  N_A <- c(R0_A, rep(NA, 3)); N_B <- c(R0_B, rep(NA, 3))
  for (a in 2:4) {
    rawA <- N_A[a-1]*StepA; rawB <- N_B[a-1]*StepB
    mov  <- rawA*hz[a]
    N_A[a] <- rawA - mov; N_B[a] <- rawB + mov
  }

  expect_equal(as.numeric(N[[1]][1, , 1]), N_A, tolerance = 1e-8)
  expect_equal(as.numeric(N[[2]][1, , 1]), N_B, tolerance = 1e-8)
  expect_equal(N_A[4], 0)  # hazard[4]=1: everything moves out of A
})

test_that("plus group: terminal-age power iteration matches the algebraic closed form", {
  om <- .make_herm_fixture(PlusGroup = TRUE)
  sn <- StockNames(om)

  # Frac -> hazard c(0, 0.3, 0.6, 0.4) -- h[4] < 1, so both terminal ages
  # genuinely self-reference and must be solved jointly
  Frac <- array(c(1, 0.7, 0.28, 0.168), dim = c(1, 4))
  expect_equal(as.numeric(.HermHazardRate(Frac)[1, ]), c(0, 0.3, 0.6, 0.4), tolerance = 1e-6)

  om@Herm <- list(Herm(From = sn[1], To = sn[2], Frac = Frac))
  N <- CalcUnfishedNumber(om)

  M_A <- 0.25; M_B <- 0.35; R0_A <- 1000; R0_B <- 300
  hz <- c(0, 0.3, 0.6, 0.4)
  StepA <- exp(-M_A); StepB <- exp(-M_B)

  NA2 <- R0_A*StepA; NB2 <- R0_B*StepB
  mov2 <- NA2*hz[2]; NA2f <- NA2-mov2; NB2f <- NB2+mov2
  NA3 <- NA2f*StepA; NB3 <- NB2f*StepB
  mov3 <- NA3*hz[3]; NA3f <- NA3-mov3; NB3f <- NB3+mov3

  inflowA <- NA3f*StepA; inflowB <- NB3f*StepB
  selfA <- exp(-M_A); selfB <- exp(-M_B); h4 <- hz[4]
  # hand-derived closed form for the joint terminal-age fixed point:
  #   a4 = inflowA*(1-h4) / (1 - selfA*(1-h4))
  #   b4 = (inflowB + h4*inflowA + h4*selfA*a4) / (1 - selfB)
  a4 <- inflowA*(1-h4) / (1 - selfA*(1-h4))
  b4 <- (inflowB + h4*inflowA + h4*selfA*a4) / (1-selfB)

  expect_equal(as.numeric(N[[1]][1, 4, 1]), a4, tolerance = 1e-8)
  expect_equal(as.numeric(N[[2]][1, 4, 1]), b4, tolerance = 1e-8)
})

test_that("multi-sim: joint recursion is fully vectorised, matches hand values per sim", {
  om <- .make_herm_fixture(PlusGroup = TRUE, nSim = 2)
  sn <- StockNames(om)

  NaturalMortality(om@Stock[[1]]) <- NaturalMortality(Pars = list(M = c(0.2, 0.3)))
  NaturalMortality(om@Stock[[2]]) <- NaturalMortality(Pars = list(M = c(0.3, 0.4)))
  om <- PopulateOM(om, silent = TRUE, force = TRUE)

  Frac <- array(c(1, 0.7, 0.28, 0.168), dim = c(1, 4))
  om@Herm <- list(Herm(From = sn[1], To = sn[2], Frac = Frac))
  N <- CalcUnfishedNumber(om)

  hz <- as.numeric(.HermHazardRate(Frac)[1, ])
  handcalc <- function(M_A, M_B, R0_A = 1000, R0_B = 300) {
    StepA <- exp(-M_A); StepB <- exp(-M_B)
    N_A <- c(R0_A, NA, NA); N_B <- c(R0_B, NA, NA)
    for (a in 2:3) {
      rawA <- N_A[a-1]*StepA; rawB <- N_B[a-1]*StepB
      mov <- rawA*hz[a]
      N_A[a] <- rawA-mov; N_B[a] <- rawB+mov
    }
    inflowA <- N_A[3]*StepA; inflowB <- N_B[3]*StepB
    selfA <- exp(-M_A); selfB <- exp(-M_B); h4 <- hz[4]
    a4 <- inflowA*(1-h4)/(1-selfA*(1-h4))
    b4 <- (inflowB+h4*inflowA+h4*selfA*a4)/(1-selfB)
    list(A = c(N_A[1:3], a4), B = c(N_B[1:3], b4))
  }
  h1 <- handcalc(0.2, 0.3)
  h2 <- handcalc(0.3, 0.4)

  expect_equal(as.numeric(N[[1]][1, , 1]), h1$A, tolerance = 1e-8)
  expect_equal(as.numeric(N[[1]][2, , 1]), h2$A, tolerance = 1e-8)
  expect_equal(as.numeric(N[[2]][1, , 1]), h1$B, tolerance = 1e-8)
  expect_equal(as.numeric(N[[2]][2, , 1]), h2$B, tolerance = 1e-8)
})

test_that("a stock outside any Herm pair is byte-identical to the non-Herm closed form", {
  om <- .make_herm_fixture(nStock = 3, PlusGroup = FALSE)
  sn <- StockNames(om)

  N_base <- CalcUnfishedNumber(om)

  Frac <- array(c(1, 0.7, 0.28, 0), dim = c(1, 4))
  om@Herm <- list(Herm(From = sn[1], To = sn[2], Frac = Frac))
  N_herm <- CalcUnfishedNumber(om)

  expect_identical(N_herm[[3]], N_base[[3]])
  expect_false(isTRUE(all.equal(N_herm[[1]], N_base[[1]])))
  expect_false(isTRUE(all.equal(N_herm[[2]], N_base[[2]])))
})

test_that("SP = TRUE applies the same-age spawn-time partial-mortality factor to the Herm-consistent result", {
  om <- .make_herm_fixture(PlusGroup = FALSE)
  sn <- StockNames(om)
  SRR(om@Stock[[1]])@SpawnTimeFrac <- 0.4
  SRR(om@Stock[[2]])@SpawnTimeFrac <- 0.6
  om <- PopulateOM(om, silent = TRUE, force = TRUE)

  Frac <- array(c(1, 0.7, 0.28, 0), dim = c(1, 4))
  om@Herm <- list(Herm(From = sn[1], To = sn[2], Frac = Frac))

  N_full  <- CalcUnfishedNumber(om, SP = FALSE)
  N_spawn <- CalcUnfishedNumber(om, SP = TRUE)

  predA <- as.numeric(N_full[[1]][1, , 1]) * exp(-0.25 * 0.4)
  predB <- as.numeric(N_full[[2]][1, , 1]) * exp(-0.35 * 0.6)

  expect_equal(as.numeric(N_spawn[[1]][1, , 1]), predA, tolerance = 1e-8)
  expect_equal(as.numeric(N_spawn[[2]][1, , 1]), predB, tolerance = 1e-8)
})

test_that("non-convergence warns and returns a finite best-available result", {
  om <- .make_herm_fixture(PlusGroup = TRUE)
  sn <- StockNames(om)

  NaturalMortality(om@Stock[[1]]) <- NaturalMortality(Pars = list(M = 1e-7))
  NaturalMortality(om@Stock[[2]]) <- NaturalMortality(Pars = list(M = 1e-7))
  om <- PopulateOM(om, silent = TRUE, force = TRUE)

  # hazard essentially 0 with a tiny nonzero terminal-age value -- genuine
  # (but barely) coupled, and both stocks' self-survival is ~1, so the
  # terminal-age fixed point converges too slowly to reach tol within
  # max_iter
  Frac <- array(c(1, 1, 1, 1 - 1e-8), dim = c(1, 4))
  om@Herm <- list(Herm(From = sn[1], To = sn[2], Frac = Frac))

  expect_message(N <- CalcUnfishedNumber(om), "not reached")
  expect_true(is.finite(N[[1]][1, 4, 1]))
  expect_true(is.finite(N[[2]][1, 4, 1]))
  expect_true(N[[1]][1, 4, 1] > 0)
  expect_true(N[[2]][1, 4, 1] > 0)
})
