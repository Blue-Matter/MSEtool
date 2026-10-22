skip_on_cran()

# Single stock, one area, deterministic recruitment, constant effort, with M
# raised by `Mult` from the second year onward
.tv_m_hist <- function(Mult = 1.3, nYear = 150) {
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim  <- 2
  om@nYear <- nYear
  om@Stock[[1]]@SRR@SD <- 0
  om@Stock[[1]]@Spatial <- new("spatial")
  om@Stock[[1]]@Depletion@Final <- 0.4
  for (f in seq_along(om@Fleet[[1]]))
    om@Fleet[[1]][[f]]@Effort@Effort <- data.frame(Year = c(0, 1), Lower = c(1, 1),
                                                   Upper = c(1, 1), CV = c(0, 0))
  M <- PopulateOM(om, silent = TRUE)@Stock[[1]]@NaturalMortality@MeanAtAge
  AllYears <- c(Years(om, 'H'), Years(om, 'P'))
  M2 <- array(M[, , 1], dim = c(dim(M)[1:2], length(AllYears)),
              dimnames = list(Sim = dimnames(M)$Sim, Age = dimnames(M)$Age, Year = AllYears))
  M2[, , -1] <- M2[, , -1] * Mult
  om@Stock[[1]]@NaturalMortality <- NaturalMortality(MeanAtAge = M2)
  Simulate(om, silent = TRUE, control = SimControl(MSYRefs = FALSE))
}

test_that("equilibrium recruitment uses the first-year SRR reference under time-varying biology", {
  hist  <- .tv_m_hist()
  nY    <- length(Years(hist@OM, 'H'))
  LastY <- as.numeric(Years(hist@OM, 'H')[nY])
  R     <- apply(hist@Number[[1]][, 1, , , drop = FALSE], c(1, 3), sum)
  R0    <- hist@OM@Stock[[1]]@SRR@R0[1, 1]
  SB    <- hist@SBiomass[, 1, nY]

  eq <- CalcEquilibrium(hist, apicalF = exp(seq(log(1e-3), log(2), length.out = 300)),
                        Years = LastY)
  for (s in 1:2) {
    SBPR <- eq@SBiomass[s, 1, 1, ] / eq@Recruits[s, 1, 1, ]
    ok   <- is.finite(SBPR) & eq@Recruits[s, 1, 1, ] > 0
    At   <- \(q) stats::splinefun(SBPR[ok], q[ok])(SB[s] / R[s, nY])
    expect_equal(At(eq@RelRecruits[s, 1, 1, ]), unname(R[s, nY] / R0), tolerance = 1e-5)
    expect_equal(At(eq@SBiomass[s, 1, 1, ]), unname(SB[s]), tolerance = 1e-5)
  }
})

test_that("the first-year SRR reference equals current unfished SP per recruit with constant biology", {
  for (name in c("SingleStockOM", "ComplexOM", "SeasonalSpatialOM")) {
    data(list = name, envir = environment())
    om <- get(name, envir = environment())
    om@nSim <- 2
    hist <- Simulate(om, silent = TRUE, control = SimControl(MSYRefs = FALSE))
    yr <- utils::tail(Years(hist@OM, 'Historical'), 1)
    if (hist@OM@Seasons > 1) yr <- unique(floor(yr))
    st <- StockNames(hist)[Complexes(hist)[[1]]]
    inputs <- MSEtool:::.PrepPerRecruitInputs(
      hist@OM@Stock[st], hist@OM@Fleet[st], Array2List(hist@Reference@SPR0)[st], yr,
      EffortYears = hist@OM@RefEffortYears, RefSeason = hist@OM@RefSeason,
      Phi0Ref = MSEtool:::.SRRPhi0Ref(hist)[st])
    Current <- sapply(inputs$RecParsList, \(p) p$SPR0[, 1])
    Current <- matrix(Current, nrow = nSim(hist))
    expect_equal(unname(Current), unname(matrix(inputs$Phi0SRR[, , 1], nrow = nSim(hist))),
                 tolerance = 1e-8, info = name)
  }
})
