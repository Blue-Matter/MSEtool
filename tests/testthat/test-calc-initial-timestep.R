.InitialHist <- function(OM) {
  Hist <- .OM2Hist(OM, silent = TRUE)
  Hist@Unfished@Equilibrium <- CalcUnfished_Equilibrium(Hist@OM, silent = TRUE)
  Hist
}

.ExpectedInitialNumber <- function(Hist, InitialAtAge = 1) {
  nSim <- nSim(Hist)
  SRR <- Hist@OM@Stock[[1]]@SRR
  Devs <- cbind(ExtendSims(SRR@RecDevHist[, 1, drop = FALSE], nSim),
                ExtendSims(SRR@RecDevInit, nSim) * InitialAtAge)
  Equil <- ExtendSims(Hist@Unfished@Equilibrium@Number[[1]][, , 1, , drop = FALSE], nSim)
  sweep(abind::adrop(Equil, 3), 1:2, Devs, `*`)
}

.InitialSB <- function(Hist) {
  N <- abind::adrop(Hist@Number[[1]][, , 1, , drop = FALSE], 3) |> apply(1:2, sum)
  W <- abind::adrop(Hist@OM@Stock[[1]]@Weight@MeanAtAge[, , 1, drop = FALSE], 3) |> ExtendSims(nSim(Hist))
  M <- abind::adrop(Hist@OM@Stock[[1]]@Maturity@MeanAtAge[, , 1, drop = FALSE], 3) |> ExtendSims(nSim(Hist))
  rowSums(N * W * M)
}

test_that(".CalcDynamicInitial(Unfished = TRUE) ignores Depletion@Initial", {
  skip_on_cran()
  OM <- SingleStockOM
  nSim(OM) <- 3
  OM@Stock[[1]]@Depletion <- Depletion(Initial = 0.3, Reference = "SB0")
  Hist <- .InitialHist(OM)

  Unfished <- .CalcDynamicInitial(Hist, Unfished = TRUE)
  expect_equal(unname(Unfished@Number[[1]][, , 1, ]),
               unname(.ExpectedInitialNumber(Hist)))

  Fished <- .CalcDynamicInitial(Hist)
  SB0 <- Hist@Unfished@Equilibrium@SBiomass[, 1, 1] |> rep_len(3)
  expect_equal(unname(.InitialSB(Fished) / SB0), rep(0.3, 3), tolerance = 1e-3)
})

test_that("Depletion@Initial does not change the dynamic unfished conditions", {
  skip_on_cran()
  OM <- SingleStockOM
  nSim(OM) <- 3
  Base <- CalcUnfished_Dynamic(OM, silent = TRUE)

  OM@Stock[[1]]@Depletion <- Depletion(Initial = 0.3, Reference = "SB0")
  Dep <- CalcUnfished_Dynamic(OM, silent = TRUE)
  expect_equal(Dep@SBiomass, Base@SBiomass)
  expect_equal(Dep@Number, Base@Number)

  Hist <- Simulate(OM, silent = TRUE)
  expect_equal(Hist@Unfished@Dynamic@SBiomass, Base@SBiomass)
  Fished <- .CalcDynamicInitial(.InitialHist(OM))
  expect_equal(Hist@Number[[1]][, , 1, ], Fished@Number[[1]][, , 1, ])
})

test_that("InitialAtAge applies to the fished dynamics only", {
  skip_on_cran()
  OM <- SingleStockOM
  nSim(OM) <- 3
  Base <- .InitialHist(OM)
  BaseUnfished <- CalcUnfished_Dynamic(OM, silent = TRUE)

  Ages <- OM@Stock[[1]]@Ages@Classes[-1]
  InitialAtAge <- array(exp(-0.1 * Ages), dim = c(1, length(Ages)),
                        dimnames = list(Sim = 1, Age = Ages))
  OM@Stock[[1]]@Depletion@Misc$InitialAtAge <- InitialAtAge
  Hist <- .InitialHist(OM)

  Fished <- .CalcDynamicInitial(Hist)
  Mult <- matrix(InitialAtAge, nrow = 3, ncol = length(Ages), byrow = TRUE)
  expect_equal(unname(Fished@Number[[1]][, , 1, ]),
               unname(.ExpectedInitialNumber(Hist, Mult)))

  Unfished <- .CalcDynamicInitial(Hist, Unfished = TRUE)
  expect_equal(Unfished@Number[[1]][, , 1, ],
               .CalcDynamicInitial(Base, Unfished = TRUE)@Number[[1]][, , 1, ])
  expect_equal(CalcUnfished_Dynamic(OM, silent = TRUE)@SBiomass, BaseUnfished@SBiomass)
})
