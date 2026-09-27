test_that(".GetBAMInitRecDevs maps log.Nage.dev to age classes", {
  BAMdata <- list(parm.avec = list(age = 1:4, log.Nage.dev = c(NA, log(0.5), 0, log(2))))
  expect_equal(.GetBAMInitRecDevs(BAMdata, 1:5), c(1, 0.5, 1, 2, 1))
  expect_equal(.GetBAMInitRecDevs(list(), 1:3), c(1, 1, 1))
})

test_that("ImportBAM splits initial numbers-at-age into rec devs and InitialAtAge", {
  skip_on_cran()
  skip_if_not_installed("bamExtras")
  BAMdata <- GetBAMOutput("RedGrouper")

  OM <- suppressMessages(ImportBAM("RedGrouper", nSim = 2, pYear = 3, silent = TRUE))
  Stock <- OM@Stock[[1]]
  Ages <- Stock@Ages@Classes
  expect_equal(as.numeric(Stock@SRR@RecDevInit[1, ]),
               .GetBAMInitRecDevs(BAMdata, Ages)[-1])
  expect_false(is.null(Stock@Depletion@Misc$InitialAtAge))

  Hist <- Simulate(OM, silent = TRUE)
  N1 <- apply(Hist@Number[[1]][1, , 1, , drop = FALSE], 2, sum)
  expect_equal(unname(N1[-1]), unname(BAMdata$N.age[1, -1]), tolerance = 1e-6)

  Init <- .OM2Hist(OM, silent = TRUE)
  Init@Unfished@Equilibrium <- CalcUnfished_Equilibrium(Init@OM, silent = TRUE)
  Unfished <- .CalcDynamicInitial(Init, Unfished = TRUE)
  Equil <- apply(Init@Unfished@Equilibrium@Number[[1]][1, , 1, , drop = FALSE], 2, sum)
  expect_equal(unname(apply(Unfished@Number[[1]][1, -1, 1, , drop = FALSE], 2, sum)),
               unname(Equil[-1] * Stock@SRR@RecDevInit[1, ]))
})
