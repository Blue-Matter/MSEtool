test_that(".SubsetSim() returns array sims in the order of `Sims`", {
  A <- array(1:12, dim = c(4, 3), dimnames = list(Sim = 1:4, Year = 2001:2003))
  Sub <- .SubsetSim(A, c(3, 1))
  expect_identical(unname(Sub[, 1]), c(3L, 1L))
  expect_identical(dimnames(Sub)$Sim, c("1", "2"))
  expect_identical(dimnames(.SubsetSim(A, c(3, 1), keep_sim_name = TRUE))$Sim, c("3", "1"))

  B <- aperm(A, c(2, 1))
  expect_identical(unname(.SubsetSim(B, c(4, 2))[1, ]), c(4L, 2L))

  One <- A[1, , drop = FALSE]
  expect_identical(.SubsetSim(One, c(3, 1)), One)
  expect_error(.SubsetSim(A, c(2, 5)), "not found")
})

test_that(".SubsetSim() subsets sim-keyed vectors, lists and Sim-column data frames", {
  Vec  <- stats::setNames(c(10, 20, 30, 40), 1:4)
  Chr  <- stats::setNames(letters[1:4], 1:4)
  Plain <- c(10, 20, 30, 40)
  DF   <- data.frame(Sim = 1:4, AC = Plain)
  Lst  <- stats::setNames(as.list(Plain), 1:4)
  Yrs  <- stats::setNames(as.list(Plain), 2001:2004)
  x <- list(Vec = Vec, Chr = Chr, Plain = Plain, DF = DF, Lst = Lst, Yrs = Yrs)

  Sub <- .SubsetSim(x, c(4, 2), nSim = 4)
  expect_identical(Sub$Vec, stats::setNames(c(40, 20), 1:2))
  expect_identical(Sub$Chr, stats::setNames(c("d", "b"), 1:2))
  expect_identical(Sub$Plain, Plain)
  expect_identical(Sub$DF, data.frame(Sim = 1:2, AC = c(40, 20)))
  expect_identical(unlist(Sub$Lst, use.names = FALSE), c(40, 20))
  expect_identical(Sub$Yrs, Yrs)

  # without nSim, only lists named 1..n by position are sim-keyed
  NoN <- .SubsetSim(x, c(4, 2))
  expect_identical(NoN$Vec, Vec)
  expect_identical(NoN$DF, Sub$DF)
  expect_identical(NoN$Lst, stats::setNames(Lst[c(4, 2)], 1:2))
  expect_identical(.SubsetSim(data.frame(Sim = 1, AC = 1), 3), data.frame(Sim = 1, AC = 1))
})

# ---- conditioned observation model ----

.ConditionedHist <- function(nSim = 11) {
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 1
  om@pYear <- 10
  set.seed(1)
  h0 <- Simulate(om, silent = TRUE)
  fl <- FleetNames(h0)[1]
  HY <- Years(h0, "H")
  NomIndex <- .CalcNomIndex(Number_List = h0@Number[1], object = h0, stocks = 1, fleet = fl,
                            IndexObs = IndicesObs(), Years = HY)
  Val <- as.numeric(NomIndex[1, ])^0.6 * exp(stats::rnorm(length(HY), 0, 0.2))
  Surv <- Val * exp(stats::rnorm(length(HY), 0, 0.3))
  om@Data <- list(Data(Years = c(HY, Years(h0, "P")),
                       CPUE  = IndicesData(Name = fl,
                                           Value = matrix(Val / mean(Val), ncol = 1,
                                                          dimnames = list(as.character(HY), fl))),
                       Survey = IndicesData(Name = "Surv",
                                            Value = matrix(Surv / mean(Surv), ncol = 1,
                                                           dimnames = list(as.character(HY), "Surv")))))
  om@Obs[[1]][["Surv"]] <- Obs("Surv", Survey = IndicesObs(Selectivity = "Biomass"))
  om@nSim <- nSim
  set.seed(1)
  Simulate(om, control = SimControl(EstimateBeta = TRUE), silent = TRUE)
}

# paths of leaves sized to `nSim` that are not identified by sim, or Sim-column frames covering other sims
.UnkeyedSimPaths <- function(x, nSim, path = "") {
  if (is.null(x) || is.function(x) || is.environment(x)) return(character())
  if (isS4(x))
    return(unlist(lapply(methods::slotNames(x), \(s)
      .UnkeyedSimPaths(methods::slot(x, s), nSim, paste0(path, "@", s)))))
  if (is.data.frame(x))
    return(if ("Sim" %in% names(x) && !all(x$Sim %in% seq_len(nSim))) path else character())
  if (is.list(x)) {
    nms <- names(x) %||% rep("", length(x))
    return(unlist(lapply(seq_along(x), \(i)
      .UnkeyedSimPaths(x[[i]], nSim, paste0(path, "[[", if (nzchar(nms[i])) nms[i] else i, "]]")))))
  }
  if (!is.null(dim(x))) {
    DN <- names(dimnames(x))
    if ("Sim" %in% DN) return(if (dim(x)[match("Sim", DN)] %in% c(1, nSim)) character() else path)
    return(if (any(dim(x) == nSim)) path else character())
  }
  if (length(x) == nSim && !identical(names(x), as.character(seq_len(nSim)))) path else character()
}

.ConditionedObs <- function(hist, type = "CPUE")
  methods::slot(hist@OM@Obs[[1]][[if (type == "CPUE") 1 else "Surv"]], type)

test_that("conditioned per-sim observation parameters are subset with their sims", {
  skip_on_cran()
  hist <- .ConditionedHist()
  expect_identical(.UnkeyedSimPaths(hist, 11), character())

  Sims <- c(9, 2, 7, 4)
  Sub  <- Subset(hist, Sims = Sims)
  expect_identical(setdiff(.UnkeyedSimPaths(Sub, length(Sims)), "@OM@Misc[[SimIDs]]"), character())
  for (type in c("CPUE", "Survey")) {
    Full <- .ConditionedObs(hist, type)
    Obs  <- .ConditionedObs(Sub, type)
    for (sl in c("Beta", "Efficiency", "CV", "AC"))
      expect_identical(methods::slot(Obs, sl), stats::setNames(methods::slot(Full, sl)[Sims], 1:4),
                       label = paste(type, sl))
    expect_identical(Obs@Stats$AC, Full@Stats$AC[Sims])
    expect_identical(Obs@Stats$Sim, 1:4)
    expect_identical(Obs@Misc$BetaFit$Status, stats::setNames(Full@Misc$BetaFit$Status[Sims], 1:4))
  }
  expect_identical(Sub@OM@Misc$SimIDs, Sims)
  expect_equal(Sub@OM@Misc$nSimGlobal, 11)

  expect_identical(setdiff(.UnkeyedSimPaths(ReduceNSim(hist, 5), 5), "@OM@Control[[DataCV]]"),
                   character())
  expect_null(ReduceNSim(hist, 5)@OM@Misc$SimIDs)
})

test_that("a non-contiguous Subset() projects the same as those sims of the full run", {
  skip_on_cran()
  hist <- .ConditionedHist()

  StochCatch <- function(Data) {
    Adv <- CurrentCatch(Data)
    Adv@TAC <- Adv@TAC * stats::rlnorm(1, 0, 0.3)
    Adv
  }
  class(StochCatch) <- "mp"
  MPs <- list(IR = "IndexRate", SC = StochCatch)

  Sims <- c(2, 5, 6, 9, 11)
  Full <- Project(hist, MPs = MPs, silent = TRUE)
  Sub  <- Subset(hist, Sims = Sims)
  Seq  <- Project(Sub, MPs = MPs, silent = TRUE)
  Sub@OM@Control$ProjectChunks <- 2
  Chk  <- Project(Sub, MPs = MPs, silent = TRUE)

  for (sl in c("SBiomass", "Landings", "Effort")) {
    Want <- .SubsetSim(methods::slot(Full, sl), Sims)
    expect_identical(methods::slot(Seq, sl), Want, label = paste("sequential", sl))
    expect_identical(methods::slot(Chk, sl), Want, label = paste("chunked", sl))
  }
})

test_that("Subset() of an unpopulated OM with unnamed arrays simulates the same as those sims of the full run", {
  skip_on_cran()
  om <- SingleStockOM
  om@nSim <- 4
  Stay <- c(0.9, 0.7, 0.5, 0.3)
  Mov <- array(NA, dim = c(4, 2, 2))
  for (s in 1:4)
    Mov[s, , ] <- matrix(c(Stay[s], 1 - Stay[s], 1 - Stay[s], Stay[s]), 2, byrow = TRUE)
  om@Stock[[1]]@Spatial@Movement <- Mov

  Sims <- c(4, 1, 2)
  Sub <- Subset(om, Sims = Sims)
  expect_identical(dimnames(Sub@Stock[[1]]@Spatial@Movement)$Sim, c("1", "2", "3"))

  Full <- Simulate(om, silent = TRUE)
  Hist <- Simulate(Sub, silent = TRUE)
  for (sl in c("Number", "Biomass", "Effort", "FDead"))
    expect_equal(methods::slot(Hist, sl), methods::slot(Subset(Full, Sims = Sims), sl), label = sl)
})

test_that("a 2-sim Subset() of a populated OM simulates the same as those sims of the full run", {
  skip_on_cran()
  om <- SingleStockOM
  om@nSim <- 4
  Pop <- Populate(om, silent = TRUE)
  Sims <- c(4, 2)
  Sub <- Subset(Pop, Sims = Sims)

  Full <- Simulate(Pop, silent = TRUE)
  Hist <- Simulate(Sub, silent = TRUE)
  expect_equal(unname(Hist@OM@Stock[[1]]@Depletion@Final),
               unname(Pop@Stock[[1]]@Depletion@Final[Sims]))
  for (sl in c("Number", "Biomass", "Effort", "FDead"))
    expect_equal(methods::slot(Hist, sl), methods::slot(Subset(Full, Sims = Sims), sl), label = sl)
})

test_that("extractors of a Subset() MSE number sims 1..nSim", {
  skip_on_cran()
  hist <- .ConditionedHist(nSim = 6)
  Sims <- c(5, 2, 6)
  Sub  <- Subset(hist, Sims = Sims)
  expect_identical(names(Sub@Data), if (length(hist@Data) > 1) c("1", "2", "3") else names(hist@Data))

  SP <- SetMPArgs(SurplusProduction, IndexSource = "CPUE")
  for (chunks in list(NULL, 2)) {
    Sub@OM@Control$ProjectChunks <- chunks
    MSE <- Project(Sub, MPs = list(SP = SP), silent = TRUE)
    Want <- 1:3
    expect_setequal(unique(TACs(MSE)$Sim), Want)
    expect_setequal(unique(Removals(MSE, df = TRUE)$Sim), Want)
    expect_setequal(unique(SB_SBMSY(MSE, df = TRUE)$Sim), Want)
    expect_setequal(unique(SPEstimates(MSE)$Sim), Want)
    expect_identical(names(PPD(MSE)[[1]]), as.character(Want))
  }
})

test_that("CalcAgeSizeKey() numbers sims 1..n when only CVatAge varies by sim", {
  Mean <- array(seq(5, 50, length.out = 10), c(1, 10, 1),
                dimnames = list(Sim = 1, Age = 0:9, Year = 2020))
  CV <- array(c(0.1, 0.15, 0.2), c(3, 1, 1), dimnames = list(Sim = 1:3, Age = 0, Year = 2020))
  Classes <- seq(1, 60, 2)
  ALK <- CalcAgeSizeKey(Mean, CV, Classes = Classes, silent = TRUE)
  expect_identical(dimnames(ALK)$Sim, c("1", "2", "3"))
  for (s in 1:3) {
    One <- CalcAgeSizeKey(Mean, CV[s, , , drop = FALSE], Classes = Classes, silent = TRUE)
    expect_equal(unname(ALK[s, , , ]), unname(One[1, , , ]))
  }
  expect_identical(dimnames(.SubsetSim(ALK, 3:1, keep_sim_name = TRUE))$Sim, c("3", "2", "1"))
})

test_that(".RepairSimDimnames() renumbers only duplicated or NA sims", {
  A <- array(1:8, c(4, 2), dimnames = list(Sim = rep(1, 4), Year = 1:2))
  B <- array(1:4, c(2, 2), dimnames = list(Sim = c(5, 7), Year = 1:2))
  x <- list(A = A, B = B, One = A[1, , drop = FALSE])
  Fix <- .RepairSimDimnames(x)
  expect_identical(dimnames(Fix$A)$Sim, as.character(1:4))
  expect_identical(Fix$B, B)
  expect_identical(Fix$One, x$One)
})

test_that("an OM with duplicated Sim dimnames still simulates", {
  skip_on_cran()
  om <- SingleStockOM
  om@nSim <- 4
  Pop <- Populate(om, silent = TRUE)
  ALK <- Pop@Stock[[1]]@Length@ALK
  skip_if(is.null(ALK) || dim(ALK)[1] == 1)
  Bad <- Pop
  dimnames(Bad@Stock[[1]]@Length@ALK)$Sim <- rep("1", dim(ALK)[1])
  Hist <- Simulate(Bad, silent = TRUE)
  expect_equal(Hist@Biomass, Simulate(Pop, silent = TRUE)@Biomass)
})
