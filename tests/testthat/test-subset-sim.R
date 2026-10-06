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
  expect_identical(NoN$Lst, Lst[c(4, 2)])
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
  om@Data <- list(Data(Years = c(HY, Years(h0, "P")),
                       CPUE  = IndicesData(Name = fl,
                                           Value = matrix(Val / mean(Val), ncol = 1,
                                                          dimnames = list(as.character(HY), fl)))))
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

.ConditionedObs <- function(hist) hist@OM@Obs[[1]][[1]]@CPUE

test_that("conditioned per-sim observation parameters are subset with their sims", {
  skip_on_cran()
  hist <- .ConditionedHist()
  Full <- .ConditionedObs(hist)
  expect_identical(.UnkeyedSimPaths(hist, 11), character())

  Sims <- c(9, 2, 7, 4)
  Sub  <- Subset(hist, Sims = Sims)
  Obs  <- .ConditionedObs(Sub)
  expect_identical(setdiff(.UnkeyedSimPaths(Sub, length(Sims)), "@OM@Misc[[SimIDs]]"), character())
  for (sl in c("Beta", "Efficiency", "CV", "AC"))
    expect_identical(unname(methods::slot(Obs, sl)), unname(methods::slot(Full, sl)[Sims]), label = sl)
  expect_identical(Obs@Stats$AC, Full@Stats$AC[Sims])
  expect_identical(Obs@Misc$BetaFit$Status, stats::setNames(Full@Misc$BetaFit$Status[Sims], 1:4))
  expect_identical(Sub@OM@Misc$SimIDs, Sims)
  expect_equal(Sub@OM@Misc$nSimGlobal, 11)

  expect_identical(.UnkeyedSimPaths(ReduceNSim(hist, 5), 5), character())
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
