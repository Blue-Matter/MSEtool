# .OptFinalDepletion() (R/opt-final-depletion.R) groups stocks by SRR@SPFrom
# connectivity (R/populate-om.R:.SPFromConnectedGroups()), independent of
# OM@Complexes. Source (self-recruiting) stocks get an independently-fit
# catchability scale; dependent stocks' scale is derived as the weighted
# average of their source(s)' fitted scale, and the depletion objective is
# matched once per group (not once per stock) against the group's source(s)'
# own Depletion@Final/Reference.

skip_on_cran()

.build_hist_for_depletion_test <- function(om) {
  OM   <- MSEtool:::.StartUp(om, NULL, silent = TRUE)
  Hist <- MSEtool:::.OM2Hist(OM, TRUE)
  Hist@Unfished@Equilibrium <- CalcUnfished_Equilibrium(OM, TRUE)
  Hist <- MSEtool:::.CalcDynamicInitial(Hist)
  MSEtool:::.PrepHistMisc(Hist)
}

.make_spfrom_depletion_om <- function(final1 = NULL, final2 = NULL, nSim = 2) {
  data(MultiStockOM, envir = environment())
  om <- MultiStockOM
  om@nSim <- nSim
  sn <- StockNames(om)

  om@Stock[[1]]@Depletion@Final <- final1
  if (!is.null(final1)) om@Stock[[1]]@Depletion@Reference <- 'B0'

  om@Stock[[2]]@SRR@SPFrom <- sn[1]
  om@Stock[[2]]@Depletion@Final <- final2
  if (!is.null(final2)) om@Stock[[2]]@Depletion@Reference <- 'B0'

  om
}

## ---- Unit tests: .DeriveStockQ ----

test_that(".DeriveStockQ derives a dependent's scale as its source's scale (single source)", {
  ActiveInfo <- list(
    groups = list(list(members = 1:2, sources = 1L, dependents = 2L,
                        weights = list(list(from = 1L, weight = 1)))),
    sources = 1L,
    stocks  = 1:2
  )
  qStock <- MSEtool:::.DeriveStockQ(qSource = 2.5, ActiveInfo = ActiveInfo, nStock = 2)
  expect_equal(qStock, c(2.5, 2.5))
})

test_that(".DeriveStockQ computes a weighted average for a multi-source dependent", {
  ActiveInfo <- list(
    groups = list(list(members = 1:3, sources = c(1L, 2L), dependents = 3L,
                        weights = list(list(from = c(1L, 2L), weight = c(0.6, 0.4))))),
    sources = c(1L, 2L),
    stocks  = 1:3
  )
  qStock <- MSEtool:::.DeriveStockQ(qSource = c(2, 4), ActiveInfo = ActiveInfo, nStock = 3)
  expect_equal(qStock[3], 0.6 * 2 + 0.4 * 4)
})

## ---- Unit tests: .SPFromConnectedGroups / .ResolveGroupTarget ----

test_that(".SPFromConnectedGroups groups an SPFrom-linked pair and leaves an unrelated stock separate", {
  om <- .make_spfrom_depletion_om(final1 = c(0.3, 0.3), nSim = 1)
  stC <- om@Stock[[1]]
  Name(stC) <- "Stock C"
  om@Stock <- list(om@Stock[[1]], om@Stock[[2]], stC)
  om@Fleet <- c(om@Fleet, stats::setNames(list(om@Fleet[[1]]), "Stock C"))
  om@Obs   <- c(om@Obs,   stats::setNames(list(om@Obs[[1]]), "Stock C"))
  om@Imp   <- c(om@Imp,   stats::setNames(list(om@Imp[[1]]), "Stock C"))

  Groups <- MSEtool:::.SPFromConnectedGroups(om)
  expect_length(Groups, 2)

  sizes <- sort(purrr::map_int(Groups, \(g) length(g$members)))
  expect_equal(sizes, c(1, 2))

  pair <- purrr::keep(Groups, \(g) length(g$members) == 2)[[1]]
  expect_equal(pair$sources, 1L)
  expect_equal(pair$dependents, 2L)
})

test_that(".ResolveGroupTarget flags a group inactive when its source has no target", {
  om <- .make_spfrom_depletion_om(final1 = NULL, final2 = NULL, nSim = 1)
  Groups <- MSEtool:::.SPFromConnectedGroups(om)
  Resolved <- purrr::map(Groups, MSEtool:::.ResolveGroupTarget, OM = om)
  expect_false(Resolved[[1]]$active)
})

test_that(".ResolveGroupTarget ignores a dependent's own (differing) target", {
  om <- .make_spfrom_depletion_om(final1 = 0.3, final2 = 0.9, nSim = 1)
  Groups <- MSEtool:::.SPFromConnectedGroups(om)
  Resolved <- purrr::map(Groups, MSEtool:::.ResolveGroupTarget, OM = om)
  expect_true(Resolved[[1]]$active)
  expect_equal(Resolved[[1]]$value, 0.3)
})

test_that(".ResolveGroupTarget aborts when multiple sources of a shared dependent disagree", {
  om <- .make_spfrom_depletion_om(final1 = 0.3, nSim = 1)
  sn <- StockNames(om)
  stC <- om@Stock[[1]]
  Name(stC) <- "Stock C"
  stC@Depletion@Final <- 0.6  # conflicts with Stock 1's 0.3
  om@Stock <- list(om@Stock[[1]], om@Stock[[2]], stC)
  om@Fleet <- c(om@Fleet, stats::setNames(list(om@Fleet[[1]]), "Stock C"))
  om@Obs   <- c(om@Obs,   stats::setNames(list(om@Obs[[1]]), "Stock C"))
  om@Imp   <- c(om@Imp,   stats::setNames(list(om@Imp[[1]]), "Stock C"))

  # Stock 2 now draws from both Stock 1 and Stock C, which disagree
  om@Stock[[2]]@SRR@SPFrom <- c(0.5, 0.5) |> stats::setNames(c(sn[1], "Stock C"))

  Groups <- MSEtool:::.SPFromConnectedGroups(om)
  pair <- purrr::keep(Groups, \(g) length(g$members) == 3)[[1]]
  expect_error(MSEtool:::.ResolveGroupTarget(pair, om))
})

## ---- Integration tests: full .OptFinalDepletion() ----

test_that("an SPFrom-linked group's aggregate B0 depletion matches its source's target", {
  om <- .make_spfrom_depletion_om(final1 = c(0.3, 0.3), nSim = 2)
  Hist <- .build_hist_for_depletion_test(om)
  Hist <- MSEtool:::.OptFinalDepletion(Hist, silent = TRUE)

  TermInd <- length(Years(Hist@OM, 'Historical'))
  PopDyn <- CalcFisheryDynamics_(
    MSEtool:::.PrepHistMisc(Hist), Years = Years(Hist@OM, 'Historical'),
    AllYears = Years(Hist@OM, 'Historical'), Sims = seq_len(nSim(Hist)), nSim = nSim(Hist),
    nStock(Hist@OM), nFleet(Hist@OM), nArea(Hist), DoCalcCatch = 1, clone = 1)

  AggDep <- (PopDyn@Biomass[, 1, TermInd] + PopDyn@Biomass[, 2, TermInd]) /
    (Hist@Unfished@Equilibrium@Biomass[, 1, TermInd] + Hist@Unfished@Equilibrium@Biomass[, 2, TermInd])

  expect_equal(unname(AggDep), rep(0.3, nSim(Hist)), tolerance = 1e-3)
})

test_that("a group with no target on its source is left untouched", {
  om <- .make_spfrom_depletion_om(final1 = NULL, final2 = NULL, nSim = 1)
  Hist <- .build_hist_for_depletion_test(om)
  BaselineEff <- purrr::map(Hist@OM@Fleet, \(fl) purrr::map(fl, \(f) f@Catchability@Efficiency))

  HistOut <- MSEtool:::.OptFinalDepletion(Hist, silent = TRUE)
  OutEff <- purrr::map(HistOut@OM@Fleet, \(fl) purrr::map(fl, \(f) f@Catchability@Efficiency))

  expect_equal(OutEff, BaselineEff)
})

test_that("an unrelated independent stock is unaffected by a separate SPFrom-linked pair", {
  om <- .make_spfrom_depletion_om(final1 = c(0.3, 0.3), nSim = 1)
  stC <- om@Stock[[1]]
  Name(stC) <- "Stock C"
  stC@Depletion@Final <- c(0.5, 0.5)
  om@Stock <- list(om@Stock[[1]], om@Stock[[2]], stC)
  om@Fleet <- c(om@Fleet, stats::setNames(list(om@Fleet[[1]]), "Stock C"))
  om@Obs   <- c(om@Obs,   stats::setNames(list(om@Obs[[1]]), "Stock C"))
  om@Imp   <- c(om@Imp,   stats::setNames(list(om@Imp[[1]]), "Stock C"))

  Hist <- .build_hist_for_depletion_test(om)
  Hist <- MSEtool:::.OptFinalDepletion(Hist, silent = TRUE)

  TermInd <- length(Years(Hist@OM, 'Historical'))
  PopDyn <- CalcFisheryDynamics_(
    MSEtool:::.PrepHistMisc(Hist), Years = Years(Hist@OM, 'Historical'),
    AllYears = Years(Hist@OM, 'Historical'), Sims = 1, nSim = 1,
    nStock(Hist@OM), nFleet(Hist@OM), nArea(Hist), DoCalcCatch = 1, clone = 1)

  DepC <- PopDyn@Biomass[1, 3, TermInd] / Hist@Unfished@Equilibrium@Biomass[1, 3, TermInd]
  expect_equal(DepC, 0.5, tolerance = 1e-3)
})
