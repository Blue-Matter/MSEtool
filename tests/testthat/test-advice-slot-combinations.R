# Every combination of the Advice slots on a two-fleet, two-area OM: TAC,
# Effort, or neither, with or without Closure, BagLimit, Selectivity,
# Retention, and DiscardMortality. Each MP is checked for the effect of every
# slot it sets, and for no effect of the slots it does not set.

skip_on_cran()

.ComboOM <- function() {
  OM <- TwoFleetOM
  nSim(OM)  <- 2
  pYear(OM) <- 2
  # closing an area concentrates F in the open one; keep it below the maxF cap
  OM@maxF   <- 10
  for (fl in seq_along(OM@Fleet[[1]])) {
    OM@Fleet[[1]][[fl]]@Effort@TripsScalar <- 20
    OM@Fleet[[1]][[fl]]@Effort@Theta       <- 2
  }
  OM
}
.ComboHist  <- Simulate(.ComboOM(), silent = TRUE)
.ComboHistY <- Years(.ComboHist, 'H')
.ComboTAC   <- local({
  L <- .ComboHist@Landings
  LastYear <- apply(L, match('Year', names(dimnames(L))), sum)
  0.5 * LastYear[length(LastYear)] / nSim(.ComboHist)
})

.ComboSlots <- list(
  Closure          = \(A) { A@Closure <- c(1, 0); A },
  BagLimit         = \(A) { A@BagLimit <- c(1, NA); A@LimitType <- 'boat'; A },
  Selectivity      = \(A) { A@Selectivity <- Selectivity(Pars = list(L5 = 30, LFS = 60)); A },
  Retention        = \(A) { A@Retention <- Retention(Pars = list(RL50 = 60, RL50_95 = 10)); A },
  DiscardMortality = \(A) { A@DiscardMortality <- DiscardMortality(MeanAtAge = 0.5); A }
)

.ComboGrid <- expand.grid(Control = c('None', 'TAC', 'Effort'),
                          Closure = c(FALSE, TRUE), BagLimit = c(FALSE, TRUE),
                          Selectivity = c(FALSE, TRUE), Retention = c(FALSE, TRUE),
                          DiscardMortality = c(FALSE, TRUE), stringsAsFactors = FALSE)
.ComboGrid$MP <- apply(.ComboGrid, 1, \(r) {
  On <- names(.ComboSlots)[as.logical(r[names(.ComboSlots)])]
  paste(c(r[['Control']], On), collapse = '_')
})

.ComboMP <- function(Row) {
  mp <- function(Data) {
    A <- switch(Row$Control,
                None   = Advice(),
                TAC    = Advice(TAC = .ComboTAC),
                Effort = Advice(Effort = 0.8))
    for (sl in names(.ComboSlots))
      if (Row[[sl]]) A <- .ComboSlots[[sl]](A)
    A
  }
  class(mp) <- 'mp'
  mp
}

.ComboMSE <- suppressMessages(Project(
  .ComboHist,
  MPs = stats::setNames(lapply(seq_len(nrow(.ComboGrid)), \(i) .ComboMP(.ComboGrid[i, ])), .ComboGrid$MP),
  parallel = FALSE, silent = TRUE
))

test_that("every combination of Advice slots projects without error", {
  expect_equal(nrow(.ComboGrid), 96)
  expect_length(.ComboMSE@Log$error, 0)
  expect_true(all(Discards(.ComboMSE)$Value >= -1e-8, na.rm = TRUE))
})

test_that("Closure stops all fishing in the closed area, and only when set", {
  FA <- .ComboMSE@FInteractArea[[1]]
  ProjY <- setdiff(dimnames(FA)$Year, as.character(.ComboHistY))
  for (i in seq_len(nrow(.ComboGrid))) {
    mp <- .ComboGrid$MP[i]
    Closed <- FA[, , ProjY, , '2', mp]
    if (.ComboGrid$Closure[i]) {
      expect_true(all(Closed == 0), label = mp)
    } else {
      expect_true(any(Closed > 0), label = mp)
    }
    expect_true(any(FA[, , ProjY, , '1', mp] > 0), label = mp)
  }
})

test_that("a TAC is never exceeded, and applies only when set", {
  Att <- TACAttainment(.ComboMSE)
  for (i in seq_len(nrow(.ComboGrid))) {
    mp <- .ComboGrid$MP[i]
    Rows <- Att[Att$MP == mp, , drop = FALSE]
    if (.ComboGrid$Control[i] == 'TAC') {
      expect_gt(nrow(Rows), 0)
      expect_true(all(Rows$Catch <= Rows$TAC * (1 + 1e-4) + 1e-8), label = mp)
    } else {
      expect_equal(nrow(Rows), 0, label = mp)
    }
  }
})

test_that("relative effort advice sets each fleet's effort", {
  E <- .ComboMSE@Effort
  LastHist <- .ComboHist@Effort[, length(.ComboHistY), ]
  for (i in which(.ComboGrid$Control == 'Effort')) {
    mp <- .ComboGrid$MP[i]
    for (y in seq_len(dim(E)[2]))
      expect_equal(E[, y, , mp], 0.8 * LastHist, ignore_attr = TRUE, tolerance = 1e-6, label = mp)
  }
})

test_that("a bag limit applies to its fleet only, and only when set", {
  Bag <- BagLimitEffect(.ComboMSE)
  for (i in seq_len(nrow(.ComboGrid))) {
    mp <- .ComboGrid$MP[i]
    Rows <- Bag[Bag$MP == mp, , drop = FALSE]
    if (.ComboGrid$BagLimit[i]) {
      expect_gt(nrow(Rows), 0)
      expect_true(all(Rows$Fleet == FleetNames(.ComboHist)[1]), label = mp)
      expect_true(any(Rows$RetentionFraction < 1), label = mp)
    } else {
      expect_equal(nrow(Rows), 0, label = mp)
    }
  }
})

test_that("Selectivity, Retention, and DiscardMortality advice apply only when set, whatever else is advised", {
  Rec <- \(sl, mp) .ComboMSE@Misc[[sl]][[mp]]
  Ref <- \(sl, Row) {
    Base <- .ComboGrid$Control == 'None' & !.ComboGrid$Closure & !.ComboGrid$BagLimit
    for (s in c('Selectivity', 'Retention', 'DiscardMortality'))
      Base <- Base & .ComboGrid[[s]] == (s == sl || (sl == 'Retention' && s == 'Selectivity' && Row$Selectivity))
    .ComboGrid$MP[Base]
  }
  for (i in seq_len(nrow(.ComboGrid))) {
    Row <- .ComboGrid[i, ]
    for (sl in c('Selectivity', 'Retention', 'DiscardMortality')) {
      if (Row[[sl]]) {
        RefMP <- Ref(sl, Row)
        expect_length(RefMP, 1)
        expect_equal(Rec(sl, Row$MP), Rec(sl, RefMP), label = paste(Row$MP, sl))
      } else {
        expect_null(Rec(sl, Row$MP), label = paste(Row$MP, sl))
      }
    }
  }
})

test_that("ReplayMP() replays an MP that returns per-fleet schedules and NULL fleets", {
  skip_if_not_installed('gganimate')
  OM <- .ComboOM()
  pYear(OM) <- 3
  Hist <- Simulate(OM, silent = TRUE)
  SchedMP <- function(Data) {
    A <- Advice(TAC = .ComboTAC)
    A@Closure <- c(1, 0)
    A@Selectivity <- list(Selectivity(Pars = list(L5 = 30, LFS = 60)), NULL)
    A@Retention <- list(NULL, Retention(Pars = list(RL50 = 60, RL50_95 = 10)))
    A@DiscardMortality <- DiscardMortality(MeanAtAge = 0.5)
    A
  }
  class(SchedMP) <- 'mp'
  MSE <- suppressMessages(Project(Hist, MPs = list(SchedMP = SchedMP), parallel = FALSE, silent = TRUE))
  expect_length(MSE@Log$error, 0)
  R <- expect_no_warning(suppressMessages(ReplayMP(MSE, 'SchedMP', Sim = 1, Status = NULL)))
  Tab <- R$Table
  expect_true(all(abs(Tab$Advice - Tab$Recorded) < 1e-6 * pmax(1, abs(Tab$Recorded)), na.rm = TRUE))
})
