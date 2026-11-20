# Advice@Selectivity/Retention/DiscardMortality arrangements on a two-fleet OM:
# single object vs per-fleet list (positional, named, NULL entries) for each way
# of defining the schedule, plus the invalid arrangements.

skip_on_cran()

.ArrOM <- function() {
  OM <- TwoFleetOM
  nSim(OM)  <- 2
  pYear(OM) <- 2
  OM
}
.ArrHist  <- Simulate(.ArrOM(), silent = TRUE)
.ArrFl    <- FleetNames(.ArrHist)
.ArrCl    <- .ArrHist@OM@Stock[[1]]@Length@Classes
.ArrAg    <- .ArrHist@OM@Stock[[1]]@Ages@Classes
.ArrOwnCl <- seq(0, max(.ArrCl), by = 10)

.ArrMP <- function(SlotName, value) {
  mp <- function(Data) {
    A <- CurrentEffort(Data)
    methods::slot(A, SlotName) <- value
    A
  }
  class(mp) <- 'mp'
  mp
}

.ArrProject <- function(MPs)
  suppressMessages(Project(.ArrHist, MPs = MPs, parallel = FALSE, silent = TRUE))

# projected schedule of `fl` under `mp`; NULL when unchanged from the OM
.ArrRecorded <- function(MSE, SlotName, mp, fl) MSE@Misc[[SlotName]][[mp]][[1]][[fl]]

.ArrErrors <- function(MSE, mp)
  vapply(Filter(\(e) identical(e$mp, mp), MSE@Log$error), \(e) gsub('\\s+', ' ', e$message), character(1))

# two different schedules (A, B) for each component and way of defining it
.ArrForms <- list(
  Selectivity = list(
    Pars          = list(Selectivity(Pars = list(L5 = 30, LFS = 60)),
                         Selectivity(Pars = list(L5 = 50, LFS = 80, Vmaxlen = 0.4))),
    MeanAtAge     = list(Selectivity(MeanAtAge = pmin(1, .ArrAg / 4)),
                         Selectivity(MeanAtAge = pmin(1, .ArrAg / 8))),
    MeanAtLength  = list(Selectivity(MeanAtLength = pmin(1, .ArrCl / 60), Classes = .ArrCl),
                         Selectivity(MeanAtLength = pmin(1, .ArrCl / 90), Classes = .ArrCl)),
    NoClasses     = list(Selectivity(MeanAtLength = pmin(1, .ArrCl / 60)),
                         Selectivity(MeanAtLength = pmin(1, .ArrCl / 90))),
    OwnClasses    = list(Selectivity(MeanAtLength = pmin(1, .ArrOwnCl / 60), Classes = .ArrOwnCl),
                         Selectivity(MeanAtLength = pmin(1, .ArrOwnCl / 90), Classes = .ArrOwnCl))
  ),
  Retention = list(
    Pars          = list(Retention(Pars = list(RL50 = 60, RL50_95 = 10)),
                         Retention(Pars = list(RL50 = 90, RL50_95 = 5))),
    MeanAtAge     = list(Retention(MeanAtAge = as.numeric(.ArrAg >= 3)),
                         Retention(MeanAtAge = as.numeric(.ArrAg >= 6))),
    MeanAtLength  = list(Retention(MeanAtLength = as.numeric(.ArrCl >= 60), Classes = .ArrCl),
                         Retention(MeanAtLength = as.numeric(.ArrCl >= 90), Classes = .ArrCl)),
    NoClasses     = list(Retention(MeanAtLength = as.numeric(.ArrCl >= 60)),
                         Retention(MeanAtLength = as.numeric(.ArrCl >= 90))),
    OwnClasses    = list(Retention(MeanAtLength = as.numeric(.ArrOwnCl >= 60), Classes = .ArrOwnCl),
                         Retention(MeanAtLength = as.numeric(.ArrOwnCl >= 90), Classes = .ArrOwnCl))
  ),
  DiscardMortality = list(
    MeanAtAge     = list(DiscardMortality(MeanAtAge = ifelse(.ArrAg >= 3, 0.2, 0.6)),
                         DiscardMortality(MeanAtAge = ifelse(.ArrAg >= 6, 0.1, 0.9))),
    MeanAtLength  = list(DiscardMortality(MeanAtLength = ifelse(.ArrCl >= 60, 0.2, 0.6), Classes = .ArrCl),
                         DiscardMortality(MeanAtLength = ifelse(.ArrCl >= 90, 0.1, 0.9), Classes = .ArrCl)),
    NoClasses     = list(DiscardMortality(MeanAtLength = ifelse(.ArrCl >= 60, 0.2, 0.6)),
                         DiscardMortality(MeanAtLength = ifelse(.ArrCl >= 90, 0.1, 0.9))),
    OwnClasses    = list(DiscardMortality(MeanAtLength = ifelse(.ArrOwnCl >= 60, 0.2, 0.6), Classes = .ArrOwnCl),
                         DiscardMortality(MeanAtLength = ifelse(.ArrOwnCl >= 90, 0.1, 0.9), Classes = .ArrOwnCl))
  )
)

for (sl in names(.ArrForms)) for (form in names(.ArrForms[[sl]])) {
  test_that(paste(sl, "advice by", form, "is applied for every arrangement"), {
    A <- .ArrForms[[sl]][[form]][[1]]
    B <- .ArrForms[[sl]][[form]][[2]]
    MPs <- list(
      SingleA  = .ArrMP(sl, A),
      SingleB  = .ArrMP(sl, B),
      ListAB   = .ArrMP(sl, list(A, B)),
      Named    = .ArrMP(sl, stats::setNames(list(B, A), rev(.ArrFl))),
      WithNULL = .ArrMP(sl, list(A, NULL)),
      AllNULL  = .ArrMP(sl, list(NULL, NULL)),
      LengthOne = .ArrMP(sl, list(A))
    )
    MSE <- .ArrProject(MPs)
    for (mp in names(MPs))
      expect_length(.ArrErrors(MSE, mp), 0)
    expect_true(all(Discards(MSE)$Value >= -1e-8, na.rm = TRUE))

    Rec <- \(mp, fl) .ArrRecorded(MSE, sl, mp, fl)
    for (fl in 1:2) {
      expect_false(is.null(Rec('SingleA', fl)), label = paste('SingleA fleet', fl))
      expect_equal(Rec('LengthOne', fl), Rec('SingleA', fl))
    }
    expect_false(isTRUE(all.equal(Rec('SingleA', 1)$MeanAtAge, Rec('SingleB', 1)$MeanAtAge)))

    # list by position
    expect_equal(Rec('ListAB', 1), Rec('SingleA', 1))
    expect_equal(Rec('ListAB', 2), Rec('SingleB', 2))
    # list by fleet name, in reverse order
    expect_equal(Rec('Named', 1), Rec('SingleA', 1))
    expect_equal(Rec('Named', 2), Rec('SingleB', 2))
    # NULL leaves the fleet unchanged
    expect_equal(Rec('WithNULL', 1), Rec('SingleA', 1))
    expect_null(Rec('WithNULL', 2))
    expect_null(MSE@Misc[[sl]][['AllNULL']])

    if (form == 'OwnClasses') {
      AtLen <- Rec('SingleA', 1)$MeanAtLength
      expect_equal(as.numeric(dimnames(AtLen)$Class), .ArrCl)
      expect_false(anyNA(AtLen))
      Mapped <- A@MeanAtLength[pmax(findInterval(.ClassMidpoints(.ArrCl), .ArrOwnCl), 1)]
      expect_equal(as.numeric(apply(AtLen, 2, max)), Mapped)
      expect_equal(as.numeric(apply(AtLen, 2, min)), Mapped)
    }
    if (form == 'MeanAtAge') {
      Prescribed <- A@MeanAtAge
      AtAge <- Rec('SingleA', 1)$MeanAtAge
      expect_equal(as.numeric(apply(AtAge, 2, max)), Prescribed)
      expect_equal(as.numeric(apply(AtAge, 2, min)), Prescribed)
      # the at-length schedule follows the new at-age schedule
      expect_false(is.null(Rec('SingleA', 1)$MeanAtLength))
    }
  })
}

test_that("invalid Advice schedule arrangements are logged with a clear message", {
  Sel <- Selectivity(Pars = list(L5 = 30, LFS = 60))
  MPs <- list(
    TooLong   = .ArrMP('Selectivity', list(Sel, Sel, Sel)),
    WrongCls  = .ArrMP('Selectivity', list(Sel, Retention(Pars = list(RL50 = 60, RL50_95 = 10)))),
    BadNames  = .ArrMP('Selectivity', list(Fleet_A = Sel, Fleet_B = Sel)),
    BadLength = .ArrMP('DiscardMortality', DiscardMortality(MeanAtLength = c(0.5, 0.5)))
  )
  MSE <- .ArrProject(MPs)
  Expected <- c(TooLong   = 'It has length 3.',
                WrongCls  = 'Not a `Selectivity()` object: element 2.',
                BadNames  = 'must match the fleet names',
                BadLength = 'must be length `nClass`')
  for (mp in names(Expected)) {
    Msgs <- .ArrErrors(MSE, mp)
    expect_true(length(Msgs) > 0, label = mp)
    expect_true(all(grepl(Expected[[mp]], Msgs, fixed = TRUE)), label = mp)
  }
})

test_that("a list by fleet inside a schedule slot is rejected by the constructor", {
  Vals <- list(F1 = rep(0.5, 3), F2 = rep(0.2, 3))
  for (fn in list(Selectivity, Retention, DiscardMortality)) {
    expect_error(fn(MeanAtLength = Vals, Classes = 1:3), 'not a list')
    expect_error(fn(MeanAtAge = Vals), 'not a list')
  }
  expect_error(Selectivity(MeanAtWeight = Vals), 'not a list')

  ListMP <- function(Data) {
    A <- CurrentEffort(Data)
    A@DiscardMortality <- DiscardMortality(MeanAtLength = list(rep(0.5, 3), rep(0.2, 3)))
    A
  }
  class(ListMP) <- 'mp'
  MSE <- .ArrProject(list(ListMP = ListMP))
  expect_true(all(grepl('list of `DiscardMortality()` objects, one per fleet', .ArrErrors(MSE, 'ListMP'), fixed = TRUE)))
})

test_that("OM fleet at-length vectors without Classes use the stock's length classes", {
  for (nm in c('Selectivity', 'Retention', 'DiscardMortality')) {
    fn  <- get(nm)
    v   <- if (nm == 'DiscardMortality') ifelse(.ArrCl >= 60, 0.2, 0.6) else pmin(1, .ArrCl / 60)
    Get <- function(obj) {
      OM <- .ArrOM()
      Fl <- OM@Fleet[[1]][[1]]
      methods::slot(Fl, nm) <- obj
      OM@Fleet[[1]][[1]] <- Fl
      methods::slot(Simulate(OM, silent = TRUE)@OM@Fleet[[1]][[1]], nm)
    }
    NoCl <- Get(fn(MeanAtLength = v))
    WithCl <- Get(fn(MeanAtLength = v, Classes = .ArrCl))
    expect_equal(NoCl@MeanAtAge, WithCl@MeanAtAge, label = nm)
    expect_equal(NoCl@MeanAtLength, WithCl@MeanAtLength, label = nm)
  }
})

test_that("retention supplied only at age gives retained weight equal to selected weight", {
  OM <- .ArrOM()
  Fl <- OM@Fleet[[1]][[1]]
  Fl@Retention <- Retention(MeanAtAge = as.numeric(.ArrAg >= 3))
  OM@Fleet[[1]][[1]] <- Fl
  Fl <- Simulate(OM, silent = TRUE)@OM@Fleet[[1]][[1]]
  expect_false(Fl@Retention@isAtLength)
  expect_equal(Fl@WeightFleetRetained, Fl@WeightFleetSelected)
  expect_false(isFALSE(.ArrHist@OM@Fleet[[1]][[1]]@Retention@isAtLength))
})

test_that("schedules given by area ([nClass, nArea] matrices) are applied per area", {
  expect_equal(nArea(.ArrHist@OM), 2)
  MA <- cbind(pmin(1, .ArrAg / 4), pmin(1, .ArrAg / 8))
  ML <- cbind(pmin(1, .ArrCl / 60), pmin(1, .ArrCl / 90))
  DA <- cbind(ifelse(.ArrAg >= 3, 0.2, 0.6), ifelse(.ArrAg >= 6, 0.1, 0.9))
  MPs <- list(
    SelAge = .ArrMP('Selectivity', Selectivity(MeanAtAge = MA)),
    SelLen = .ArrMP('Selectivity', Selectivity(MeanAtLength = ML)),
    RetAge = .ArrMP('Retention', Retention(MeanAtAge = (MA >= 0.5) * 1)),
    RetLen = .ArrMP('Retention', Retention(MeanAtLength = (ML >= 0.5) * 1)),
    DMAge  = .ArrMP('DiscardMortality', DiscardMortality(MeanAtAge = DA)),
    DMLen  = .ArrMP('DiscardMortality', DiscardMortality(MeanAtLength = ML * 0.5)),
    BadDim = .ArrMP('Selectivity', Selectivity(MeanAtAge = cbind(MA, MA[, 1])))
  )
  MSE <- .ArrProject(MPs)
  for (mp in setdiff(names(MPs), 'BadDim'))
    expect_length(.ArrErrors(MSE, mp), 0)
  expect_true(all(grepl('incorrect dimensions', .ArrErrors(MSE, 'BadDim'))))

  AtAge <- \(sl, mp) .ArrRecorded(MSE, sl, mp, 1)$MeanAtAge
  for (ar in 1:2) {
    expect_equal(as.numeric(apply(AtAge('Selectivity', 'SelAge')[, , , ar, drop = FALSE], 2, range)),
                 rep(MA[, ar], each = 2))
    expect_equal(as.numeric(apply(AtAge('DiscardMortality', 'DMAge')[, , , ar, drop = FALSE], 2, range)),
                 rep(DA[, ar], each = 2))
  }
  for (sl_mp in list(c('Selectivity', 'SelLen'), c('Retention', 'RetLen'), c('DiscardMortality', 'DMLen'))) {
    A <- AtAge(sl_mp[1], sl_mp[2])
    expect_false(isTRUE(all.equal(A[, , , 1], A[, , , 2])), label = sl_mp[2])
  }
})

test_that("Selectivity, Retention, and DiscardMortality advised together are each applied", {
  Sel <- Selectivity(Pars = list(L5 = 30, LFS = 60))
  Ret <- Retention(Pars = list(RL50 = 70, RL50_95 = 10))
  DM  <- DiscardMortality(MeanAtAge = ifelse(.ArrAg >= 3, 0.2, 0.6))
  All <- function(Data) {
    A <- CurrentEffort(Data)
    A@Selectivity <- Sel
    A@Retention <- list(Ret, NULL)
    A@DiscardMortality <- DM
    A
  }
  class(All) <- 'mp'
  MSE <- .ArrProject(list(All = All, Sel = .ArrMP('Selectivity', Sel), DM = .ArrMP('DiscardMortality', DM)))
  expect_length(MSE@Log$error, 0)
  expect_true(all(Discards(MSE)$Value >= -1e-8, na.rm = TRUE))
  for (fl in 1:2) {
    expect_equal(.ArrRecorded(MSE, 'Selectivity', 'All', fl), .ArrRecorded(MSE, 'Selectivity', 'Sel', fl))
    expect_equal(.ArrRecorded(MSE, 'DiscardMortality', 'All', fl), .ArrRecorded(MSE, 'DiscardMortality', 'DM', fl))
  }
  expect_false(is.null(.ArrRecorded(MSE, 'Retention', 'All', 1)))
  expect_null(.ArrRecorded(MSE, 'Retention', 'All', 2))
})

test_that("schedules that change between management years are applied in each year", {
  OM <- .ArrOM()
  pYear(OM) <- 3
  Hist <- Simulate(OM, silent = TRUE)
  AgeAt <- \(y) 3 + (y %% 2) * 3
  TimeVary <- function(Data) {
    A <- CurrentEffort(Data)
    A@Retention <- Retention(MeanAtAge = as.numeric(.ArrAg >= AgeAt(floor(max(Data@Years)) + 1)))
    A
  }
  class(TimeVary) <- 'mp'
  MSE <- suppressMessages(Project(Hist, MPs = list(TimeVary = TimeVary), parallel = FALSE, silent = TRUE))
  expect_length(MSE@Log$error, 0)
  AtAge <- .ArrRecorded(MSE, 'Retention', 'TimeVary', 1)$MeanAtAge
  Yrs <- as.numeric(dimnames(AtAge)$Year)
  expect_length(Yrs, 3)
  for (i in seq_along(Yrs))
    expect_equal(as.numeric(AtAge[1, , i, 1]), as.numeric(.ArrAg >= AgeAt(Yrs[i])), label = Yrs[i])
})

test_that("schedules given at weight are applied, and at-weight retention gives consistent fleet weights", {
  OM <- .ArrOM()
  W <- OM@Stock[[1]]@Weight
  W@CVatAge <- 0.1
  OM@Stock[[1]]@Weight <- W
  Hist <- Simulate(OM, silent = TRUE)
  WC <- Hist@OM@Stock[[1]]@Weight@Classes
  expect_gt(length(WC), 0)
  MPs <- list(SelW = .ArrMP('Selectivity', Selectivity(MeanAtWeight = pmin(1, WC / 5))),
              RetW = .ArrMP('Retention', list(Retention(MeanAtWeight = as.numeric(WC >= 5)), NULL)))
  MSE <- suppressMessages(Project(Hist, MPs = MPs, parallel = FALSE, silent = TRUE))
  expect_length(MSE@Log$error, 0)
  expect_true(all(Discards(MSE)$Value >= -1e-8, na.rm = TRUE))
  expect_false(is.null(.ArrRecorded(MSE, 'Selectivity', 'SelW', 2)))
  expect_false(is.null(.ArrRecorded(MSE, 'Retention', 'RetW', 1)))
  expect_null(.ArrRecorded(MSE, 'Retention', 'RetW', 2))

  Fl <- OM@Fleet[[1]][[1]]
  Fl@Retention <- Retention(MeanAtWeight = as.numeric(WC >= 5), Classes = WC)
  OM@Fleet[[1]][[1]] <- Fl
  Fl <- Simulate(OM, silent = TRUE)@OM@Fleet[[1]][[1]]
  expect_false(Fl@Retention@isAtLength)
  expect_equal(Fl@WeightFleetRetained, Fl@WeightFleetSelected)
})

test_that("multi-stock OMs apply each stock's advice to that stock only (mp and mmp)", {
  OM <- MultiStockOM
  nSim(OM) <- 2
  pYear(OM) <- 2
  Hist <- Simulate(OM, silent = TRUE)
  Stocks <- names(Hist@OM@Complexes)
  expect_length(Stocks, 2)
  nAge2 <- length(Hist@OM@Stock[[2]]@Ages@Classes)
  Sel <- Selectivity(Pars = list(L5 = 10, LFS = 20))
  DM2 <- DiscardMortality(MeanAtAge = rep(0.4, nAge2))
  PerStock <- function(DataList) {
    out <- lapply(DataList, CurrentEffort)
    out[[Stocks[1]]]@Selectivity <- list(Sel, NULL)
    out[[Stocks[2]]]@DiscardMortality <- list(NULL, DM2)
    out
  }
  class(PerStock) <- 'mmp'
  MSE <- suppressMessages(Project(Hist, MPs = list(PerStock = PerStock, Same = .ArrMP('Selectivity', Sel)),
                                  parallel = FALSE, silent = TRUE))
  expect_length(MSE@Log$error, 0)
  Rec <- \(sl, mp, st, fl) MSE@Misc[[sl]][[mp]][[st]][[fl]]
  expect_false(is.null(Rec('Selectivity', 'PerStock', 1, 1)))
  expect_null(Rec('Selectivity', 'PerStock', 1, 2))
  expect_null(Rec('Selectivity', 'PerStock', 2, 1))
  expect_null(Rec('Selectivity', 'PerStock', 2, 2))
  expect_null(Rec('DiscardMortality', 'PerStock', 1, 1))
  expect_null(Rec('DiscardMortality', 'PerStock', 1, 2))
  expect_null(Rec('DiscardMortality', 'PerStock', 2, 1))
  expect_equal(as.numeric(apply(Rec('DiscardMortality', 'PerStock', 2, 2)$MeanAtAge, 2, max)), rep(0.4, nAge2))
  for (st in 1:2) for (fl in 1:2)
    expect_false(is.null(Rec('Selectivity', 'Same', st, fl)), label = paste(st, fl))
})

test_that("advice for a stock complex is applied to every stock in the complex", {
  OM <- HermOM
  nSim(OM) <- 2
  pYear(OM) <- 2
  Hist <- Simulate(OM, silent = TRUE)
  expect_length(Hist@OM@Complexes, 1)
  expect_length(Hist@OM@Complexes[[1]], 2)
  Ag <- Hist@OM@Stock[[1]]@Ages@Classes
  Ret <- Retention(MeanAtAge = as.numeric(Ag >= 5))
  MPs <- list(SelPars = .ArrMP('Selectivity', Selectivity(Pars = list(L5 = 30, LFS = 60))),
              RetAge  = .ArrMP('Retention', list(Ret, NULL)))
  MSE <- suppressMessages(Project(Hist, MPs = MPs, parallel = FALSE, silent = TRUE))
  expect_length(MSE@Log$error, 0)
  Rec <- \(sl, mp, st, fl) MSE@Misc[[sl]][[mp]][[st]][[fl]]
  for (st in 1:2) {
    for (fl in 1:2) expect_false(is.null(Rec('Selectivity', 'SelPars', st, fl)))
    expect_equal(as.numeric(apply(Rec('Retention', 'RetAge', st, 1)$MeanAtAge, 2, max)), as.numeric(Ag >= 5))
    expect_null(Rec('Retention', 'RetAge', st, 2))
  }
})

test_that("schedules are applied in every season of a seasonal, three-area OM", {
  OM <- SeasonalSpatialOM
  nSim(OM) <- 2
  pYear(OM) <- 2
  Hist <- Simulate(OM, silent = TRUE)
  expect_equal(nArea(Hist@OM), 3)
  Ag <- Hist@OM@Stock[[1]]@Ages@Classes
  Ret <- as.numeric(Ag >= 2)
  MPs <- list(SelPars = .ArrMP('Selectivity', Selectivity(Pars = list(L5 = 10, LFS = 20))),
              RetAge  = .ArrMP('Retention', Retention(MeanAtAge = Ret)),
              DMLen   = .ArrMP('DiscardMortality', DiscardMortality(MeanAtLength = 0.5)))
  MSE <- suppressMessages(Project(Hist, MPs = MPs, parallel = FALSE, silent = TRUE))
  expect_length(MSE@Log$error, 0)
  expect_true(all(Discards(MSE)$Value >= -1e-8, na.rm = TRUE))
  AtAge <- MSE@Misc$Retention$RetAge[[1]][[1]]$MeanAtAge
  expect_gt(dim(AtAge)[3], 2)
  expect_equal(as.numeric(apply(AtAge, 2, max)), Ret)
  expect_equal(as.numeric(apply(AtAge, 2, min)), Ret)
  expect_false(is.null(MSE@Misc$Selectivity$SelPars[[1]][[1]]))
  expect_true(all(abs(MSE@Misc$DiscardMortality$DMLen[[1]][[1]]$MeanAtAge - 0.5) < 1e-8))
})

test_that("advice on other size classes keeps every projected array on the fleet's classes", {
  OM <- .ArrOM()
  pYear(OM) <- 3
  Hist <- Simulate(OM, silent = TRUE, control = SimControl(CalcCatchAtSize = TRUE))
  Hist@OM@Control$CalcCatchAtSizeNeeded <- TRUE
  MPs <- list(
    SelLen  = .ArrMP('Selectivity', Selectivity(MeanAtLength = pmin(1, .ArrOwnCl / 60), Classes = .ArrOwnCl)),
    RetLen  = .ArrMP('Retention', list(NULL, Retention(MeanAtLength = as.numeric(.ArrOwnCl >= 60), Classes = .ArrOwnCl))),
    DMLen   = .ArrMP('DiscardMortality', DiscardMortality(MeanAtLength = ifelse(.ArrOwnCl >= 60, 0.2, 0.6), Classes = .ArrOwnCl)),
    SelPars = .ArrMP('Selectivity', Selectivity(Pars = list(L5 = 30, LFS = 60), Classes = .ArrOwnCl)),
    Ref     = .ArrMP('Selectivity', Selectivity(Pars = list(L5 = 30, LFS = 60)))
  )
  MSE <- suppressMessages(Project(Hist, MPs = MPs, parallel = FALSE, silent = TRUE))
  expect_length(MSE@Log$error, 0)
  for (mp in names(MPs)) for (fl in 1:2) {
    LAS <- MSE@LandingsAtSize[[1]][[fl]]
    expect_equal(as.numeric(dimnames(LAS)$Class), .ArrCl, label = paste(mp, fl))
  }
  expect_false(anyNA(MSE@LandingsAtSize[[1]][[1]]))
  for (sl_mp in list(c('Selectivity', 'SelLen'), c('DiscardMortality', 'DMLen'), c('Selectivity', 'SelPars')))
    expect_equal(as.numeric(dimnames(.ArrRecorded(MSE, sl_mp[1], sl_mp[2], 1)$MeanAtLength)$Class), .ArrCl)
  expect_equal(as.numeric(dimnames(.ArrRecorded(MSE, 'Retention', 'RetLen', 2)$MeanAtLength)$Class), .ArrCl)
  # a model is evaluated on the fleet's classes whatever `Classes` the advice gives
  expect_equal(.ArrRecorded(MSE, 'Selectivity', 'SelPars', 1), .ArrRecorded(MSE, 'Selectivity', 'Ref', 1))
})

test_that("advice is mapped onto a fleet's own size classes", {
  OM <- .ArrOM()
  FleetCl <- seq(0, max(.ArrCl), by = 5)
  Fl <- OM@Fleet[[1]][[1]]
  Fl@Selectivity <- Selectivity(MeanAtLength = pmin(1, FleetCl / 50), Classes = FleetCl)
  OM@Fleet[[1]][[1]] <- Fl
  Hist <- Simulate(OM, silent = TRUE)
  expect_equal(as.numeric(dimnames(Hist@OM@Fleet[[1]][[1]]@Selectivity@MeanAtLength)$Class), FleetCl)
  Adv <- pmin(1, .ArrCl / 70)
  MSE <- suppressMessages(Project(Hist, MPs = list(StockCl = .ArrMP('Selectivity', Selectivity(MeanAtLength = Adv, Classes = .ArrCl))),
                                  parallel = FALSE, silent = TRUE))
  expect_length(MSE@Log$error, 0)
  AtLen <- .ArrRecorded(MSE, 'Selectivity', 'StockCl', 1)$MeanAtLength
  expect_equal(as.numeric(dimnames(AtLen)$Class), FleetCl)
  expect_equal(as.numeric(apply(AtLen, 2, max)), Adv[pmax(findInterval(.ClassMidpoints(FleetCl), .ArrCl), 1)])
  expect_equal(as.numeric(dimnames(.ArrRecorded(MSE, 'Selectivity', 'StockCl', 2)$MeanAtLength)$Class), .ArrCl)
})

test_that("at-weight advice on other weight classes is mapped onto the fleet's weight classes", {
  OM <- .ArrOM()
  W <- OM@Stock[[1]]@Weight
  W@CVatAge <- 0.1
  OM@Stock[[1]]@Weight <- W
  Hist <- Simulate(OM, silent = TRUE)
  WC <- Hist@OM@Stock[[1]]@Weight@Classes
  OwnW <- seq(0, max(WC), length.out = 10)
  Vals <- pmin(1, OwnW / 5)
  MSE <- suppressMessages(Project(Hist, MPs = list(SelW = .ArrMP('Selectivity', Selectivity(MeanAtWeight = Vals, Classes = OwnW))),
                                  parallel = FALSE, silent = TRUE))
  expect_length(MSE@Log$error, 0)
  Rec <- .ArrRecorded(MSE, 'Selectivity', 'SelW', 1)
  expect_false(is.null(Rec$MeanAtAge))
  # the at-length schedule follows the new at-weight schedule
  expect_false(is.null(Rec$MeanAtLength))
  expect_equal(as.numeric(dimnames(Rec$MeanAtLength)$Class), .ArrCl)
  expect_false(anyNA(Rec$MeanAtLength))
})
