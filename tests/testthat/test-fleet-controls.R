# Per fleet, an NA TAC or effort is not set: a TAC fleet fishes to its TAC, an effort
# fleet fishes its effort, a fleet with both stops at whichever binds first, and a fleet
# with neither keeps last year's effort. Implementation error applies throughout.

.ControlMP <- function(TAC = NULL, Effort = NULL, EffType = 'Rel') {
  mp <- eval(bquote(function(Data) {
    a <- Advice()
    if (!is.null(.(TAC)))    a@TAC <- .(TAC)
    if (!is.null(.(Effort))) { a@Effort <- .(Effort); a@EffType <- .(EffType) }
    a
  }))
  class(mp) <- 'mp'
  mp
}

.ImpError <- function(hist, value) {
  AllY <- Years(hist@OM)
  array(value, c(nSim(hist), length(AllY)),
        dimnames = list(Sim = seq_len(nSim(hist)), Year = as.character(AllY)))
}

test_that("each fleet follows its own TAC and effort controls in a single-complex OM", {
  skip_on_cran()
  om <- TwoFleetOM
  nSim(om) <- 2
  pYear(om) <- 3
  Hist <- Simulate(om, silent = TRUE)
  nHist <- dim(Hist@Effort)[2]
  LastEff <- ExtendSims(Hist@Effort, 2)[, nHist, ]
  LastC   <- apply(ArraySum(Hist@Landings, Hist@Discards)[, 1, nHist, , drop = FALSE], c(1, 4), sum)
  T1 <- 0.5 * mean(LastC[, 1]); T2 <- 0.5 * mean(LastC[, 2])

  Hist@OM@Imp[[1]][[1]]@TAC@Error    <- .ImpError(Hist, 1.2)
  Hist@OM@Imp[[1]][[2]]@Effort@Error <- .ImpError(Hist, 0.8)

  MPs <- list(
    TACOnly   = .ControlMP(TAC = c(T1, NA)),
    TACEffort = .ControlMP(TAC = c(T1, NA), Effort = c(NA, 1.5)),
    Both      = .ControlMP(TAC = c(T1, T2), Effort = c(0.2, NA)),
    AreaEff   = .ControlMP(TAC = c(T1, NA), Effort = rbind(c(NA, NA), c(0.5, NA)), EffType = 'Abs')
  )
  MSE <- Project(Hist, MPs = MPs, parallel = FALSE, silent = TRUE)
  expect_length(MSE@Log$error, 0)

  df  <- TACAttainment(MSE)
  Fl  <- FleetNames(Hist)
  Eff <- ExtendSims(MSE@Effort, 2)
  PY  <- as.character(Years(Hist@OM, 'Projection'))

  # TAC fleet catches its TAC, with implementation error, whatever the other fleet does
  for (mp in c('TACOnly', 'TACEffort', 'AreaEff')) {
    F1 <- df[df$MP == mp & df$Fleet == Fl[1], ]
    expect_equal(F1$TAC, rep(1.2 * T1, nrow(F1)), tolerance = 1e-8, label = mp)
    expect_equal(F1$Catch, F1$TAC, tolerance = 1e-4, label = mp)
    expect_false(any(df$MP == mp & df$Fleet == Fl[2] & !is.na(df$TAC)))
  }

  # neither control: last year's effort
  expect_equal(unname(Eff[, PY, 2, 'TACOnly']), matrix(LastEff[, 2], 2, length(PY)),
               tolerance = 1e-6, ignore_attr = TRUE)

  # effort control only: advised effort, with implementation error
  expect_equal(unname(Eff[, PY, 2, 'TACEffort']), matrix(1.5 * 0.8 * LastEff[, 2], 2, length(PY)),
               tolerance = 1e-6, ignore_attr = TRUE)

  # both: the effort cap binds before the TAC; the other fleet still catches its TAC
  B1 <- df[df$MP == 'Both' & df$Fleet == Fl[1], ]
  expect_true(all(B1$Catch < B1$TAC))
  expect_true(all(B1$Limit == 'Ceiling'))
  expect_equal(unname(Eff[, PY, 1, 'Both']), matrix(0.2 * LastEff[, 1], 2, length(PY)),
               tolerance = 1e-6, ignore_attr = TRUE)
  B2 <- df[df$MP == 'Both' & df$Fleet == Fl[2], ]
  expect_equal(B2$Catch, B2$TAC, tolerance = 1e-4)

  # fleet x area effort: NA cells are not set
  expect_equal(unname(Eff[, PY, 2, 'AreaEff']), matrix(0.5 * 0.8, 2, length(PY)),
               tolerance = 1e-6, ignore_attr = TRUE)
  Dist <- ExtendSims(MSE@Distribution, 2)[, PY, 2, , 'AreaEff']
  expect_equal(range(Dist[, , 1]), c(1, 1))
})

test_that("a fleet with no TAC in one complex is not limited by that complex", {
  skip_on_cran()
  om <- MultiStockOM
  nSim(om) <- 2
  pYear(om) <- 3
  set.seed(1)
  Hist <- Simulate(om, silent = TRUE)
  nHist <- dim(Hist@Effort)[2]
  C <- ArraySum(Hist@Landings, Hist@Discards)
  assign('.CtlC1', apply(C[, 1, nHist, , drop = FALSE], c(1, 4), sum), globalenv())
  assign('.CtlC2', apply(C[, 2, nHist, , drop = FALSE], c(1, 4), sum), globalenv())
  on.exit(rm('.CtlC1', '.CtlC2', envir = globalenv()), add = TRUE)

  # fleet 1 is managed only by complex 1, fleet 2 only by complex 2
  Split <- function(Data) {
    s <- Data@Misc$Sim
    if (grepl('Albacore', Data@Name)) Advice(TAC = c(0.5 * .CtlC1[s, 1], NA))
    else Advice(TAC = c(NA, 1.3 * .CtlC2[s, 2]))
  }
  class(Split) <- 'mp'
  MSE <- Project(Hist, MPs = list(Split = Split), parallel = FALSE, silent = TRUE)
  df <- TACAttainment(MSE)
  Set <- df[!is.na(df$TAC), ]
  expect_equal(nrow(Set), 2 * 3 * 2)
  expect_equal(Set$Catch, Set$TAC, tolerance = 1e-3)
  expect_true(all(Set$Limit == 'None'))
})

test_that("a complex with empty advice beside a TAC complex keeps the TAC solve working", {
  skip_on_cran()
  om <- MultiStockOM
  nSim(om) <- 2
  pYear(om) <- 2
  Hist  <- Simulate(om, silent = TRUE)
  nHist <- dim(Hist@Effort)[2]
  First <- names(Hist@OM@Complexes)[1]
  TAC1  <- 0.5 * apply(Hist@Landings[, Hist@OM@Complexes[[1]], nHist, , drop = FALSE], 4, sum) / 2
  MP <- function(Data)
    if (identical(Data@Misc$StockName, First)) Advice(TAC = TAC1) else Advice()
  class(MP) <- 'mp'
  MSE <- suppressMessages(Project(Hist, MPs = list(A = MP), parallel = FALSE, silent = TRUE))
  expect_length(MSE@Log$error, 0)
  expect_false(anyNA(MSE@Effort))
  df <- TACAttainment(MSE)
  expect_true(all(is.na(df$TAC[df$Stock != First])))
  Set <- df[!is.na(df$TAC) & df$Limit == 'None', ]
  expect_gt(nrow(Set), 0)
  expect_equal(Set$Catch, Set$TAC, tolerance = 1e-4)
})

test_that("effort compliance does not rescale an effort that no other complex competes with", {
  skip_on_cran()
  for (k in c(0.7, 0.3)) {
    om <- MultiStockOM
    nSim(om) <- 2
    pYear(om) <- 2
    for (cx in seq_along(om@Imp))
      for (fl in seq_along(om@Imp[[cx]]))
        om@Imp[[cx]][[fl]]@Effort@Compliance <- k
    Hist  <- Simulate(om, silent = TRUE)
    First <- names(Hist@OM@Complexes)[1]
    MP <- function(Data) {
      if (identical(Data@Misc$StockName, First))
        Advice(Effort = 0.8, EffType = 'Rel')
      else
        Advice(Effort = NA_real_, EffType = 'Rel')
    }
    class(MP) <- 'mp'
    MSE  <- suppressMessages(Project(Hist, MPs = list(A = MP), parallel = FALSE, silent = TRUE))
    Last <- Hist@Effort[1, dim(Hist@Effort)[2], ]
    expect_equal(unname(MSE@Effort[1, 1, , 1]), unname(0.8 * Last), tolerance = 1e-8, label = paste('Compliance', k))
  }
})

.FirstThenMP <- function(First, Later) {
  mp <- eval(bquote(function(Data) if (isTRUE(Data@Misc$FirstAdvice)) .(First) else .(Later)))
  class(mp) <- 'mp'
  mp
}

test_that("an advised effort stays in place for a fleet until a TAC or new effort replaces it", {
  skip_on_cran()
  om <- TwoFleetOM
  nSim(om) <- 2
  pYear(om) <- 3
  Hist  <- Simulate(om, silent = TRUE)
  nHist <- dim(Hist@Effort)[2]
  LastEff <- ExtendSims(Hist@Effort, 2)[, nHist, ]
  LastC   <- apply(ArraySum(Hist@Landings, Hist@Discards)[, 1, nHist, , drop = FALSE], 4, sum) / 2
  T1   <- 0.3 * LastC
  Area <- rbind(c(0.9, 0.1), c(0.6, 0.4))

  MPs <- list(
    EffEmpty     = .FirstThenMP(quote(Advice(Effort = 0.5)), quote(Advice())),
    AreaEmpty    = .FirstThenMP(bquote(Advice(Effort = .(Area), EffType = 'Abs')), quote(Advice())),
    AreaEvery    = .FirstThenMP(bquote(Advice(Effort = .(Area), EffType = 'Abs')),
                                bquote(Advice(Effort = .(Area), EffType = 'Abs'))),
    TACNA        = .FirstThenMP(bquote(Advice(TAC = .(T1))), quote(Advice(TAC = c(NA_real_, NA_real_)))),
    TACFleet1    = .FirstThenMP(bquote(Advice(TAC = .(T1))), bquote(Advice(TAC = c(.(T1[1]), NA)))),
    EffTACFleet1 = .FirstThenMP(quote(Advice(Effort = 0.5)), bquote(Advice(TAC = c(.(T1[1]), NA)))),
    EffFleet2    = .FirstThenMP(quote(Advice(Effort = 0.5)), quote(Advice(Effort = c(NA, 0.7)))),
    TACEff       = .FirstThenMP(bquote(Advice(TAC = .(T1))), quote(Advice(Effort = 0.6)))
  )
  MSE <- suppressMessages(Project(Hist, MPs = MPs, parallel = FALSE, silent = TRUE))
  expect_length(MSE@Log$error, 0)

  Eff   <- ExtendSims(MSE@Effort, 2)
  PY    <- as.character(Years(Hist@OM, 'Projection'))
  Later <- PY[-1]
  Want  <- \(x) matrix(x, 2, length(Later))
  Got   <- \(mp, fl) unname(Eff[, Later, fl, mp])

  expect_equal(Got('EffEmpty', 1), Want(0.5 * LastEff[, 1]), tolerance = 1e-8)
  expect_equal(Got('EffEmpty', 2), Want(0.5 * LastEff[, 2]), tolerance = 1e-8)
  for (fl in 1:2) {
    expect_equal(Got('TACNA', fl), Want(LastEff[, fl]), tolerance = 1e-8)
    expect_equal(Got('TACEff', fl), Want(0.6 * LastEff[, fl]), tolerance = 1e-8)
  }
  expect_equal(Got('TACFleet1', 2), Want(LastEff[, 2]), tolerance = 1e-8)
  expect_equal(Got('EffTACFleet1', 2), Want(0.5 * LastEff[, 2]), tolerance = 1e-8)
  expect_equal(Got('EffFleet2', 1), Want(0.5 * LastEff[, 1]), tolerance = 1e-8)
  expect_equal(Got('EffFleet2', 2), Want(0.7 * LastEff[, 2]), tolerance = 1e-8)

  # an effort split by area keeps its split
  expect_equal(Eff[, , , 'AreaEmpty'], Eff[, , , 'AreaEvery'], tolerance = 1e-10)
  Dist <- ExtendSims(MSE@Distribution, 2)
  expect_equal(Dist[, , , , 'AreaEmpty'], Dist[, , , , 'AreaEvery'], tolerance = 1e-10)
  expect_equal(unname(Dist[1, Later, 1, , 'AreaEmpty']), matrix(c(0.9, 0.1), length(Later), 2, byrow = TRUE),
               tolerance = 1e-10)

  Att   <- TACAttainment(MSE)
  Fl    <- FleetNames(Hist)
  After <- Att$Year %in% Later & !is.na(Att$TAC)
  expect_false(any(After & Att$MP %in% c('TACNA', 'TACEff')))
  expect_false(any(After & Att$MP == 'TACFleet1' & Att$Fleet == Fl[2]))
  expect_true(any(After & Att$MP == 'EffTACFleet1' & Att$Fleet == Fl[1]))
})

test_that("an advised effort stays in place across years without MP advice when Interval > 1", {
  skip_on_cran()
  om <- TwoFleetOM
  nSim(om) <- 2
  pYear(om) <- 5
  Hist  <- Simulate(om, silent = TRUE)
  Hist@OM@Interval <- 2
  nHist <- dim(Hist@Effort)[2]
  LastEff <- ExtendSims(Hist@Effort, 2)[, nHist, ]
  LastC   <- apply(ArraySum(Hist@Landings, Hist@Discards)[, 1, nHist, , drop = FALSE], 4, sum) / 2
  T1 <- 0.3 * LastC

  MPs <- list(
    EffEmpty  = .FirstThenMP(quote(Advice(Effort = 0.5)), quote(Advice())),
    EffFleet2 = .FirstThenMP(quote(Advice(Effort = 0.5)), quote(Advice(Effort = c(NA, 0.7)))),
    TACEff    = .FirstThenMP(bquote(Advice(TAC = .(T1))), quote(Advice(Effort = 0.6))),
    TACNA     = .FirstThenMP(bquote(Advice(TAC = .(T1))), quote(Advice(TAC = c(NA_real_, NA_real_))))
  )
  MSE <- suppressMessages(Project(Hist, MPs = MPs, parallel = FALSE, silent = TRUE))
  expect_length(MSE@Log$error, 0)

  Eff  <- ExtendSims(MSE@Effort, 2)
  PY   <- as.character(Years(Hist@OM, 'Projection'))
  Gap  <- PY[2]
  Next <- PY[-(1:2)]
  Want <- \(x, yrs) matrix(x, 2, length(yrs))
  Got  <- \(mp, fl, yrs) matrix(unname(Eff[, yrs, fl, mp]), 2, length(yrs))

  for (fl in 1:2)
    expect_equal(Got('EffEmpty', fl, PY), Want(0.5 * LastEff[, fl], PY), tolerance = 1e-8)
  for (fl in 1:2)
    expect_equal(Got('EffFleet2', fl, Gap), Want(0.5 * LastEff[, fl], Gap), tolerance = 1e-8)
  expect_equal(Got('EffFleet2', 1, Next), Want(0.5 * LastEff[, 1], Next), tolerance = 1e-8)
  expect_equal(Got('EffFleet2', 2, Next), Want(0.7 * LastEff[, 2], Next), tolerance = 1e-8)
  for (fl in 1:2) {
    expect_equal(Got('TACEff', fl, Next), Want(0.6 * LastEff[, fl], Next), tolerance = 1e-8)
    expect_equal(Got('TACNA', fl, Next), Want(LastEff[, fl], Next), tolerance = 1e-8)
  }

  Att <- TACAttainment(MSE)
  Att <- Att[!is.na(Att$TAC), ]
  expect_true(all(c('TACEff', 'TACNA') %in% Att$MP[Att$Year == Gap]))
  expect_false(any(Att$MP %in% c('TACEff', 'TACNA') & Att$Year %in% Next))
})

test_that("a repeated TAC or effort gets each year's implementation error", {
  skip_on_cran()
  om <- TwoFleetOM
  nSim(om) <- 2
  pYear(om) <- 3
  Hist  <- Simulate(om, silent = TRUE)
  nHist <- dim(Hist@Effort)[2]
  LastC <- apply(ArraySum(Hist@Landings, Hist@Discards)[, 1, nHist, , drop = FALSE], 4, sum) / 2
  T1    <- 0.3 * LastC
  Err   <- .ImpError(Hist, 1)
  PY    <- as.character(Years(Hist@OM, 'Projection'))
  Err[, PY] <- rep(c(0.9, 1.1, 1.2), each = 2)
  for (fl in 1:2) {
    Hist@OM@Imp[[1]][[fl]]@TAC@Error    <- Err
    Hist@OM@Imp[[1]][[fl]]@Effort@Error <- Err
  }
  Once  <- list(TAC = .FirstThenMP(bquote(Advice(TAC = .(T1))), quote(Advice())),
                Eff = .FirstThenMP(quote(Advice(Effort = 0.5)), quote(Advice())))
  Every <- list(TACEvery = .FirstThenMP(bquote(Advice(TAC = .(T1))), bquote(Advice(TAC = .(T1)))),
                EffEvery = .FirstThenMP(quote(Advice(Effort = 0.5)), quote(Advice(Effort = 0.5))))
  MSE <- suppressMessages(Project(Hist, MPs = c(Once, Every), parallel = FALSE, silent = TRUE))
  expect_length(MSE@Log$error, 0)
  expect_equal(MSE@Effort[, , , 'TAC'], MSE@Effort[, , , 'TACEvery'], tolerance = 1e-8)
  expect_equal(MSE@Effort[, , , 'Eff'], MSE@Effort[, , , 'EffEvery'], tolerance = 1e-8)
  Att <- TACAttainment(MSE)
  expect_equal(Att$TAC[Att$MP == 'TAC'], Att$TAC[Att$MP == 'TACEvery'], tolerance = 1e-8)
  A <- Att[Att$MP == 'TAC', ]
  expect_equal(sort(unique(round(A$TAC / T1[match(A$Fleet, FleetNames(Hist))], 6))), c(0.9, 1.1, 1.2))
})

test_that("in a seasonal OM a fleet with no control fishes the same season of the last historical year", {
  skip_on_cran()
  om <- SeasonalSpatialOM
  nSim(om) <- 2
  pYear(om) <- 2
  Hist  <- Simulate(om, silent = TRUE)
  nSeas <- Hist@OM@Seasons
  nHist <- dim(Hist@Effort)[2]
  nFl   <- length(FleetNames(Hist))
  HistE <- ExtendSims(Hist@Effort, 2)[, nHist - nSeas + seq_len(nSeas), , drop = FALSE]
  LastC <- apply(ArraySum(Hist@Landings, Hist@Discards)[, , nHist - nSeas + seq_len(nSeas), , drop = FALSE], 4, sum) / 2
  MP <- .FirstThenMP(bquote(Advice(TAC = .(0.3 * LastC))), bquote(Advice(TAC = .(rep(NA_real_, nFl)))))
  MSE <- suppressMessages(Project(Hist, MPs = list(A = MP), parallel = FALSE, silent = TRUE))
  expect_length(MSE@Log$error, 0)
  Eff <- ExtendSims(MSE@Effort, 2)
  Y2  <- dim(Eff)[2] - nSeas + seq_len(nSeas)
  expect_equal(c(Eff[, Y2, , 'A', drop = FALSE]), c(HistE), tolerance = 1e-8)
})

test_that("empty advice after interim TACs is not a repeat of the interim TAC", {
  skip_on_cran()
  om <- TwoFleetOM
  nSim(om) <- 2
  pYear(om) <- 4
  Hist  <- Simulate(om, silent = TRUE)
  nHist <- dim(Hist@Effort)[2]
  LastEff <- ExtendSims(Hist@Effort, 2)[, nHist, ]
  PY <- Years(Hist@OM, 'Projection')
  Hist@OM@MPStartYear   <- PY[3]
  Hist@OM@InterimAdvice <- data.frame(Year = PY[1:2], Type = 'TAC', Mean = 50)
  MSE <- suppressMessages(Project(Hist, MPs = list(A = CurrentEffort), parallel = FALSE, silent = TRUE))
  expect_length(MSE@Log$error, 0)
  Att <- TACAttainment(MSE)
  expect_true(all(PY[1:2] %in% Att$Year[!is.na(Att$TAC)]))
  expect_false(any(Att$Year %in% PY[3:4] & !is.na(Att$TAC)))
  Eff <- ExtendSims(MSE@Effort, 2)
  for (fl in 1:2)
    expect_equal(unname(Eff[, as.character(PY[3:4]), fl, 1]), matrix(LastEff[, fl], 2, 2), tolerance = 1e-8)
})

test_that("empty advice after an interim effort is not a repeat of the interim effort", {
  skip_on_cran()
  om <- TwoFleetOM
  nSim(om) <- 2
  pYear(om) <- 4
  Hist  <- Simulate(om, silent = TRUE)
  nHist <- dim(Hist@Effort)[2]
  LastEff <- ExtendSims(Hist@Effort, 2)[, nHist, ]
  PY <- Years(Hist@OM, 'Projection')
  Hist@OM@MPStartYear   <- PY[3]
  Hist@OM@InterimAdvice <- data.frame(Year = PY[1:2], Type = 'Effort', Mean = 0.5, EffType = 'Rel')
  Empty <- function(Data) Advice()
  class(Empty) <- 'mp'
  MSE <- suppressMessages(Project(Hist, MPs = list(A = Empty), parallel = FALSE, silent = TRUE))
  expect_length(MSE@Log$error, 0)
  Eff <- ExtendSims(MSE@Effort, 2)
  for (fl in 1:2) {
    expect_equal(unname(Eff[, as.character(PY[1:2]), fl, 1]), matrix(0.5 * LastEff[, fl], 2, 2), tolerance = 1e-8)
    expect_equal(unname(Eff[, as.character(PY[3:4]), fl, 1]), matrix(LastEff[, fl], 2, 2), tolerance = 1e-8)
  }
})

test_that("effort compliance blends the efforts advised by several complexes, per sim and year", {
  skip_on_cran()
  om <- MultiStockOM
  nSim(om) <- 2
  pYear(om) <- 3
  Hist  <- Simulate(om, silent = TRUE)
  nHist <- dim(Hist@Effort)[2]
  LastE <- ExtendSims(Hist@Effort, 2)[, nHist, ]
  Cx    <- names(Hist@OM@Complexes)
  E1 <- c(0.4, 0.9)
  E2 <- c(0.8, 0.5)
  MP <- eval(bquote(function(Data)
    if (Data@Misc$StockName == .(Cx[1])) Advice(Effort = .(E1)) else Advice(Effort = .(E2))))
  class(MP) <- 'mp'
  Blend <- \(E, k) min(k * E + (1 - k) * max(E))
  Yrs <- as.character(Years(Hist@OM))
  PY  <- as.character(Years(Hist@OM, 'Projection'))
  K1  <- array(1, c(2, length(Yrs)), dimnames = list(Sim = 1:2, Year = Yrs))
  K1[2, ]     <- 0.5
  K1[1, PY[2]] <- 0
  K2 <- K1
  K2[] <- 0.2
  for (fl in 1:2) {
    Hist@OM@Imp[[1]][[fl]]@Effort@Compliance <- K1
    Hist@OM@Imp[[2]][[fl]]@Effort@Compliance <- K2
  }
  MSE <- suppressMessages(Project(Hist, MPs = list(A = MP), parallel = FALSE, silent = TRUE))
  Eff <- ExtendSims(MSE@Effort, 2)
  for (s in 1:2) for (y in PY) for (fl in 1:2) {
    Want <- Blend(c(E1[fl], E2[fl]), c(K1[s, y], K2[s, y])) * LastE[s, fl]
    expect_equal(unname(Eff[s, y, fl, 1]), Want, tolerance = 1e-8, label = paste(s, y, fl))
  }
})

test_that("a fleet does not catch more than its TAC when another fleet in its complex is choked", {
  skip_on_cran()
  om <- MultiStockOM
  nSim(om) <- 3
  pYear(om) <- 3
  for (st in seq_along(om@Fleet)) {
    f <- om@Fleet[[st]][[2]]
    f@Name <- 'ThirdFleet'
    om@Fleet[[st]][['ThirdFleet']] <- f
    om@Imp[[st]][['ThirdFleet']] <- om@Imp[[st]][[2]]
    om@Obs[[st]][['ThirdFleet']] <- om@Obs[[st]][[2]]
  }
  Hist  <- Simulate(om, silent = TRUE)
  Cx    <- names(Hist@OM@Complexes)
  Fl    <- FleetNames(Hist)
  nHist <- dim(Hist@Effort)[2]
  C     <- ArraySum(Hist@Landings, Hist@Discards)
  TB    <- lapply(Hist@OM@Complexes, \(st) apply(C[, st, nHist, , drop = FALSE], 4, sum) / nSim(Hist))
  Sub <- eval(bquote(function(Data) {
    i <- match(Data@Misc$StockName, .(Cx))
    TAC <- 0.6 * .(TB)[[i]]
    if (i == 1) TAC[3] <- NA else TAC[1] <- NA
    Advice(TAC = TAC)
  }))
  Choke <- eval(bquote(function(Data) {
    i <- match(Data@Misc$StockName, .(Cx))
    Advice(TAC = c(0.3, 1.2)[i] * .(TB)[[i]])
  }))
  class(Sub) <- class(Choke) <- 'mp'
  MSE <- suppressMessages(Project(Hist, MPs = list(Sub = Sub, Choke = Choke), parallel = FALSE, silent = TRUE))
  expect_length(MSE@Log$error, 0)
  Att <- TACAttainment(MSE)
  Att <- Att[is.finite(Att$TAC), ]
  expect_true(any(Att$Limit == 'Choke'))
  expect_true(all(Att$Catch <= Att$TAC * (1 + 1e-3)))
  Free <- Att[Att$Limit == 'None', ]
  expect_gt(nrow(Free), 0)
  expect_equal(Free$Catch, Free$TAC, tolerance = 1e-3)
})
