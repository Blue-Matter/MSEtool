# One projection of every example OM structure (single stock, two fleets, two
# stocks, a stock complex, a hermaphroditic complex, a seasonal 3-area OM)
# under the usual and edge-case control types: no control, scalar and
# fleet-specific TACs, relative, absolute, fleet-specific and fleet x area
# effort, TAC with an effort cap, closures, size limits and discard mortality,
# written as `mp` (one call per stock/complex) and `mmp` (one call per sim).

skip_on_cran()

.MatrixOMs <- c('SingleStockOM', 'TwoFleetOM', 'MultiStockOM', 'ComplexOM', 'HermOM', 'SeasonalSpatialOM')

.MatrixControls <- c('None', 'TAC', 'TACFleet', 'TACFirstFleet', 'TACLandings',
                     'EffRel', 'EffAbsFleet', 'EffFleetArea', 'TACEffortCap',
                     'EffClose', 'TACClose', 'EffSize', 'TACSizeDM', 'TACAll')
.MatrixMMP <- c('TACFleet', 'EffFleetArea', 'TACClose', 'TACAll')

.MatrixCtx <- function(Name, n = 2) {
  OM <- local({
    data(list = Name, envir = environment())
    get(Name)
  })
  nSim(OM)  <- n
  pYear(OM) <- 2
  OM@maxF   <- 10
  Hist  <- Simulate(OM, silent = TRUE)
  nSeas <- Hist@OM@Seasons
  nH    <- dim(Hist@Effort)[2]
  Last  <- seq(nH - nSeas + 1, nH)
  L     <- Hist@Landings
  Cx    <- Hist@OM@Complexes
  TAC   <- lapply(Cx, \(st) 0.5 * apply(L[, st, Last, , drop = FALSE], 4, sum) / nSim(Hist))
  list(Name = Name, Hist = Hist, nSeas = nSeas, nFleet = length(FleetNames(Hist)),
       nArea = nArea(Hist), TAC = TAC,
       LastE = apply(Hist@Effort[, Last, , drop = FALSE], c(1, 3), sum) )
}

.MatrixAdvice <- function(Ctrl, Ctx, Cx) {
  TAC   <- Ctx$TAC[[Cx]]
  nF    <- Ctx$nFleet
  E     <- colMeans(Ctx$LastE)
  Share <- if (Ctx$nArea == 2) c(0.6, 0.4) else c(0.5, 0.3, rep(0.2 / (Ctx$nArea - 2), Ctx$nArea - 2))
  Close <- c(1, 0, rep(1, Ctx$nArea - 2))
  Sel   <- Selectivity(Pars = list(L5 = 0.5, LFS = 0.9), isRel = TRUE)
  Ret   <- Retention(Pars = list(RL50 = 1, RL50_95 = 0.2), isRel = TRUE)
  DM    <- DiscardMortality(MeanAtAge = 0.5)
  switch(Ctrl,
    None          = Advice(),
    TAC           = Advice(TAC = sum(TAC)),
    TACFleet      = Advice(TAC = TAC),
    TACFirstFleet = Advice(TAC = c(TAC[1], rep(NA_real_, nF - 1))),
    TACLandings   = Advice(TAC = TAC, TACType = 'Landings'),
    EffRel        = Advice(Effort = 0.7),
    EffAbsFleet   = Advice(Effort = 0.7 * E, EffType = 'Abs'),
    EffFleetArea  = Advice(Effort = outer(0.7 * E, Share), EffType = 'Abs'),
    TACEffortCap  = Advice(TAC = 10 * TAC, Effort = 0.5),
    EffClose      = Advice(Effort = 0.8, Closure = Close),
    TACClose      = Advice(TAC = TAC, Closure = Close),
    EffSize       = Advice(Effort = 0.7, Selectivity = Sel, Retention = Ret, DiscardMortality = DM),
    TACSizeDM     = Advice(TAC = TAC, Selectivity = Sel, Retention = Ret, DiscardMortality = DM),
    TACAll        = Advice(TAC = TAC, Closure = Close, Selectivity = Sel, Retention = Ret,
                           DiscardMortality = DM)
  )
}

.MatrixMP <- function(Ctrl, Ctx, mmp = FALSE) {
  Ctx$Hist <- NULL
  if (mmp) {
    f <- function(DataList)
      lapply(stats::setNames(names(DataList), names(DataList)), \(nm) .MatrixAdvice(Ctrl, Ctx, nm))
    class(f) <- 'mmp'
  } else {
    f <- function(Data) .MatrixAdvice(Ctrl, Ctx, Data@Misc$StockName)
    class(f) <- 'mp'
  }
  f
}

.MatrixRun <- function(Name, n = 2) {
  Ctx <- .MatrixCtx(Name, n)
  MPs <- c(stats::setNames(lapply(.MatrixControls, .MatrixMP, Ctx = Ctx), .MatrixControls),
           stats::setNames(lapply(.MatrixMMP, .MatrixMP, Ctx = Ctx, mmp = TRUE), paste0(.MatrixMMP, '_mmp')))
  MSE <- suppressMessages(Project(Ctx$Hist, MPs = MPs, parallel = FALSE, silent = TRUE))
  list(Ctx = Ctx, MSE = MSE)
}

.MatrixTotal <- function(df, mp) sum(df$Value[df$MP == mp & df$Period == 'Projection'])

for (Name in .MatrixOMs) local({
  Name <- Name
  Run  <- .MatrixRun(Name)
  Ctx  <- Run$Ctx
  MSE  <- Run$MSE
  MPNames <- names(MSE@MPs)
  Proj <- as.character(Years(Ctx$Hist@OM, 'Projection'))
  nSim <- nSim(Ctx$Hist)

  test_that(paste(Name, ": every control projects without error and gives valid results"), {
    expect_length(MSE@Log$error, 0)
    expect_equal(length(MPNames), length(.MatrixControls) + length(.MatrixMMP))
    for (fn in c('Landings', 'Discards', 'Biomass')) {
      df <- get(fn)(MSE)
      df <- df[df$Period == 'Projection', ]
      expect_false(anyNA(df$Value), label = fn)
      expect_true(all(is.finite(df$Value)), label = fn)
      expect_true(all(df$Value >= -1e-8), label = fn)
    }
  })

  test_that(paste(Name, ": removals stay within the biomass available"), {
    B <- Biomass(MSE)
    B <- B[B$Period == 'Projection', ]
    R <- merge(aggregate(Value ~ MP + Year + Stock, rbind(Landings(MSE), Discards(MSE))[, c('MP', 'Year', 'Stock', 'Value', 'Period')] |>
                           (\(x) x[x$Period == 'Projection', ])(), FUN = sum),
               aggregate(Value ~ MP + Year + Stock, B, FUN = max),
               by = c('MP', 'Year', 'Stock'), suffixes = c('.Removed', '.Biomass'))
    expect_true(all(R$Value.Removed <= R$Value.Biomass * 1.15))
  })

  test_that(paste(Name, ": a TAC is never exceeded, and only TAC controls report one"), {
    Att <- TACAttainment(MSE)
    for (mp in MPNames) {
      Rows   <- Att[Att$MP == mp, , drop = FALSE]
      HasTAC <- grepl('^TAC', sub('_mmp$', '', mp))
      if (HasTAC) {
        expect_gt(nrow(Rows), 0)
        expect_true(all(Rows$Catch <= Rows$TAC * (1 + 1e-3) + 1e-8, na.rm = TRUE), label = mp)
      } else {
        expect_equal(nrow(Rows), 0, label = mp)
      }
    }
  })

  test_that(paste(Name, ": a TAC is caught when no effort ceiling or choke prevents it"), {
    Att <- TACAttainment(MSE)
    for (mp in c('TAC', 'TACFleet', 'TACLandings')) {
      Rows <- Att[Att$MP == mp & is.finite(Att$TAC) & !Att$Limit %in% c('Ceiling', 'Choke'), ]
      if (nrow(Rows))
        expect_true(all(Rows$Catch >= Rows$TAC * (1 - 1e-3)), label = mp)
    }
  })

  test_that(paste(Name, ": effort advice sets each fleet effort"), {
    E     <- MSE@Effort
    Lst   <- Ctx$LastE
    nY    <- dim(E)[2] / Ctx$nSeas
    for (mp in c('EffRel', 'EffAbsFleet', 'EffFleetArea', 'EffFleetArea_mmp')) {
      Want <- switch(sub('_mmp$', '', mp),
                     EffRel = 0.7 * Lst,
                     matrix(0.7 * colMeans(Lst), nrow(Lst), ncol(Lst), byrow = TRUE))
      for (y in seq_len(nY)) {
        ts <- (y - 1) * Ctx$nSeas + seq_len(Ctx$nSeas)
        Got <- apply(E[, ts, , mp, drop = FALSE], c(1, 3), sum)
        expect_equal(unname(Got), unname(Want), tolerance = 1e-6, label = paste(mp, y))
      }
    }
  })

  test_that(paste(Name, ": TAC with an effort cap fishes no harder than the cap"), {
    E   <- MSE@Effort
    Cap <- 0.5 * Ctx$LastE
    nY  <- dim(E)[2] / Ctx$nSeas
    for (y in seq_len(nY)) {
      ts  <- (y - 1) * Ctx$nSeas + seq_len(Ctx$nSeas)
      Got <- apply(E[, ts, , 'TACEffortCap', drop = FALSE], c(1, 3), sum)
      expect_true(all(Got <= Cap * (1 + 1e-6)))
    }
  })

  test_that(paste(Name, ": a closure stops fishing in the closed area and leaves the open area fished"), {
    for (mp in c('EffClose', 'TACClose', 'TACAll', 'TACClose_mmp', 'TACAll_mmp')) {
      for (st in seq_along(MSE@FInteractArea)) {
        FA <- MSE@FInteractArea[[st]]
        PY <- intersect(dimnames(FA)$Year, Proj)
        expect_true(all(FA[, , PY, , '2', mp] == 0), label = paste(mp, st))
        expect_true(any(FA[, , PY, , '1', mp] > 0), label = paste(mp, st))
      }
    }
    for (mp in c('None', 'TAC', 'EffRel')) {
      FA <- MSE@FInteractArea[[1]]
      expect_true(any(FA[, , intersect(dimnames(FA)$Year, Proj), , '2', mp] > 0), label = mp)
    }
  })

  test_that(paste(Name, ": size limits and discard mortality change what is landed and discarded"), {
    L <- Landings(MSE)
    D <- Discards(MSE)
    expect_gt(.MatrixTotal(D, 'EffSize'), .MatrixTotal(D, 'EffRel'))
    expect_false(isTRUE(all.equal(.MatrixTotal(L, 'EffSize'), .MatrixTotal(L, 'EffRel'))))
    for (sl in c('Selectivity', 'Retention'))
      expect_false(is.null(MSE@Misc[[sl]][['EffSize']]), label = sl)
    expect_null(MSE@Misc[['Retention']][['EffRel']])
    expect_false(is.null(MSE@Misc[['DiscardMortality']][['TACSizeDM']]))
  })

  test_that(paste(Name, ": mmp and mp forms of the same advice give the same projection"), {
    for (mp in c('TACFleet', 'EffFleetArea', 'TACClose', 'TACAll')) {
      for (fn in c('Landings', 'Discards')) {
        df <- get(fn)(MSE)
        a  <- df[df$MP == mp & df$Period == 'Projection', ]
        b  <- df[df$MP == paste0(mp, '_mmp') & df$Period == 'Projection', ]
        expect_equal(a$Value, b$Value, tolerance = 1e-6, label = paste(mp, fn))
      }
    }
  })
})

# a sim's projection does not depend on the other sims in the run
for (Name in c('TwoFleetOM', 'MultiStockOM', 'ComplexOM')) local({
  Name <- Name
  test_that(paste(Name, ": a sim's projection is the same alone as within a larger run"), {
    Run  <- .MatrixRun(Name, n = 3)
    Hist <- Run$Ctx$Hist
    Full <- Run$MSE
    MPs  <- Full@MPs
    One  <- suppressMessages(Project(Subset(Hist, Sims = 2), MPs = MPs, parallel = FALSE, silent = TRUE))
    for (fn in c('Landings', 'Discards')) {
      a <- get(fn)(Full)
      a <- a[a$Sim == 2 & a$Period == 'Projection', ]
      b <- get(fn)(One)
      b <- b[b$Period == 'Projection', ]
      for (mp in names(MPs)) {
        x <- a[a$MP == mp, ]
        y <- b[b$MP == mp, ]
        x <- x[order(x$Year, x$Stock, x$Fleet), ]
        y <- y[order(y$Year, y$Stock, y$Fleet), ]
        expect_equal(x$Value, y$Value, tolerance = 1e-6, label = paste(fn, mp))
      }
    }
  })
})

# management advised in the first year only stays in place until changed
for (Name in c('SingleStockOM', 'TwoFleetOM', 'MultiStockOM', 'SeasonalSpatialOM')) local({
  Name <- Name
  test_that(paste(Name, ": management advised once stays in place"), {
    Ctx  <- .MatrixCtx(Name)
    Hist <- Ctx$Hist
    Ctx$Hist <- NULL
    Ctrls <- c('EffRel', 'EffAbsFleet', 'EffFleetArea', 'EffClose', 'EffSize', 'TACFleet', 'TACLandings', 'TACClose', 'TACSizeDM', 'TACAll', 'TACEffortCap')
    Every <- lapply(Ctrls, \(ct) {
      f <- function(Data) .MatrixAdvice(ct, Ctx, Data@Misc$StockName)
      class(f) <- 'mp'
      f
    })
    Once  <- lapply(Ctrls, \(ct) {
      f <- function(Data) if (isTRUE(Data@Misc$FirstAdvice)) .MatrixAdvice(ct, Ctx, Data@Misc$StockName) else Advice()
      class(f) <- 'mp'
      f
    })
    MPs <- c(stats::setNames(Every, paste0(Ctrls, '_every')), stats::setNames(Once, paste0(Ctrls, '_once')))
    MSE <- suppressMessages(Project(Hist, MPs = MPs, parallel = FALSE, silent = TRUE))
    for (fn in c('Landings', 'Discards')) {
      x <- get(fn)(MSE)
      x <- x[x$Period == 'Projection', ]
      for (ct in Ctrls)
        expect_equal(x$Value[x$MP == paste0(ct, '_once')], x$Value[x$MP == paste0(ct, '_every')],
                     tolerance = 1e-8, label = paste(fn, ct))
    }
    for (ct in Ctrls) {
      expect_equal(MSE@Effort[, , , paste0(ct, '_once')], MSE@Effort[, , , paste0(ct, '_every')],
                   tolerance = 1e-8, label = paste('Effort', ct))
      expect_equal(MSE@Distribution[, , , , paste0(ct, '_once')], MSE@Distribution[, , , , paste0(ct, '_every')],
                   tolerance = 1e-8, label = paste('Distribution', ct))
    }
  })
})

test_that("empty advice before any TAC or bag limit is status-quo effort", {
  Ctx  <- .MatrixCtx('TwoFleetOM')
  Hist <- Ctx$Hist
  Ctx$Hist <- NULL
  Late <- function(Data)
    if (isTRUE(Data@Misc$FirstAdvice)) Advice() else .MatrixAdvice('TACFleet', Ctx, Data@Misc$StockName)
  class(Late) <- 'mp'
  None <- .MatrixMP('None', Ctx)
  MSE  <- suppressMessages(Project(Hist, MPs = list(None = None, Late = Late), parallel = FALSE, silent = TRUE))
  x <- Landings(MSE)
  x <- x[x$Period == 'Projection', ]
  y1 <- min(x$Year)
  expect_equal(x$Value[x$MP == 'Late' & x$Year == y1], x$Value[x$MP == 'None' & x$Year == y1], tolerance = 1e-8)
  Att <- TACAttainment(MSE)
  expect_true(all(Att$MP[Att$Year == y1] != 'Late'))
  expect_true(any(Att$MP == 'Late' & Att$Year > y1))
})
