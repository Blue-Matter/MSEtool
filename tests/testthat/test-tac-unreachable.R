# A TAC is a catch target: each fleet fishes until it catches its share, or
# until its effort reaches its ceiling (Effort@Maximum, else the effort at
# which maxF binds). Shortfalls are recorded in MSE@Misc$TACAttainment.

BigTAC <- function(Data) Advice(TAC = 1e9)
class(BigTAC) <- 'mp'

.TACMP <- function(TAC) {
  mp <- eval(bquote(function(Data) Advice(TAC = .(TAC))))
  class(mp) <- 'mp'
  mp
}

# total apical interaction F over fleets, [Sim, Year, MP]
.TotalF <- function(MSE, st = 1)
  apply(MSE@FInteract[, st, , , , drop = FALSE], c(1, 3, 5), sum)

.LastCatch <- function(Hist, st = 1) {
  ny <- dim(Hist@Landings)[3]
  apply(ArraySum(Hist@Landings, Hist@Discards)[, st, ny, , drop = FALSE], c(1, 4), sum)
}

.EffortConvergenceLog <- function(MSE)
  Filter(\(e) identical(e$name, 'EffortConvergence'), MSE@Log$warning)

test_that("a single fleet that cannot catch its TAC fishes at maxF and is reported", {
  skip_on_cran()
  om <- SingleStockOM
  nSim(om) <- 4
  pYear(om) <- 4
  Hist <- Simulate(om, silent = TRUE)
  Half <- .TACMP(0.5 * mean(.LastCatch(Hist)))

  Msg <- testthat::capture_messages(
    MSE <- Project(Hist, MPs = list(BigTAC = BigTAC, Half = Half), parallel = FALSE)
  )
  expect_equal(sum(grepl("TAC not caught", Msg)), 1)
  expect_true(any(grepl("BigTAC TAC not caught in 16 of 16 sim-years", Msg)))

  expect_equal(range(.TotalF(MSE)[, , 'BigTAC']), c(3, 3), tolerance = 0.01)
  expect_length(.EffortConvergenceLog(MSE), 0)

  df <- TACAttainment(MSE)
  expect_equal(nrow(df), 2 * 4 * 4)
  expect_true(all(df$Limit[df$MP == 'BigTAC'] == 'Ceiling'))

  # reachable TAC: caught exactly, nothing reported
  ok <- df[df$MP == 'Half', ]
  expect_true(all(ok$Limit == 'None'))
  expect_equal(ok$Catch, ok$TAC, tolerance = 1e-3)
  pm <- PM_TACAttained(MSE)
  expect_equal(unname(pm@Mean[1, 'Half']), 1)
  expect_equal(unname(pm@Mean[1, 'BigTAC']), 0)
})

test_that("a fleet with a reachable TAC catches it while another fishes to its ceiling", {
  skip_on_cran()
  om <- TwoFleetOM
  nSim(om) <- 4
  pYear(om) <- 3
  Hist <- Simulate(om, silent = TRUE)
  TAC1 <- 0.5 * mean(.LastCatch(Hist)[, 1])

  MSE <- Project(Hist, MPs = list(OneShort = .TACMP(c(TAC1, 1e9)), BigTAC = BigTAC,
                                  OneFleet = .TACMP(c(1e9, 0))),
                 parallel = FALSE, silent = TRUE)
  df <- TACAttainment(MSE)
  Fl <- FleetNames(Hist)

  One <- df[df$MP == 'OneShort', ]
  F1  <- One[One$Fleet == Fl[1], ]
  expect_true(all(F1$Catch <= F1$TAC * (1 + 1e-3)))
  expect_true(all(F1$Limit[F1$Year == min(F1$Year)] == 'None'))
  expect_equal(F1$Catch[F1$Limit == 'None'], F1$TAC[F1$Limit == 'None'], tolerance = 1e-3)
  expect_true(all(One$Limit[One$Fleet == Fl[2]] == 'Ceiling'))

  expect_true(all(df$Limit[df$MP == 'BigTAC'] == 'Ceiling'))
  for (mp in c('OneShort', 'BigTAC'))
    expect_equal(range(.TotalF(MSE)[, , mp]), c(3, 3), tolerance = 0.01, label = mp)

  # single active fleet: at its ceiling, not at last year's effort
  expect_equal(range(.TotalF(MSE)[, , 'OneFleet']), c(3, 3), tolerance = 0.01)
  expect_true(all(MSE@Effort[, , 2, 'OneFleet'] == 0))
  expect_length(.EffortConvergenceLog(MSE), 0)
})

test_that("Effort@Maximum caps effort for TACs and effort advice", {
  skip_on_cran()
  om <- SingleStockOM
  nSim(om) <- 3
  pYear(om) <- 3
  Hist <- Simulate(om, silent = TRUE)
  nHist <- dim(Hist@Effort)[2]
  Max   <- 0.5 * Hist@Effort[, nHist, 1]
  Hist@OM@Fleet[[1]][[1]]@Effort@Maximum <- array(Max, c(3, 1),
                                                   dimnames = list(Sim = 1:3, Year = 2000))

  MoreEffort <- function(Data) Advice(Effort = 2, EffType = 'Rel')
  class(MoreEffort) <- 'mp'
  MSE <- Project(Hist, MPs = list(BigTAC = BigTAC, MoreEffort = MoreEffort),
                 parallel = FALSE, silent = TRUE)

  for (mp in c('BigTAC', 'MoreEffort'))
    expect_equal(range(MSE@Effort[, , 1, mp] / Max), c(1, 1), tolerance = 1e-8, label = mp)
  expect_true(all(.TotalF(MSE)[, , 'BigTAC'] < 2.9))
  expect_true(all(TACAttainment(MSE)$Limit == 'Ceiling'))

  om2 <- TwoFleetOM
  om2@Fleet[[1]][[1]]@Effort@Maximum <- 1
  expect_error(CombineFleets(om2, silent = TRUE), "Maximum")

  Eff <- Effort(Effort = matrix(1, 1, 3, dimnames = list(Sim = 1, Year = 2001:2003)), Maximum = -1)
  expect_error(PopulateEffort(Eff, HistYears = 2001:2003, nSim = 1), "positive")
})

test_that("multi-complex: an unreachable complex never binds under full compliance", {
  skip_on_cran()
  om <- MultiStockOM
  nSim(om) <- 3
  pYear(om) <- 3
  set.seed(1)
  Hist <- Simulate(om, silent = TRUE)
  assign('.TAC2', 0.5 * rowSums(.LastCatch(Hist, 2)), globalenv())
  assign('.TAC2fl', 1.5 * .LastCatch(Hist, 2), globalenv())
  on.exit(rm('.TAC2', '.TAC2fl', envir = globalenv()), add = TRUE)

  Unreach1 <- function(Data) {
    if (grepl('Albacore', Data@Name)) return(Advice(TAC = 1e9))
    Advice(TAC = .TAC2[Data@Misc$Sim])
  }
  # complex 1 is fished by fleet 1 only
  Fleet1Only <- function(Data) {
    if (grepl('Albacore', Data@Name)) return(Advice(TAC = c(1e9, 0)))
    Advice(TAC = .TAC2fl[Data@Misc$Sim, ])
  }
  class(Unreach1) <- class(Fleet1Only) <- 'mp'
  MSE <- Project(Hist, MPs = list(Unreach1 = Unreach1, Fleet1Only = Fleet1Only),
                 parallel = FALSE, silent = TRUE)
  df <- TACAttainment(MSE)
  Cx <- names(Hist@OM@Complexes)

  First <- df[df$MP == 'Unreach1' & df$Year == min(df$Year), ]
  Cx2 <- First[First$Stock == Cx[2], ]
  expect_equal(tapply(Cx2$Catch, Cx2$Sim, sum), tapply(Cx2$TAC, Cx2$Sim, sum),
               tolerance = 1e-3, ignore_attr = TRUE)
  expect_true(all(df$Limit[df$MP == 'Unreach1' & df$Stock == Cx[1]] == 'Choke'))

  # fleet 1 is not held back by the complex it cannot fill
  F1 <- df[df$MP == 'Fleet1Only' & df$Stock == Cx[2] & df$Fleet == FleetNames(Hist)[1], ]
  expect_false(any(F1$Limit == 'Choke'))

  Sub <- TACAttainment(Subset(MSE, Stocks = StockNames(Hist)[Hist@OM@Complexes[[2]]]))
  expect_equal(Sub[, c('Sim', 'MP', 'Year', 'Fleet', 'TAC', 'Catch')],
               df[df$Stock == Cx[2], c('Sim', 'MP', 'Year', 'Fleet', 'TAC', 'Catch')],
               ignore_attr = TRUE)

  # zero compliance: fleets fish to the last TAC, which is never reached
  for (cx in seq_along(Hist@OM@Imp))
    for (fl in seq_along(Hist@OM@Imp[[cx]]))
      Hist@OM@Imp[[cx]][[fl]]@TAC@Compliance <- 0
  MSE0 <- Project(Hist, MPs = list(Unreach1 = Unreach1), parallel = FALSE, silent = TRUE)
  df0 <- TACAttainment(MSE0)
  expect_true(all(df0$Limit[df0$Stock == Cx[1]] == 'Ceiling'))
})

test_that("seasonal TACs are per-season shares with no carryover", {
  skip_on_cran()
  om <- SeasonalSpatialOM
  nSim(om) <- 3
  pYear(om) <- 2
  set.seed(1)
  Hist <- Simulate(om, silent = TRUE)
  Seasons <- Hist@OM@Seasons
  ny <- dim(Hist@Landings)[3]
  LastYr <- (ny - Seasons + 1):ny
  Annual <- 0.5 * mean(apply(ArraySum(Hist@Landings, Hist@Discards)[, 1, LastYr, , drop = FALSE], 1, sum))
  Reach <- .TACMP(Annual)

  Msg <- testthat::capture_messages(
    MSE <- Project(Hist, MPs = list(BigTAC = BigTAC, Reach = Reach), parallel = FALSE)
  )
  expect_true(any(grepl("sim-time steps", Msg)))

  df <- TACAttainment(MSE)
  SA <- Hist@OM@SeasonalAllocation[[1]]
  Big <- df[df$MP == 'BigTAC', ]
  AllYears <- c(Years(Hist@OM, 'Historical'), Years(Hist@OM, 'Projection'))
  Season <- SeasonOfYear(AllYears, Seasons, Big$Year)$SeasonInd
  expect_equal(Big$TAC, 1e9 * SA[cbind(pmin(Big$Sim, dim(SA)[1]), Season, 1)], tolerance = 1e-8)
  expect_true(all(Big$Limit == 'Ceiling'))

  R1 <- df[df$MP == 'Reach' & df$Year < min(df$Year) + 1, ]
  expect_equal(nrow(R1), 3 * Seasons)
  expect_equal(R1$Catch, R1$TAC, tolerance = 1e-3)
})

test_that("a landings TAC is measured in landings", {
  skip_on_cran()
  om <- SingleStockOM
  nSim(om) <- 3
  pYear(om) <- 2
  Hist <- Simulate(om, silent = TRUE)
  Land <- function(Data) Advice(TAC = 1e9, TACType = 'Landings')
  class(Land) <- 'mp'
  MSE <- Project(Hist, MPs = list(Land = Land), parallel = FALSE, silent = TRUE)
  df <- TACAttainment(MSE)
  L <- apply(MSE@Landings[, 1, , , 1, drop = FALSE], c(1, 3), sum)
  expect_equal(df$Catch, L[cbind(df$Sim, match(df$Year, as.numeric(colnames(L))))],
               tolerance = 1e-6)
  expect_true(all(df$Limit == 'Ceiling'))
})

test_that("TAC attainment survives Subset, CombineMSE, chunking and failed sims", {
  skip_on_cran()
  om <- TwoFleetOM
  nSim(om) <- 4
  pYear(om) <- 3
  Hist <- Simulate(om, silent = TRUE)
  FailYear <- Years(Hist@OM, 'Projection')[2]
  PartFail <- function(Data) {
    if (Data@Misc$Sim == 3 && Data@Misc$AdviceYear >= FailYear) stop("sim 3 failed")
    Advice(TAC = 1e9)
  }
  class(PartFail) <- 'mp'
  MPs <- list(BigTAC = BigTAC, PartFail = PartFail)

  MSE <- suppressMessages(Project(Hist, MPs = MPs, parallel = FALSE, silent = TRUE))
  df  <- TACAttainment(MSE)
  Key <- \(d) d[, c('Sim', 'Stock', 'MP', 'Year', 'Fleet', 'TAC', 'Catch', 'Limit')]

  Fail <- df[df$MP == 'PartFail' & df$Sim == 3, ]
  expect_true(all(is.na(Fail$Catch)))
  expect_false(anyNA(df$Catch[df$MP == 'BigTAC']))

  Hist2 <- Hist
  Hist2@OM@Control$ProjectChunks <- 2
  MSE2 <- suppressMessages(Project(Hist2, MPs = MPs, parallel = FALSE, silent = TRUE))
  expect_equal(Key(TACAttainment(MSE2)), Key(df), tolerance = 1e-8)

  expect_equal(Key(TACAttainment(Subset(MSE, MPs = 2))), Key(df[df$MP == 'PartFail', ]),
               ignore_attr = TRUE)
  expect_equal(Key(TACAttainment(Subset(MSE, MPs = 'BigTAC'))), Key(df[df$MP == 'BigTAC', ]),
               ignore_attr = TRUE)
  expect_named(Subset(MSE, MPs = 1)@Misc$Advice, 'BigTAC')

  Sub <- TACAttainment(Subset(MSE, Sims = c(2, 4)))
  Ref <- df[df$Sim %in% c(2, 4), ]
  Ref$Sim <- match(Ref$Sim, c(2, 4))
  expect_equal(Key(Sub), Key(Ref), ignore_attr = TRUE)

  Yr <- Years(Hist@OM, 'Projection')[1:2]
  expect_equal(Key(TACAttainment(Subset(MSE, Years = Yr))), Key(df[df$Year %in% Yr, ]),
               ignore_attr = TRUE)
  Fl <- FleetNames(Hist)[2]
  expect_equal(Key(TACAttainment(Subset(MSE, Fleets = Fl))), Key(df[df$Fleet == Fl, ]),
               ignore_attr = TRUE)

  Comb <- TACAttainment(CombineMSE(list(MSE, MSE), silent = TRUE))
  Ref2 <- rbind(df, transform(df, Sim = Sim + 4))
  expect_equal(Key(Comb), Key(dplyr::arrange(Ref2, Sim, Stock, MP, Year, Fleet)),
               ignore_attr = TRUE)
})
