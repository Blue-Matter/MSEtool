MockEval <- function(Objective, Constraint, Min, TolPM = 0.005) {
  function(x) {
    Slack <- (Constraint(x) - Min) / abs(Min)
    data.frame(x = x, Objective = Objective(x), Slack = Slack, Feasible = Slack >= -TolPM)
  }
}
Always <- function(x) rep(1, length(x))

test_that(".TuneSearch() finds constraint boundaries and interior optima", {
  Ctl <- TuneControl()
  B <- .TuneSearch(MockEval(log, \(x) 1 / (1 + x), 0.4), Ctl, TRUE)
  expect_identical(B$Status, 'boundary')
  expect_equal(B$x, 1.5, tolerance = 0.01)
  expect_lte(B$x, 1.5 * 1.001)

  BP <- .TuneSearch(MockEval(log, \(x) 1 / (1 + x), 0.4), Ctl, TRUE, nPerRound = 4)
  expect_equal(BP$x, 1.5, tolerance = 0.01)

  I <- .TuneSearch(MockEval(\(x) -(log(x) - log(2))^2, Always, 0.4), Ctl, TRUE)
  expect_identical(I$Status, 'interior')
  expect_equal(I$x, 2, tolerance = 0.01)

  Dome <- .TuneSearch(MockEval(\(x) -(log(x) - log(5))^2, \(x) 1 / (1 + x / 2), 0.4), Ctl, TRUE)
  expect_identical(Dome$Status, 'boundary')
  expect_equal(Dome$x, 3, tolerance = 0.01)

  Wide <- .TuneSearch(MockEval(log, \(x) 1 / (1 + x / 20), 0.4), Ctl, TRUE)
  expect_equal(Wide$x, 30, tolerance = 0.01)

  Low <- .TuneSearch(MockEval(log, \(x) 1 / (1 + x * 100), 0.4), Ctl, TRUE)
  expect_equal(Low$x, 0.015, tolerance = 0.01)

  None <- .TuneSearch(MockEval(log, \(x) rep(0.1, length(x)), 0.4), Ctl, TRUE)
  expect_identical(None$Status, 'infeasible')
  NoExpand <- .TuneSearch(MockEval(log, \(x) rep(0.1, length(x)), 0.4),
                          TuneControl(Expand = FALSE), TRUE)
  expect_identical(NoExpand$Status, 'infeasible')
  expect_equal(nrow(NoExpand$Points), Ctl$nGrid)

  Edge <- .TuneSearch(MockEval(log, Always, 0.4), TuneControl(Expand = FALSE), TRUE)
  expect_identical(Edge$Status, 'at_bound')
  expect_equal(Edge$x, 10)

  Step <- .TuneSearch(MockEval(log, \(x) ifelse(x < 1.234, 0.7, 0.3), 0.4), Ctl, TRUE)
  expect_equal(Step$x, 1.234, tolerance = 0.01)
  expect_lt(Step$x, 1.234)
})

test_that(".TuneSearch() without an objective maximises or minimises the tuning argument", {
  NoObj <- \(x) rep(NA_real_, length(x))
  Up <- .TuneSearch(MockEval(NoObj, \(x) 1 / (1 + x), 0.4), TuneControl(), FALSE)
  expect_equal(Up$x, 1.5, tolerance = 0.01)
  Down <- .TuneSearch(MockEval(NoObj, \(x) x / (1 + x), 0.4),
                      TuneControl(Direction = 'decreasing'), FALSE)
  expect_equal(Down$x, 2 / 3, tolerance = 0.01)
})

test_that(".TuneAggregate() combines stocks and Hist objects", {
  PerHist <- list(A = list(Values = list(M = c(S1 = 0.7, S2 = 0.5))),
                  B = list(Values = list(M = c(S1 = 0.4, S2 = 0.9))))
  W <- c(A = 1, B = 3)
  n <- c(A = 10, B = 30)
  Con <- function(...) .TuneResolveMetrics(list(utils::modifyList(
    TuneConstraint(PM_SBSBMSY, Min = 0.6, Name = 'M'), list(...))), c('A', 'B'))[[1]]

  Worst <- .TuneAggregate(Con(), PerHist, W, n)
  expect_equal(Worst[['Value']], (0.7 * 1 + 0.4 * 3) / 4)

  Both <- .TuneAggregate(Con(HistSummary = 'worst'), PerHist, W, n)
  expect_equal(Both[['Value']], 0.4)
  expect_equal(Both[['Slack']], (0.4 - 0.6) / 0.6)

  Mean <- .TuneAggregate(Con(StockSummary = 'mean', HistSummary = 'min'), PerHist, W, n)
  expect_equal(Mean[['Value']], 0.6)

  Pooled <- .TuneAggregate(Con(StockSummary = 'max', HistSummary = 'pooled'), PerHist, W, n)
  expect_equal(Pooled[['Value']], (0.7 * 10 + 0.9 * 30) / 40)

  OnlyB <- .TuneResolveMetrics(list(TuneConstraint(PM_SBSBMSY, Min = 0.6, Name = 'M', Hists = 'B')),
                               c('A', 'B'))[[1]]
  expect_equal(.TuneAggregate(OnlyB, PerHist, W, n)[['Value']], 0.4)

  Obj <- .TuneResolveMetrics(list(TuneObjective(PM_Landings, Name = 'M')), c('A', 'B'))[[1]]
  expect_equal(.TuneAggregate(Obj, PerHist, W, n)[['Value']], (1.2 * 1 + 1.3 * 3) / 4)
})

test_that("tuning specifications are validated", {
  expect_error(TuneConstraint(PM_SBSBMSY), 'Min')
  expect_error(TuneConstraint(PM_SBSBMSY, Min = 0.8, Max = 0.5), 'greater')
  expect_error(TuneObjective(PM_Landings, Value = 'Median'), 'Value')
  expect_error(TuneControl(Interval = c(2, 1)), 'Interval')
  expect_identical(TuneConstraint('PM_SBSBMSY', Min = 0.6)$Name, 'PM_SBSBMSY')
  expect_identical(TuneObjective(PM_Landings)$Name, 'PM_Landings')

  Cfg <- .TuneConfigList(list(Smooth = c(TRUE, FALSE), IndexWeight = list(c(1, 1), c(2, 1))),
                         IndexRate, 'tunepar', TuneControl())
  expect_length(Cfg, 4)
  expect_identical(Cfg[[4]], list(Smooth = FALSE, IndexWeight = c(2, 1)))
  Explicit <- .TuneConfigList(list(list(CalibYears = 3), list(CalibYears = 5, Smooth = FALSE)),
                              IndexRate, 'tunepar', TuneControl())
  expect_length(Explicit, 2)
  DF <- .TuneConfigList(data.frame(CalibYears = c(2, 4)), IndexRate, 'tunepar', TuneControl())
  expect_identical(DF[[2]], list(CalibYears = 4))
  expect_error(.TuneConfigList(list(tunepar = 1:2), IndexRate, 'tunepar', TuneControl()), 'tuning argument')
  expect_error(.TuneConfigList(list(Foo = 1:2), IndexRate, 'tunepar', TuneControl()), 'not in')
  expect_error(.TuneConfigList(list(CalibYears = 1:5), IndexRate, 'tunepar', TuneControl(MaxConfigs = 3)),
               'MaxConfigs')
})

test_that("NewPM() and PM_LogYield() build pm objects", {
  df <- expand.grid(Sim = 1:3, Stock = 'S', Year = 2030:2032, MP = c('A', 'B'),
                    stringsAsFactors = FALSE)
  df$Value <- ifelse(df$MP == 'A', 1, 2) * df$Sim
  P <- NewPM(df, Name = 'Test', Ref = 3, Op = `>=`)
  expect_s4_class(P, 'pm')
  expect_equal(P@Mean[1, ], c(A = 1 / 3, B = 2 / 3))
  expect_error(NewPM(df[, -1], Name = 'x'), 'Sim')
  expect_error(NewPM(df, Name = 'x', Op = `>`), 'Ref')
})

test_that("TuneMP() tunes an MP to a constraint boundary over two Hist objects", {
  skip_on_cran()
  OM <- SingleStockOM
  OM@nSim <- 4
  H1 <- Simulate(OM, silent = TRUE)
  OM@Seed <- 42
  H2 <- Simulate(OM, silent = TRUE)
  Hists <- list(A = H1, B = H2)

  Ctl <- TuneControl(nGrid = 5, MaxIter = 6)
  Tuned <- TuneMP(Hists, IndexTarget,
                  Constraints = TuneConstraint(PM_SBSBMSY, Min = 0.6),
                  Control = Ctl, silent = TRUE)
  expect_s4_class(Tuned, 'tunemp')
  expect_true(Tuned@Status %in% c('boundary', 'interior'))
  Con <- TuneTable(Tuned)
  expect_gte(Con$Value[Con$Name == 'PM_SBSBMSY'], 0.6 - 0.6 * Ctl$TolPM)
  expect_identical(attr(Tuned@MP, 'Tuning')$Value, Tuned@Args$tunepar)

  Again <- lapply(Hists, \(h) Project(h, MPs = list(T = Tuned@MP), silent = TRUE))
  SB <- mean(vapply(Again, \(m) PM_SBSBMSY(m)@Mean[1, 'T'], numeric(1)))
  expect_equal(SB, Con$Value[Con$Name == 'PM_SBSBMSY'], tolerance = 1e-8)

  if (Tuned@Status == 'boundary') {
    Up <- SetMPArgs(Tuned@MP, tunepar = Tuned@Args$tunepar * (1 + 3 * Ctl$TolTune))
    SBUp <- mean(vapply(Hists, \(h) {
      PM_SBSBMSY(Project(h, MPs = list(T = Up), silent = TRUE))@Mean[1, 'T']
    }, numeric(1)))
    expect_lte(SBUp, 0.6 + 1e-8)
  }

  expect_error(TuneMP(H1, IndexTarget, Objective = NULL), 'Objective')
  expect_error(TuneMP(H1, IndexTarget, Constraints = list(), Configs = list(Smooth = c(TRUE, FALSE)),
                      Objective = NULL), 'Objective')
  expect_error(TuneMP(H1, IndexTarget, Configs = list(Smooth = 'yes'),
                      Constraints = TuneConstraint(PM_SBSBMSY, Min = 0.6), silent = TRUE),
               'failed')
})

test_that("TuneMP() compares configurations and reports validation", {
  skip_on_cran()
  OM <- SingleStockOM
  OM@nSim <- 3
  Hist <- Simulate(OM, silent = TRUE)
  Tuned <- TuneMP(Hist, IndexTarget,
                  Objective = TuneObjective(PM_Landings),
                  Constraints = TuneConstraint(PM_SBSBMSY, Min = 0.5),
                  Configs = list(RecentYears = 1:2),
                  Control = TuneControl(nGrid = 4, nGridConfig = 3, MaxIter = 4),
                  ValidationHist = Hist, silent = TRUE)
  Cfg <- TuneTable(Tuned, 'Configs')
  expect_equal(nrow(Cfg), 2)
  expect_true(Tuned@Args$RecentYears %in% 1:2)
  expect_equal(sum(!is.na(Cfg$TunedValue)), 1)
  Val <- TuneTable(Tuned, 'Validation')
  expect_setequal(Val$Name, c('PM_Landings', 'PM_SBSBMSY'))
  expect_equal(Val$Value[Val$Name == 'PM_SBSBMSY'],
               Tuned@Constraints$Value[Tuned@Constraints$Name == 'PM_SBSBMSY'], tolerance = 1e-8)

  Dry <- TuneMP(Hist, IndexTarget, Constraints = TuneConstraint(PM_SBSBMSY, Min = 0.5),
                Configs = list(RecentYears = 1:2), Control = TuneControl(DryRun = TRUE),
                silent = TRUE)
  expect_equal(Dry$Stage1Projections, 2 * TuneControl()$nGridConfig)
})

test_that(".SaveRNG()/.RestoreRNG() leave the global RNG state as they found it", {
  Had <- exists('.Random.seed', envir = globalenv(), inherits = FALSE)
  Keep <- if (Had) get('.Random.seed', envir = globalenv())
  on.exit(if (Had) assign('.Random.seed', Keep, envir = globalenv()))

  set.seed(3)
  Before <- get('.Random.seed', envir = globalenv())
  Saved <- .SaveRNG()
  set.seed(99, kind = "L'Ecuyer-CMRG")
  .RestoreRNG(Saved)
  expect_identical(get('.Random.seed', envir = globalenv()), Before)
  expect_identical(RNGkind(), Saved$Kind)

  rm('.Random.seed', envir = globalenv())
  Saved <- .SaveRNG()
  expect_null(Saved$Seed)
  set.seed(99)
  .RestoreRNG(Saved)
  expect_false(exists('.Random.seed', envir = globalenv(), inherits = FALSE))
})
