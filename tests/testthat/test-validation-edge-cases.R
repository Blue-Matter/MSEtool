.EdgeFixture <- new.env()

# SingleStockOM, 3 sims, short projection
.EdgeOM <- function() {
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 3
  om@pYear <- 6
  om
}

.EdgeHist <- function() {
  if (is.null(.EdgeFixture$hist)) {
    set.seed(1)
    .EdgeFixture$hist <- Simulate(.EdgeOM(), silent = TRUE)
  }
  .EdgeFixture$hist
}

# ---- OM@Relations ----

test_that("Simulate() aborts when OM@Relations is not empty", {
  skip_on_cran()
  om <- .EdgeOM()
  om@Relations <- list(list(Stock = 1))
  expect_error(Simulate(om, silent = TRUE), "Relations")
})

test_that("HistRel is no longer a recognised OM@Control setting", {
  expect_false('HistRel' %in% MSEtool:::.OMControlNames)
})

# ---- Project() without historical data ----

test_that("Project() aborts clearly when the hist has no generated data", {
  skip_on_cran()
  h <- Simulate(.EdgeOM(), nSim = 2, silent = TRUE,
                control = SimControl(GenerateData = FALSE))
  expect_error(Project(h, MPs = 'CurrentEffort', silent = TRUE), "GenerateData")
})

# ---- Depletion@Reference ----

test_that("Depletion@Reference accepts only B0 and SB0", {
  expect_s4_class(Depletion(Final = 0.4, Reference = 'B0'), 'depletion')
  expect_s4_class(Depletion(Final = 0.4, Reference = 'SB0'), 'depletion')
  expect_s4_class(methods::new('depletion'), 'depletion')
  for (ref in c('BMSY', 'SBMSY', 'SP0', 'SPMSY'))
    expect_error(Depletion(Final = 0.4, Reference = ref), "Reference", label = ref)
  expect_error(Depletion(Reference = c('B0', 'SB0')), "Reference")
  dep <- Depletion(Final = 0.4)
  dep@Reference <- 'BMSY'
  expect_error(methods::validObject(dep), "Reference")
})

# ---- DataCV in OM@Control ----

test_that("re-simulating a hist does not warn about OM@Control$DataCV", {
  skip_on_cran()
  hist <- .EdgeHist()
  expect_false(is.null(hist@OM@Control$DataCV))
  Msgs <- character()
  withCallingHandlers(
    Simulate(hist, silent = TRUE),
    message = function(m) {
      Msgs <<- c(Msgs, conditionMessage(m))
      invokeRestart('muffleMessage')
    }
  )
  expect_false(any(grepl('DataCV', Msgs)))
})

# ---- relative change with zero previous value ----

test_that(".RelChange gives 0 for 0 -> 0 and 1 for 0 -> positive", {
  ch <- data.frame(Sim = 1:4, Stock = 'A', Year = 2030, MP = 'MP1',
                   Value = c(0, 50, 0, 150), Prev = c(0, 0, 100, 100))
  out <- MSEtool:::.RelChange(ch)
  expect_equal(out$Value, c(0, 1, 1, 0.5))
  expect_true(all(is.finite(out$Value)))
})

test_that("PM_AAVY and PM_Stability are finite after closures", {
  skip_on_cran()
  hist <- .EdgeHist()
  # alternates closure and reopening
  Closure <- function(Data) {
    Prev <- LastTAC(Data)
    Advice(TAC = if (isTRUE(Prev > 0)) 0 else 100)
  }
  class(Closure) <- 'mp'
  mse <- Project(hist, MPs = list(Closure = Closure), silent = TRUE)
  aav <- PM_AAVY(mse)
  expect_true(all(is.finite(aav@Stat)))
  expect_true(all(aav@Stat <= 1))
  stab <- PM_Stability(mse, Threshold = 0.5)
  expect_true(all(is.finite(stab@Stat)))
})

# ---- Fmax ----

test_that(".FindFmax returns NA without an interior maximum", {
  Fgrid <- exp(seq(log(1e-4), log(3), length.out = 50))
  expect_true(is.na(MSEtool:::.FindFmax(1 - exp(-Fgrid), Fgrid)))
  Dome <- Fgrid * exp(-Fgrid)
  expect_equal(MSEtool:::.FindFmax(Dome, Fgrid), Fgrid[which.max(Dome)])
  expect_true(is.na(MSEtool:::.FindFmax(rep(NA_real_, 50), Fgrid)))
})

test_that("CalcRefPoints() Fmax is NA or below maxF for every sim", {
  skip_on_cran()
  hist <- CalcRefPoints(.EdgeHist(), silent = TRUE)
  FMax <- as.numeric(FMax(hist))
  expect_true(all(is.na(FMax) | FMax < hist@OM@maxF))
})

# ---- FMSY at the maxF bound ----

test_that("FMSY at the maxF bound is flagged and logged per sim, for R and C++ solvers", {
  skip_on_cran()
  hist <- .EdgeHist()
  expect_null(hist@Reference@MSY@Misc$FMSYAtMaxF)

  Low <- hist
  Low@OM@maxF <- 0.01
  for (Fun in list(CalcMSY, CalcMSYCpp)) {
    Ref <- Fun(Low, silent = TRUE)
    Flags <- Ref@Misc$FMSYAtMaxF
    expect_s3_class(Flags, 'data.frame')
    expect_setequal(Flags$Sim, seq_len(nSim(hist)))
    expect_equal(unique(Flags$Complex), names(Complexes(hist)))
    expect_true(all(abs(as.numeric(Ref@FMSY) - 0.01) < 1e-4))

    Logged <- Low
    Logged@Log <- list()
    Logged@Reference@MSY <- Ref
    Logged <- MSEtool:::.LogFMSYAtMaxF(Logged)
    Entry <- Logged@Log$warning[[1]]
    expect_equal(Entry$name, 'FMSY')
    expect_equal(Entry$sim, seq_len(nSim(hist)))
    expect_match(Entry$message, 'maxF')
  }

  Unflagged <- MSEtool:::.LogFMSYAtMaxF(hist)
  expect_identical(Unflagged@Log, hist@Log)
})

test_that(".LogFMSYAtMaxF does nothing when MSY is not calculated", {
  skip_on_cran()
  hist <- Simulate(.EdgeOM(), silent = TRUE, control = SimControl(MSYRefs = FALSE))
  expect_null(hist@Reference@MSY@FMSY)
  expect_identical(MSEtool:::.LogFMSYAtMaxF(hist), hist)
})

# ---- reference MPs ----

test_that("ReferenceMPs() lists every reference MP", {
  MPs <- ReferenceMPs()
  expect_true(all(c('refFMSY', 'refFMSY75', 'refFMSY50', 'refMSY', 'refMSY75',
                    'refMSY50', 'refFCurr', 'NoFishing') %in% MPs))
  for (mp in MPs) expect_s3_class(get(mp), 'mp')
})

# ---- wrapped legacy MPs and life-history data ----

test_that("wrapped legacy MP errors say life-history data are not generated", {
  skip_on_cran()
  hist <- .EdgeHist()
  NeedsM <- function(x, Data, reps = 1, plot = FALSE) {
    Rec <- new('Rec')
    Rec@TAC <- Data@Mort[x] * 100
    if (is.na(Rec@TAC)) stop('natural mortality is missing')
    Rec
  }
  class(NeedsM) <- 'MP'
  Wrapped <- MSEtool:::.WrapLegacyMP(NeedsM)
  for (sim in seq_along(hist@Data))
    expect_error(Wrapped(hist@Data[[sim]][[1]]),
                 "natural mortality is missing.*life-history data are not generated")

  Plain <- function(x, Data, reps = 1, plot = FALSE) stop('unrelated failure')
  class(Plain) <- 'MP'
  LegacyData <- Convert(hist@Data[[1]][[1]], silent = TRUE)
  LegacyData@Mort <- LegacyData@vbK <- LegacyData@vbLinf <- LegacyData@vbt0 <- 1
  LegacyData@L50 <- LegacyData@L95 <- LegacyData@wla <- LegacyData@wlb <- 1
  LegacyData@steep <- LegacyData@MaxAge <- 1
  Err <- tryCatch(MSEtool:::.LegacyMPError(simpleError('unrelated failure'), LegacyData),
                  error = conditionMessage)
  expect_identical(Err, 'unrelated failure')
})
