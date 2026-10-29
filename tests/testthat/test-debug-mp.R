.DebugFixture <- new.env()

.DebugHist <- function(multi = FALSE) {
  key <- if (multi) 'multi' else 'single'
  if (is.null(.DebugFixture[[key]])) {
    OM <- if (multi) MultiStockOM else SingleStockOM
    nSim(OM) <- if (multi) 2 else 3
    OM@pYear <- 5
    .DebugFixture[[key]] <- Simulate(OM, silent = TRUE)
  }
  .DebugFixture[[key]]
}

.DebugProject <- function(Hist, MPs) suppressMessages(Project(Hist, MPs = MPs, silent = TRUE))

.NDebug <- function(MSE) sum(vapply(MSE@Log$error, \(e) !is.null(e$debug), logical(1)))

test_that("DebugMP() returns the inputs of the first failed MP call", {
  skip_on_cran()
  Hist <- .DebugHist()
  RandomFail <- function(Data, Target = 0.5) {
    x <- stats::runif(1)
    stop(sprintf("draw %.10f", x))
  }
  class(RandomFail) <- 'mp'
  RF3 <- SetMPArgs(RandomFail, Target = 0.3)

  MSE <- .DebugProject(Hist, list(RF3 = RF3, CurrentEffort = CurrentEffort))
  expect_equal(.NDebug(MSE), 1)
  expect_gt(length(MSE@Log$error), 1)

  d <- DebugMP(MSE)
  expect_s3_class(d, 'mpdebug')
  expect_identical(d$MP, 'RF3')
  expect_equal(d$Sim, 1)
  expect_equal(d$Year, Years(Hist@OM, 'Projection')[1])
  expect_s4_class(d$Data, 'data')
  expect_equal(d$Args, list(Target = 0.3))
  expect_true(any(grepl('^stop\\(', d$Calls)))
  expect_match(d$Calls[1], '^MPfunction\\(')
  expect_no_error(suppressMessages(print(d)))

  # same seed as in the projection
  Err <- tryCatch(DebugMP(MSE, run = TRUE), error = conditionMessage)
  expect_true(grepl(Err, d$Message, fixed = TRUE))

  expect_error(DebugMP(MSE, MP = 'CurrentEffort'), 'No MP errors')
  expect_error(DebugMP(MSE, Sim = 2), 'not stored')
})

test_that("DebugMP() stores the returned value of invalid Advice", {
  skip_on_cran()
  Hist <- .DebugHist()
  NotAdvice <- function(Data) 1
  class(NotAdvice) <- 'mp'
  MSE <- .DebugProject(Hist, list(NotAdvice = NotAdvice))
  d <- DebugMP(MSE)
  expect_identical(d$Returned, 1)
  expect_length(d$Calls, 0)
  expect_identical(DebugMP(MSE, run = TRUE), 1)
})

test_that("options(MSEtool.DebugMP) sets how many failed calls are stored", {
  skip_on_cran()
  Hist <- .DebugHist()
  FailMP <- function(Data) stop("deliberate failure")
  class(FailMP) <- 'mp'
  OldOpt <- options(MSEtool.DebugMP = 'all')
  on.exit(options(OldOpt), add = TRUE)

  MSE <- .DebugProject(Hist, list(FailMP = FailMP))
  expect_equal(.NDebug(MSE), length(MSE@Log$error))
  expect_equal(DebugMP(MSE, Sim = 3)$Sim, 3)

  options(MSEtool.DebugMP = FALSE)
  MSE <- .DebugProject(Hist, list(FailMP = FailMP))
  expect_equal(.NDebug(MSE), 0)
  expect_error(DebugMP(MSE), 'MSEtool.DebugMP')

  options(MSEtool.DebugMP = 'some')
  expect_error(.DebugProject(Hist, list(FailMP = FailMP)), 'MSEtool.DebugMP')
})

test_that("DebugMP() reports the failing sim of a chunked projection", {
  skip_on_cran()
  Hist <- .DebugHist()
  FailYear <- Years(Hist@OM, 'Projection')[3]
  PartFail <- function(Data) {
    if (Data@Misc$Sim == 2 && Data@Misc$AdviceYear >= FailYear) stop("sim 2 failed")
    CurrentEffort(Data)
  }
  class(PartFail) <- 'mp'

  for (chunks in list(NULL, 2)) {
    Hist@OM@Control$ProjectChunks <- chunks
    MSE <- .DebugProject(Hist, list(PartFail = PartFail))
    d <- DebugMP(MSE)
    expect_equal(.NDebug(MSE), 1)
    expect_equal(d$Sim, 2)
    expect_equal(d$Seed$Sim, 2)
    expect_equal(d$Year, FailYear)
    expect_equal(d$Data@Misc$Sim, 2)
    expect_error(DebugMP(MSE, run = TRUE), 'sim 2 failed')
  }
})

test_that("DebugMP() works for mmp-class MPs", {
  skip_on_cran()
  Hist <- .DebugHist(multi = TRUE)
  FailMMP <- function(DataList) stop("mmp failure")
  class(FailMMP) <- 'mmp'
  BadMMP <- function(DataList) list(1)
  class(BadMMP) <- 'mmp'
  MSE <- .DebugProject(Hist, list(FailMMP = FailMMP, BadMMP = BadMMP))

  d <- DebugMP(MSE, MP = 'FailMMP')
  expect_identical(names(d$Data), StockNames(Hist))
  expect_true(all(vapply(d$Data, methods::is, logical(1), 'data')))
  expect_error(DebugMP(MSE, MP = 'FailMMP', run = TRUE), 'mmp failure')

  d <- DebugMP(MSE, MP = 'BadMMP')
  expect_identical(d$Returned, list(1))
})

test_that("DebugMP() reruns wrapped legacy MPs on legacy Data", {
  skip_on_cran()
  Hist <- .DebugHist()
  NeedsM <- function(x, Data, reps = 1, plot = FALSE, Mult = 100) {
    Rec <- new('Rec')
    Rec@TAC <- Data@Mort[x] * Mult
    if (is.na(Rec@TAC)) stop('natural mortality is missing')
    Rec
  }
  class(NeedsM) <- 'MP'
  MSE <- .DebugProject(Hist, list(NeedsM = NeedsM))
  d <- DebugMP(MSE)
  expect_equal(d$Args, list(Mult = 100))
  expect_error(DebugMP(MSE, run = TRUE), 'natural mortality is missing')
})

test_that(".SplitMPDebug() removes the debug records from the advice", {
  Fail <- structure('Error', Debug = list(a = 1))
  Split <- MSEtool:::.SplitMPDebug(list(S1 = Advice(), S2 = Fail))
  expect_null(attr(Split$Value$S2, 'Debug'))
  expect_identical(Split$Debug, list(S2 = list(a = 1)))

  Split <- MSEtool:::.SplitMPDebug(structure('Error', class = 'try-error', Debug = list(b = 2)))
  expect_s3_class(Split$Value, 'try-error')
  expect_null(attr(Split$Value, 'Debug'))
  expect_identical(Split$Debug, list(.Sim = list(b = 2)))

  expect_null(MSEtool:::.SplitMPDebug(list(S1 = Advice()))$Debug)
})
