# Size regulations from MP advice: optional model parameters, Size compliance,
# legacy Rec size limits, and validation errors in the log.

skip_on_cran()

data(SingleStockOM, envir = environment())
data(SeasonalSpatialOM, envir = environment())

.SizeOM <- function(OM = SingleStockOM, nSim = 3, pYear = 3) {
  nSim(OM) <- nSim
  pYear(OM) <- pYear
  OM
}

.RetFrac <- function(MSE) apply(MSE@FRetain, c(3, 5), mean) / apply(MSE@FInteract, c(3, 5), mean)

.Messages <- function(entries) vapply(entries, \(e) e$message, character(1))

SizeLimitMP <- function(Data) {
  Advice <- CurrentEffort(Data)
  Advice@Retention <- Retention(Pars = list(RL50 = 90, RL50_95 = 10))
  Advice
}
class(SizeLimitMP) <- 'mp'

test_that("optional model parameters may be omitted and take their default", {
  expect_equal(.FindModel(Retention(Pars = list(RL50 = 25, RL50_95 = 8))), 'RetentionAtLength')
  expect_equal(.FindModel(Retention(Pars = list(LR5 = 20, LFR = 30))), 'DoubleNormalRetention')
  expect_equal(.FindModel(Selectivity(Pars = list(L5 = 20, LFS = 30))), 'DoubleNormal')
  expect_equal(.AddDefaultPars(Retention(Pars = list(RL50 = 25, RL50_95 = 8)))$MaxRet, 1)
  expect_equal(.AddDefaultPars(Selectivity(Pars = list(L5 = 20, LFS = 30)))$Vmaxlen, 1)

  # fewest omitted optional parameters wins
  expect_equal(.FindModel(Length(Pars = list(Linf = 100, K = 0.2))), 'vonBert')
  expect_equal(.FindModel(Length(Pars = list(Linf = 100, K = 0.2, t0 = 0))), 'vonBert')
  expect_equal(.FindModel(Length(Pars = list(Linf = 100, K = 0.2, d = 2))), 'Richards')

  expect_error(.FindModel(Retention(Pars = list(L5 = 20, LFS = 30))), 'No model found')
  local_mocked_bindings(RetentionModels = function(...) c('RetentionAtLength', 'RetentionAtLength'))
  expect_error(.FindModel(Retention(Pars = list(RL50 = 25, RL50_95 = 8))), 'more than one model')
})

test_that("OM retention and selectivity with omitted optional parameters match the defaults", {
  OM <- .SizeOM(pYear = 2)
  Fl <- OM@Fleet[[1]][[1]]
  Fl@Retention <- Retention(Pars = list(RL50 = 90, RL50_95 = 10))
  Fl@Selectivity <- Selectivity(Pars = list(L5 = 40, LFS = 80))
  OM@Fleet[[1]][[1]] <- Fl
  Hist <- Simulate(OM, silent = TRUE)

  Fl@Retention <- Retention(Pars = list(RL50 = 90, RL50_95 = 10, MaxRet = 1))
  Fl@Selectivity <- Selectivity(Pars = list(L5 = 40, LFS = 80, Vmaxlen = 1))
  OM@Fleet[[1]][[1]] <- Fl
  Ref <- Simulate(OM, silent = TRUE)

  Ret <- Hist@OM@Fleet[[1]][[1]]@Retention
  Sel <- Hist@OM@Fleet[[1]][[1]]@Selectivity
  expect_true(all(Ret@Pars$MaxRet == 1))
  expect_true(all(Sel@Pars$Vmaxlen == 1))
  expect_equal(Ret@MeanAtAge, Ref@OM@Fleet[[1]][[1]]@Retention@MeanAtAge)
  expect_equal(Sel@MeanAtAge, Ref@OM@Fleet[[1]][[1]]@Selectivity@MeanAtAge)
  expect_true(any(Ret@MeanAtAge < 1))
})

test_that("MP retention advice without MaxRet is applied in the projection", {
  Hist <- Simulate(.SizeOM(), silent = TRUE)
  MSE <- suppressMessages(Project(Hist, MPs = list(SizeLimitMP = SizeLimitMP, CurrentEffort = CurrentEffort), silent = TRUE))
  expect_length(MSE@Log$error, 0)
  Frac <- .RetFrac(MSE)
  expect_true(all(Frac[, 'SizeLimitMP'] < 0.99))
  expect_equal(unname(Frac[, 'CurrentEffort']), rep(1, nrow(Frac)), tolerance = 1e-6)
})

test_that("Size compliance is ignored for size regulations and logged once", {
  Hist <- Simulate(.SizeOM(), silent = TRUE)
  Ref <- suppressMessages(Project(Hist, MPs = list(SizeLimitMP = SizeLimitMP), silent = TRUE))

  OM <- .SizeOM()
  Imp(OM) <- Imp(Size = ImpSlot(Compliance = 0.5))
  Hist <- Simulate(OM, silent = TRUE)
  MSE <- suppressMessages(Project(Hist, MPs = list(SizeLimitMP = SizeLimitMP, CurrentEffort = CurrentEffort), silent = TRUE))

  expect_length(MSE@Log$error, 0)
  Entries <- Filter(\(e) identical(e$name, 'SizeCompliance'), MSE@Log$assumption)
  expect_length(Entries, 1)
  expect_equal(Entries[[1]]$mp, 'SizeLimitMP')
  expect_match(Entries[[1]]$message, 'currently ignored')
  expect_equal(MSE@FRetain[, , , , 'SizeLimitMP'], Ref@FRetain[, , , , 'SizeLimitMP'])
})

test_that("legacy Rec size limits map to retention and selectivity models", {
  Rec <- new('Rec')
  Rec@LR5 <- 40; Rec@LFR <- 50
  Advice <- .Rec2Advice(Rec)
  expect_equal(.FindModel(Advice@Retention), 'DoubleNormalRetention')
  expect_equal(Advice@Retention@Pars, list(LR5 = 40, LFR = 50))

  Rec@HS <- 80; Rec@Rmaxlen <- 0.8
  Advice <- .Rec2Advice(Rec)
  expect_identical(Advice@Retention@Model, .LegacySlotRetention)
  expect_equal(Advice@Retention@Pars, list(LR5 = 40, LFR = 50, Rmaxlen = 0.8, HS = 80))
  expect_equal(.LegacySlotRetention(c(30, 50, 79, 80, 100), 40, 50, 80)[4:5], c(0, 0))

  Rec <- new('Rec')
  Rec@L5 <- 30; Rec@LFS <- 45
  Advice <- .Rec2Advice(Rec)
  expect_equal(.FindModel(Advice@Selectivity), 'DoubleNormal')
  expect_null(Advice@Retention)

  Rec <- new('Rec')
  Rec@LFR <- 50
  Advice <- .Rec2Advice(Rec)
  expect_null(Advice@Retention)
  expect_match(.Messages(Advice@Log$assumption), 'must both be set', all = FALSE)
})

test_that("legacy DLMtool size-limit MPs project and change retention", {
  skip_if_not_installed('DLMtool')
  OM <- .SizeOM(nSim = 4, pYear = 3)
  Obs <- OM@Obs
  Obs[[1]][[1]]@LifeHistory <- LifeHistoryObs(Length = list(CV = 0), Weight = list(CV = 0),
                                              NaturalMortality = list(CV = 0),
                                              Maturity = list(CV = 0))
  OM@Obs <- Obs
  Hist <- Simulate(OM, silent = TRUE)

  MPs <- list(matlenlim = DLMtool::matlenlim, slotlim = DLMtool::slotlim,
              minlenLopt1 = DLMtool::minlenLopt1)
  MSE <- suppressMessages(Project(Hist, MPs = MPs, silent = TRUE))
  expect_length(MSE@Log$error, 0)
  Frac <- .RetFrac(MSE)
  expect_true(all(Frac < 0.99))
  expect_true(all(Frac[, 'slotlim'] < Frac[, 'matlenlim']))
})

test_that("advice validation errors reach the log, console, and error log file", {
  OM <- .SizeOM(SeasonalSpatialOM, nSim = 2, pYear = 2)
  Hist <- Simulate(OM, silent = TRUE)
  expect_equal(nArea(Hist@OM), 3)

  BadClosure <- function(Data) {
    Advice <- CurrentEffort(Data)
    if (Data@Misc$Sim == 2) Advice@Closure <- c(1, 0)
    Advice
  }
  class(BadClosure) <- 'mp'
  BadTAC <- function(Data) Advice(TAC = c(1, 2, 3, 4, 5))
  class(BadTAC) <- 'mp'
  BadEffort <- function(Data) Advice(Effort = matrix(1, 1, 7))
  class(BadEffort) <- 'mp'

  LogDir <- tempfile('size-reg-log-')
  OldOpt <- options(MSEtool.WriteLog = LogDir)
  on.exit(options(OldOpt), add = TRUE)

  Console <- capture.output(
    MSE <- Project(Hist, MPs = list(BadClosure = BadClosure, BadTAC = BadTAC, BadEffort = BadEffort),
                   silent = TRUE),
    type = 'message'
  ) |> paste(collapse = ' ')

  Errors <- MSE@Log$error
  Msgs <- .Messages(Errors)
  Expected <- c(BadClosure = 'must be length `nArea`',
                BadTAC = 'TAC is numeric vector, it must be either length 1 or length `nFleet`',
                BadEffort = 'second dimension must be length `nArea`')
  StockName <- names(Hist@OM@Complexes)[1]
  for (mp in names(Expected)) {
    mpMsgs <- Msgs[vapply(Errors, \(e) identical(e$mp, mp), logical(1))]
    expect_true(length(mpMsgs) > 0, label = mp)
    expect_true(all(grepl(Expected[[mp]], mpMsgs, fixed = TRUE)), label = mp)
    expect_true(all(grepl(paste0('Invalid `Advice` for ', StockName), mpMsgs, fixed = TRUE)), label = mp)
  }
  expect_false(any(grepl('did not return', Msgs)))
  expect_true(all(vapply(Errors[grepl('Closure', Msgs)], \(e) identical(e$sim, 2L) || identical(e$sim, 2), logical(1))))
  expect_match(gsub('\\s+', ' ', Console), 'must be length `nArea`', fixed = TRUE)

  Files <- list.files(LogDir, full.names = TRUE)
  expect_length(Files, 3)
  Lines <- unlist(lapply(Files, readLines))
  for (mp in names(Expected))
    expect_true(any(grepl(Expected[[mp]], Lines, fixed = TRUE)), label = mp)
})

test_that("MP-thrown errors keep the MP error message", {
  Hist <- Simulate(.SizeOM(nSim = 2, pYear = 2), silent = TRUE)
  FailMP <- function(Data) stop('deliberate failure')
  class(FailMP) <- 'mp'
  MSE <- suppressMessages(Project(Hist, MPs = list(FailMP = FailMP), silent = TRUE))
  expect_true(all(.Messages(MSE@Log$error) == 'MP error: deliberate failure'))
})

test_that("omitted optional parameters are filled in for stock model objects", {
  Ages <- Ages(10)
  Years <- 2000:2004
  Len <- PopulateLength(Length(Pars = list(Linf = 100, K = 0.2), CVatAge = 0.1),
                        Ages = Ages, Years = Years, nSim = 3, silent = TRUE)
  Ref <- PopulateLength(Length(Pars = list(Linf = 100, K = 0.2, t0 = 0), CVatAge = 0.1),
                        Ages = Ages, Years = Years, nSim = 3, silent = TRUE)
  expect_true(all(Len@Pars$t0 == 0))
  expect_equal(Len@MeanAtAge, Ref@MeanAtAge)
})

test_that("an invalid interim advice is logged with its validation message", {
  OM <- .SizeOM(nSim = 3, pYear = 4)
  Hist <- Simulate(OM, silent = TRUE)
  yp <- Years(Hist, 'P')
  Hist@OM@MPStartYear <- floor(yp[1]) + 2
  Hist@OM@InterimAdvice <- data.frame(Year = yp[1:2], Fleet = NA, Type = 'TAC', Mean = 500)

  local_mocked_bindings(.CheckAdviceTAC = function(...) stop('bad interim TAC'))
  MSE <- suppressMessages(Project(Hist, MPs = 'CurrentEffort', parallel = FALSE, silent = TRUE))

  Msgs <- .Messages(MSE@Log$error)
  StockName <- names(Hist@OM@Complexes)[1]
  expect_true(length(Msgs) > 0)
  expect_true(any(grepl(paste0('Invalid `Advice` for ', StockName, ': bad interim TAC'), Msgs, fixed = TRUE)))
  expect_false(any(grepl('no applicable method', Msgs)))
})
