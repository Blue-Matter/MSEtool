# EffortAttainment(): advised, implemented and fished effort of fleets under effort controls

.EffMP <- function(f) {
  class(f) <- 'mp'
  f
}

test_that("EffortAttainment reports effort advice and the limit that reduced it", {
  skip_on_cran()
  om <- TwoFleetOM
  nSim(om) <- 3
  pYear(om) <- 3
  set.seed(1)
  Hist <- Simulate(om, silent = TRUE)
  Fl <- FleetNames(Hist)
  nH <- dim(Hist@Effort)[2]
  Last <- ExtendSims(Hist@Effort, 3)[, nH, ]
  Max  <- 1.5 * Last[, 1]
  Hist@OM@Fleet[[1]][[1]]@Effort@Maximum <- array(Max, c(3, 1), dimnames = list(Sim = 1:3, Year = 2000))
  AllY <- Years(Hist@OM)
  Hist@OM@Imp[[1]][[2]]@Effort@Error <- array(0.8, c(3, length(AllY)),
                                              dimnames = list(Sim = 1:3, Year = as.character(AllY)))

  MPs <- list(
    Eff2   = .EffMP(function(Data) Advice(Effort = 2, EffType = 'Rel')),
    Eff50  = .EffMP(function(Data) Advice(Effort = 50, EffType = 'Rel')),
    Eff1   = .EffMP(function(Data) Advice(Effort = c(1, 0.5), EffType = 'Rel')),
    TACCap = .EffMP(function(Data) Advice(TAC = 1e6, Effort = 0.5)),
    None   = .EffMP(function(Data) Advice())
  )
  MSE <- Project(Hist, MPs = MPs, parallel = FALSE, silent = TRUE)
  df  <- EffortAttainment(MSE)
  expect_named(df, c('Sim', 'MP', 'Year', 'Fleet', 'Advised', 'Implemented', 'Effort', 'Fished', 'Limit'))

  # TAC fleets and empty advice are not effort controls
  expect_false(any(df$MP %in% c('TACCap', 'None')))

  # relative advice as absolute effort; implementation error on fleet 2
  E1 <- df[df$MP == 'Eff1', ]
  expect_true(all(E1$Limit == 'None'))
  expect_equal(E1$Advised[E1$Fleet == Fl[1]], rep(Last[, 1], each = 3), tolerance = 1e-8, ignore_attr = TRUE)
  F2 <- E1[E1$Fleet == Fl[2], ]
  expect_equal(F2$Implemented, 0.8 * F2$Advised, tolerance = 1e-8)
  expect_equal(F2$Effort, F2$Implemented, tolerance = 1e-8)

  # Effort@Maximum caps fleet 1
  E2 <- df[df$MP == 'Eff2' & df$Fleet == Fl[1], ]
  expect_true(all(E2$Limit == 'Maximum'))
  expect_equal(E2$Effort, rep(Max, each = 3), tolerance = 1e-8, ignore_attr = TRUE)
  expect_equal(E2$Fished, E2$Effort / E2$Implemented, tolerance = 1e-8)
  expect_true(all(df$Limit[df$MP == 'Eff2' & df$Fleet == Fl[2]] == 'None'))

  # the fishing mortality cap reduces fleet 2
  E50 <- df[df$MP == 'Eff50' & df$Fleet == Fl[2], ]
  expect_true(all(E50$Limit == 'MaxF'))
  expect_true(all(E50$Fished < 1))

  Msg <- vapply(Filter(\(e) identical(e$name, 'EffortAttainment'), MSE@Log$warning),
                \(e) paste(e$mp, e$message), character(1))
  expect_true(any(grepl('^Eff2 .*effort maximum 9', Msg)))
  expect_true(any(grepl('^Eff50 .*maxF 9', Msg)))
  expect_false(any(grepl('^Eff1 ', Msg)))
})

test_that("a stop-mode bag limit is reported as the limit on effort advice", {
  skip_on_cran()
  om <- SingleStockOM
  nSim(om) <- 2
  pYear(om) <- 2
  om@Fleet[[1]][[1]]@Effort@TripsScalar <- 20
  om@Fleet[[1]][[1]]@Effort@Theta       <- 2
  om@Fleet[[1]][[1]]@DiscardMortality   <- DiscardMortality(MeanAtAge = rep(1, nAge(om@Stock[[1]])))
  set.seed(1)
  Hist <- Simulate(om, silent = TRUE)
  Bag <- .EffMP(function(Data) Advice(Effort = 1, EffType = 'Rel', BagLimit = 2, LimitType = 'boat',
                                      ClosureMode = 'stop'))
  MSE <- suppressMessages(Project(Hist, MPs = list(Bag = Bag), parallel = FALSE, silent = TRUE))
  df <- EffortAttainment(MSE)
  BL <- BagLimitEffect(MSE)
  expect_gt(nrow(df), 0)
  Reduced <- unique(paste(BL$Sim, BL$Year)[BL$TimeFraction < 1 - 1e-6])
  expect_gt(length(Reduced), 0)
  expect_true(all(df$Limit[paste(df$Sim, df$Year) %in% Reduced] == 'BagLimit'))
  expect_true(all(df$Fished[df$Limit == 'BagLimit'] < 1))
})

test_that("effort and TAC attainment are the same when the projection runs in sim chunks", {
  skip_on_cran()
  om <- TwoFleetOM
  nSim(om) <- 4
  pYear(om) <- 2
  set.seed(1)
  Hist <- Simulate(om, silent = TRUE)
  MPs <- list(Eff50 = .EffMP(function(Data) Advice(Effort = 50, EffType = 'Rel')),
              TAC   = .EffMP(function(Data) Advice(TAC = 1e6)))
  Whole <- Project(Hist, MPs = MPs, parallel = FALSE, silent = TRUE)
  Hist@OM@Control$ProjectChunks <- 2
  Chunked <- Project(Hist, MPs = MPs, parallel = FALSE, silent = TRUE)
  expect_equal(EffortAttainment(Chunked), EffortAttainment(Whole))
  expect_equal(TACAttainment(Chunked), TACAttainment(Whole))
  Msg <- \(M) sort(vapply(M@Log$warning, \(e) paste(e$mp, e$name, e$message), character(1)))
  expect_identical(Msg(Chunked), Msg(Whole))
})
