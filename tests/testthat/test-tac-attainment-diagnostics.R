# TACAttainment(): advised TAC before implementation error, catch from the projection, and the
# cause and binding complex of each shortfall

.DiagMP <- function(f) {
  class(f) <- 'mp'
  f
}

.DiagImp <- function(Hist, value) {
  AllY <- Years(Hist@OM)
  array(value, c(nSim(Hist), length(AllY)),
        dimnames = list(Sim = seq_len(nSim(Hist)), Year = as.character(AllY)))
}

test_that("TACAttainment reports the advised TAC, and the catch taken in the projection", {
  skip_on_cran()
  om <- TwoFleetOM
  nSim(om) <- 2
  pYear(om) <- 3
  set.seed(1)
  Hist <- Simulate(om, silent = TRUE)
  Fl <- FleetNames(Hist)
  Hist@OM@Imp[[1]][[1]]@TAC@Error <- .DiagImp(Hist, 1.2)
  nH <- dim(Hist@Effort)[2]
  Last <- apply(ArraySum(Hist@Landings, Hist@Discards)[, 1, nH, , drop = FALSE], 4, mean)
  TAC <- 0.5 * Last
  Fixed <- eval(bquote(.DiagMP(function(Data) Advice(TAC = .(TAC)))))
  NumLast <- vapply(seq_along(Fl), \(fl) mean(apply(Hist@LandingsAtAge[[1]][, , nH, fl, , drop = FALSE], 1, sum)),
                    numeric(1))
  NumTAC <- eval(bquote(.DiagMP(function(Data) Advice(TAC = .(0.5 * NumLast), TACUnit = 'Number'))))

  MSE <- Project(Hist, MPs = list(Fixed = Fixed, NumTAC = NumTAC), parallel = FALSE, silent = TRUE)
  df <- TACAttainment(MSE)
  expect_true(all(c('Advised', 'Binding') %in% names(df)))

  F1 <- df[df$MP == 'Fixed' & df$Fleet == Fl[1], ]
  F2 <- df[df$MP == 'Fixed' & df$Fleet == Fl[2], ]
  expect_equal(F1$Advised, rep(TAC[1], nrow(F1)), tolerance = 1e-8, ignore_attr = TRUE)
  expect_equal(F1$TAC, 1.2 * F1$Advised, tolerance = 1e-8)
  expect_equal(F2$TAC, F2$Advised, tolerance = 1e-8)
  expect_true(all(df$Binding[df$MP == 'Fixed'] == StockNames(Hist)))

  # Catch is the removals of each fleet in the projection
  Fix <- df[df$MP == 'Fixed', ]
  Rem <- ArraySum(MSE@Landings, MSE@Discards)
  Got <- vapply(seq_len(nrow(Fix)), \(i)
    Rem[Fix$Sim[i], 1, as.character(Fix$Year[i]), Fix$Fleet[i], 'Fixed'], numeric(1))
  expect_equal(Fix$Catch, Got, tolerance = 1e-8)
  expect_equal(Fix$Catch, Fix$TAC, tolerance = 1e-3)

  # a TAC in numbers is caught in numbers
  Num <- df[df$MP == 'NumTAC' & df$Limit == 'None', ]
  expect_gt(nrow(Num), 0)
  expect_equal(Num$Catch, Num$TAC, tolerance = 1e-3)

  expect_false(any(vapply(MSE@Log$warning, \(e) identical(e$name, 'TACRealised'), logical(1))))
})

test_that("a multi-complex choke is reported with its binding complex and is not a solver failure", {
  skip_on_cran()
  om <- MultiStockOM
  nSim(om) <- 2
  pYear(om) <- 3
  set.seed(1)
  Hist <- Simulate(om, silent = TRUE)
  nH <- dim(Hist@Effort)[2]
  C  <- ArraySum(Hist@Landings, Hist@Discards)
  Cx <- names(Hist@OM@Complexes)
  T1 <- 0.3 * apply(C[, 1, nH, , drop = FALSE], c(1, 4), sum)
  T2 <- apply(C[, 2, nH, , drop = FALSE], c(1, 4), sum)
  Choke <- eval(bquote(.DiagMP(function(Data) {
    s <- Data@Misc$Sim
    if (identical(Data@Misc$StockName, .(Cx[1]))) Advice(TAC = .(T1)[s, ]) else Advice(TAC = .(T2)[s, ])
  })))

  for (comp in c(1, 0)) {
    H <- Hist
    for (fl in seq_along(H@OM@Imp[[1]]))
      H@OM@Imp[[1]][[fl]]@TAC@Compliance <- comp
    MSE <- Project(H, MPs = list(Choke = Choke), parallel = FALSE, silent = TRUE)
    df <- TACAttainment(MSE)
    expect_equal(nrow(SummarizeConvergence(MSE, silent = TRUE)), 0, label = paste('compliance', comp))
    expect_false(any(df$Limit == 'Solver'), label = paste('compliance', comp))
    if (comp == 1) {
      # the tight first complex binds every fleet and chokes the second
      Second <- df[df$Stock == Cx[2], ]
      expect_true(all(Second$Limit == 'Choke'))
      expect_true(all(Second$Binding == Cx[1]))
      expect_true(all(df$Limit[df$Stock == Cx[1]] == 'None'))
    } else {
      # zero compliance: fleets fish past the first complex's TAC to the second's
      First <- df[df$Stock == Cx[1], ]
      expect_true(all(First$Catch >= First$TAC))
    }
  }
})

test_that("a TAC shortfall from the fishing mortality cap is reported as MaxF", {
  skip_on_cran()
  om <- TwoFleetOM
  nSim(om) <- 3
  pYear(om) <- 4
  set.seed(1)
  Hist <- Simulate(om, silent = TRUE)
  nH <- dim(Hist@Effort)[2]
  Last <- mean(apply(ArraySum(Hist@Landings, Hist@Discards)[, 1, nH, , drop = FALSE], 1, sum))
  High <- eval(bquote(.DiagMP(function(Data) Advice(TAC = .(5 * Last)))))
  MSE <- Project(Hist, MPs = list(High = High), parallel = FALSE, silent = TRUE)
  df <- TACAttainment(MSE)
  MF <- df[df$Limit == 'MaxF', ]
  expect_gt(nrow(MF), 0)
  Hit <- MaxFEffect(MSE)
  expect_true(all(paste(MF$Sim, MF$Year, MF$Fleet) %in% paste(Hit$Sim, Hit$Year, Hit$Fleet)))
  expect_equal(nrow(SummarizeConvergence(MSE, silent = TRUE)), 0)
  Msg <- vapply(Filter(\(e) identical(e$name, 'TACAttainment'), MSE@Log$warning), `[[`, character(1), 'message')
  expect_match(Msg, "maxF [0-9]+")
})
