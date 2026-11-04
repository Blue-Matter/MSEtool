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
