# Projection time steps where the OM@maxF cap reduced fishing: one log warning
# per MP, the record kept only where the cap bound, and MaxFEffect().

skip_on_cran()

.MaxFProject <- function(maxF) {
  OM <- TwoFleetOM
  nSim(OM)  <- 2
  pYear(OM) <- 2
  OM@maxF   <- maxF
  Hist <- Simulate(OM, silent = TRUE)
  Closed <- function(Data) {
    A <- Advice(Effort = 0.8)
    A@Closure <- c(1, 0)
    A
  }
  class(Closed) <- 'mp'
  Open <- function(Data) Advice(Effort = 0.8)
  class(Open) <- 'mp'
  MPs <- list(Closed = Closed, Open = Open)
  MSE <- suppressMessages(Project(Hist, MPs = MPs, parallel = FALSE, silent = TRUE))
  list(Hist = Hist, MSE = MSE, MPs = MPs)
}

.MaxFWarnings <- function(MSE) Filter(\(e) identical(e$name, 'MaxF'), MSE@Log$warning)

test_that("effort reduced by the maxF cap is logged once per MP and reported by MaxFEffect()", {
  # closing area 2 concentrates both fleets' F in area 1, above maxF = 3 in one simulation
  R <- .MaxFProject(maxF = 3)
  MSE <- R$MSE
  expect_length(MSE@Log$error, 0)

  W <- .MaxFWarnings(MSE)
  expect_length(W, 1)
  expect_equal(W[[1]]$mp, 'Closed')
  expect_match(W[[1]]$message, 'reached `OM@maxF` (3)', fixed = TRUE)
  expect_match(W[[1]]$message, 'see `MaxFEffect()`', fixed = TRUE)

  Eff <- MaxFEffect(MSE)
  expect_named(Eff, c('Sim', 'MP', 'Year', 'Fleet', 'EffortFraction'))
  expect_gt(nrow(Eff), 0)
  expect_true(all(Eff$MP == 'Closed'))
  expect_true(all(Eff$EffortFraction > 0 & Eff$EffortFraction < 1))

  # the fraction is the realised effort over the advised effort
  LastHist <- R$Hist@Effort[, dim(R$Hist@Effort)[2], ]
  for (i in seq_len(nrow(Eff))) {
    Realised <- MSE@Effort[Eff$Sim[i], as.character(Eff$Year[i]), Eff$Fleet[i], 'Closed']
    Advised  <- 0.8 * LastHist[Eff$Sim[i], Eff$Fleet[i]]
    expect_equal(Realised / Advised, Eff$EffortFraction[i], tolerance = 1e-6)
  }
  # sim-fleet-years not listed fished the advised effort
  Full <- 0.8 * LastHist
  for (sim in 1:2) for (fl in colnames(Full)) {
    Listed <- Eff$Sim == sim & Eff$Fleet == fl
    if (!any(Listed))
      expect_equal(as.numeric(MSE@Effort[sim, , fl, 'Closed']), rep(Full[sim, fl], 2), tolerance = 1e-6)
  }

  expect_null(MSE@Misc$MaxF$Open)
  expect_equal(nrow(MaxFEffect(Subset(MSE, MPs = 'Open'))), 0)
  expect_equal(nrow(MaxFEffect(Subset(MSE, MPs = 'Closed'))), nrow(Eff))
  Sims <- unique(Eff$Sim)
  expect_equal(nrow(MaxFEffect(Subset(MSE, Sims = Sims[1]))), sum(Eff$Sim == Sims[1]))
})

test_that("nothing is logged or stored when maxF does not bind", {
  MSE <- .MaxFProject(maxF = 10)$MSE
  expect_length(.MaxFWarnings(MSE), 0)
  expect_true(all(vapply(MSE@Misc$MaxF, is.null, logical(1))))
  expect_equal(nrow(MaxFEffect(MSE)), 0)
})

test_that("the maxF record is the same when the projection runs in sim chunks", {
  R <- .MaxFProject(maxF = 3)
  Hist <- R$Hist
  Hist@OM@Control$ProjectChunks <- 2
  Chunked <- suppressMessages(Project(Hist, MPs = R$MPs, parallel = FALSE, silent = TRUE))
  expect_equal(Chunked@Misc$MaxF, R$MSE@Misc$MaxF)
  expect_equal(MaxFEffect(Chunked), MaxFEffect(R$MSE))
  expect_equal(vapply(.MaxFWarnings(Chunked), \(e) e$message, character(1)),
               vapply(.MaxFWarnings(R$MSE), \(e) e$message, character(1)))
})
