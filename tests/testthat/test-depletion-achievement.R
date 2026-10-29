# Depletion achievement check (R/plot-depletion.R) must use the same basis as
# the Final-depletion optimiser: per SPFrom-linked group, B0 = total group
# biomass, SB0 = source stocks' spawning biomass.

skip_on_cran()

.terminal_ratio <- function(Hist, slt, stocks) {
  nSim  <- Hist@OM@nSim
  num   <- ExtendSims(slot(Hist, slt), nSim)
  yrs   <- dimnames(num)$Year
  den   <- MSEtool:::.AlignDenomYears(slot(Hist@Unfished@Equilibrium, slt), yrs) |>
    ExtendSims(nSim)
  nY <- length(yrs)
  rowSums(num[, stocks, nY, drop = FALSE]) / rowSums(den[, stocks, nY, drop = FALSE])
}

.depletion_warnings <- function(Hist) {
  nms <- purrr::map_chr(Hist@Log$warning %||% list(), \(w) w$name %||% '')
  nms[startsWith(nms, 'Depletion - ')]
}

test_that("HermOM logs no false depletion warning and hits combined B/B0", {
  H <- Simulate(HermOM, nSim = 4, silent = TRUE)
  expect_length(.depletion_warnings(H), 0)

  target   <- as.numeric(H@OM@Stock[[1]]@Depletion@Final)
  achieved <- .terminal_ratio(H, 'Biomass', 1:2)
  expect_true(all(abs(log(achieved / target)) <= log(1.1)))

  # the female stock alone is not on the target basis
  alone <- .terminal_ratio(H, 'Biomass', 1)
  expect_false(all(abs(log(alone / target)) <= log(1.1)))

  units <- MSEtool:::.DepletionUnits(H@OM)
  final <- purrr::keep(units, \(u) u$Type == 'Final')
  expect_length(final, 1)
  expect_equal(final[[1]]$Stocks, 1:2)
  expect_true(final[[1]]$Linked)

  # a missed group target is reported against the group
  H2 <- H
  H2@OM@Stock[[1]]@Depletion@Final[] <- target * c(1, 2, 1, 2)
  H2@Log$warning <- NULL
  H2 <- MSEtool:::.LogDepletionAchievement(H2)
  w <- .depletion_warnings(H2)
  expect_length(w, 1)
  expect_match(w, 'total B/B0', fixed = TRUE)
  sims <- purrr::map(H2@Log$warning, 'sim') |> unlist()
  expect_equal(sort(sims), c(2, 4))
})

test_that("SB0-referenced linked group is checked on the source stock's SB/SB0", {
  om <- HermOM
  om@Stock[[1]]@Depletion@Reference <- 'SB0'
  om@Stock[[2]]@Depletion@Reference <- 'B0'
  H <- Simulate(om, nSim = 4, silent = TRUE)
  expect_length(.depletion_warnings(H), 0)

  target   <- as.numeric(H@OM@Stock[[1]]@Depletion@Final)
  achieved <- .terminal_ratio(H, 'SBiomass', 1)
  expect_true(all(abs(log(achieved / target)) <= log(1.1)))

  units <- MSEtool:::.DepletionUnits(H@OM)
  final <- purrr::keep(units, \(u) u$Type == 'Final')[[1]]
  expect_equal(final$Stocks, 1L)
  expect_equal(final$Ref, 'SB0')

  res <- MSEtool:::.ComputeDepletionAchievement(H, units, tol = 0.1)
  expect_equal(res$Achieved, unname(achieved), tolerance = 1e-8)
  expect_true(all(res$Pass))
})

test_that("Single-stock OM with an unreachable Final target still warns", {
  om <- SingleStockOM
  om@nSim <- 3
  om@Stock[[1]]@Depletion@Final <- c(0.4, 3, 0.5)
  H <- suppressWarnings(Simulate(om, silent = TRUE))

  w <- .depletion_warnings(H)
  expect_length(w, 1)
  expect_equal(w, paste0('Depletion - ', StockNames(H@OM)[1]))

  res <- MSEtool:::.ComputeDepletionAchievement(H, MSEtool:::.DepletionUnits(H@OM), tol = 0.1)
  expect_equal(res$Pass, c(TRUE, FALSE, TRUE))
  expect_true(res$Unreachable[2])
  expect_equal(res$Achieved, unname(.terminal_ratio(H, 'Biomass', 1)), tolerance = 1e-8)
})
