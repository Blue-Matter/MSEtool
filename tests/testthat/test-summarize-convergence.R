# SummarizeConvergence()/MPFailureRate() read MSE@Log$warning entries written
# by .LogEffortConvergence() (R/update-tac.R). Real end-to-end runs are kept
# out of the CRAN check budget -- see skip_on_cran() below.

test_that("SummarizeConvergence() and MPFailureRate() are empty for a clean run", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 3
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  mse <- Project(hist, MPs = "CurrentCatch", parallel = FALSE, silent = TRUE)

  tab <- SummarizeConvergence(mse, silent = TRUE)
  expect_s3_class(tab, "data.frame")
  expect_equal(nrow(tab), 0)
  expect_identical(names(tab),
                   c("Sim", "Year", "MP", "Level", "Name", "TAC", "Catch", "PctAchieved"))

  fr <- MPFailureRate(mse)
  expect_identical(fr$MP, names(PPD(mse)))
  expect_true(all(fr$N_Failed == 0))
  expect_true(all(fr$FailureRate == 0))
  expect_equal(fr$N_Total[1], nSim(mse) * length(Years(mse, "Projection")))
})

test_that("SummarizeConvergence() expands structured entries and handles legacy ones", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 3
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  mse <- Project(hist, MPs = "CurrentCatch", parallel = FALSE, silent = TRUE)

  mp <- names(PPD(mse))[1]
  yrs <- Years(mse, "Projection")

  structured <- .NewLogEntry(
    "Effort/TAC solver: did not converge within tolerance.",
    name = "EffortConvergence", sim = 1, year = yrs[1]
  )
  structured$mp    <- mp
  structured$Level <- "Fleet"
  structured$Names <- "Fleet_1"
  structured$TAC   <- 100
  structured$Catch <- 80

  legacy <- .NewLogEntry(
    "Effort/TAC solver: did not converge within tolerance.",
    name = "EffortConvergence", sim = 2, year = yrs[2]
  )
  legacy$mp <- mp

  mse@Log$warning <- c(mse@Log$warning, list(structured, legacy))

  tab <- SummarizeConvergence(mse, silent = TRUE)
  expect_equal(nrow(tab), 2)

  row1 <- tab[tab$Sim == 1, ]
  expect_equal(row1$Level, "Fleet")
  expect_equal(row1$Name, "Fleet_1")
  expect_equal(row1$TAC, 100)
  expect_equal(row1$Catch, 80)
  expect_equal(row1$PctAchieved, 80)

  row2 <- tab[tab$Sim == 2, ]
  expect_true(is.na(row2$Level))
  expect_true(is.na(row2$TAC))

  fr <- MPFailureRate(mse)
  expect_equal(fr$N_Failed[fr$MP == mp], 2)
  expect_equal(fr$FailureRate[fr$MP == mp], 2 / fr$N_Total[fr$MP == mp])
})
