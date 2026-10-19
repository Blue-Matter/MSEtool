test_that("PPD() keeps each MP's own Data@Misc", {
  skip_on_cran()

  OM <- SingleStockOM
  OM@nSim <- 2
  Hist <- Simulate(OM, silent = TRUE)

  MPa <- function(Data) {
    a <- Advice()
    a@Misc <- list(Cycles = c(Data@Misc$Cycles, Data@Misc$AdviceYear), x = 0.5)
    a
  }
  MPb <- function(Data) {
    a <- Advice()
    a@Misc <- list(x = 0.7)
    a
  }
  class(MPa) <- class(MPb) <- 'mp'
  attr(MPa, 'Interval') <- attr(MPb, 'Interval') <- 3

  MSE <- Project(Hist, MPs = list(MPa = MPa, MPb = MPb), silent = TRUE)
  ppd <- PPD(MSE)

  for (sim in 1:2) {
    expect_equal(ppd$MPa[[sim]][[1]]@Misc, MSE@PPD$MPa[[sim]][[1]]@Misc)
    expect_equal(ppd$MPb[[sim]][[1]]@Misc, list(x = 0.7))
  }
  expect_gt(length(ppd$MPa[[1]][[1]]@Misc$Cycles), 1)
  expect_equal(Years(ppd$MPb[[1]][[1]]), Years(ppd$MPa[[1]][[1]]))
})

test_that("PPD includes observed data for the final projection timestep", {
  skip_on_cran()

  OM <- SingleStockOM
  OM@nSim <- 2
  MSE <- Project(Simulate(OM, silent = TRUE),
                 MPs = c('CurrentCatch', 'AverageCatch'), silent = TRUE)
  LastYear <- max(Years(MSE, 'P'))
  ppd <- PPD(MSE)

  for (mp in names(ppd)) {
    for (sim in 1:2) {
      Data <- ppd[[mp]][[sim]][[1]]
      expect_equal(max(Years(Data)), LastYear)
      for (sl in c('Landings', 'LandingsAtAge')) {
        Val <- slot(Data, sl)@Value
        expect_equal(as.numeric(utils::tail(dimnames(Val)$Year, 1)), LastYear)
        expect_equal(unname(dim(Val)[1]), length(Years(Data)))
      }
    }
  }
  L <- Value(Landings(ppd$AverageCatch[[1]][[1]]))
  expect_false(identical(L[nrow(L), ], L[nrow(L) - 1, ]))
})

test_that("PPD() drops forward-filled final-year observations from older MSE objects", {
  skip_on_cran()

  OM <- SingleStockOM
  OM@nSim <- 2
  MSE <- Project(Simulate(OM, silent = TRUE),
                 MPs = c('CurrentCatch', 'AverageCatch'), silent = TRUE)
  LastYear <- max(Years(MSE, 'P'))

  # mimic an MSE object where the final year was padded, not generated
  Old <- MSE
  Old@PPD$AverageCatch <- purrr::map(Old@PPD$AverageCatch, \(DataList)
    purrr::map(DataList, \(Data) {
      Data@Years <- Data@Years[Data@Years < LastYear]
      for (sl in c('Landings', 'LandingsAtAge')) {
        obj <- slot(Data, sl)
        n <- dim(obj@Value)[1]
        if (length(dim(obj@Value)) == 2) obj@Value[n, ] <- obj@Value[n - 1, ]
        else obj@Value[n, , ] <- obj@Value[n - 1, , ]
        slot(Data, sl) <- obj
      }
      Data
    })
  )

  Data <- PPD(Old)$AverageCatch[[1]][[1]]
  expect_equal(max(Years(Data)), LastYear - 1)
  for (sl in c('Landings', 'LandingsAtAge'))
    expect_equal(unname(dim(slot(Data, sl)@Value)[1]), length(Years(Data)))
  TAC <- Data@Advice@TAC
  expect_equal(as.numeric(utils::tail(dimnames(TAC)$Year, 1)), LastYear)

  Padded <- Old@PPD$AverageCatch[[1]][[1]]@Landings@Value
  expect_equal(Data(Old)$AverageCatch[[1]][[1]]@Landings@Value,
               Padded[-nrow(Padded), , drop = FALSE])

  # objects with a generated final year are unchanged
  expect_identical(.DropPaddedPPDYears(MSE@PPD), MSE@PPD)
})
