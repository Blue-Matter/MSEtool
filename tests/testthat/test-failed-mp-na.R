FailMP <- function(Data) stop("deliberate failure")
class(FailMP) <- 'mp'

test_that("an MP that fails for every simulation is reported as NA, not 0", {
  skip_on_cran()
  OM <- SingleStockOM
  nSim(OM) <- 3
  Hist <- Simulate(OM, silent = TRUE)
  MSE  <- suppressMessages(
    Project(Hist, MPs = list(FailMP = FailMP, CurrentEffort = CurrentEffort), silent = TRUE)
  )

  expect_true(all(is.na(MSE@SBiomass[, , , 'FailMP'])))
  expect_false(anyNA(MSE@SBiomass[, , , 'CurrentEffort']))

  for (fn in c('SBiomass', 'Landings', 'Removals', 'SB_SB0', 'SB_SBMSY', 'F_FMSY')) {
    df <- get(fn)(MSE)
    v  <- df$Value[df$MP == 'FailMP' & df$Period == 'Projection']
    expect_true(length(v) > 0 && all(is.na(v)), label = fn)
    ok <- df$Value[df$MP == 'CurrentEffort' & df$Period == 'Projection']
    expect_false(anyNA(ok), label = fn)
  }

  for (pm in list(PM_SBSBMSY(MSE), PM_FFMSY(MSE), PM_Landings(MSE))) {
    expect_true(all(is.na(pm@Mean[, 'FailMP'])))
    expect_false(anyNA(pm@Mean[, 'CurrentEffort']))
  }

  # the first stored PPD keeps the historical years when the first MP fails
  expect_equal(names(MSE@PPD), 'CurrentEffort')
  Ref <- Project(Hist, MPs = 'CurrentEffort', silent = TRUE)
  expect_equal(MSE@PPD[[1]][[1]][[1]]@Years, Ref@PPD[[1]][[1]][[1]]@Years)
  expect_s4_class(Hist(MSE), 'hist')
})

test_that("simulations an MP fails for are NA for the whole projection", {
  skip_on_cran()
  TmpDir <- tempfile('failed-mp-')
  dir.create(TmpDir)
  OldWD <- setwd(TmpDir)
  on.exit(setwd(OldWD), add = TRUE)
  OM <- SingleStockOM
  nSim(OM) <- 3
  Hist <- Simulate(OM, silent = TRUE)
  FailYear <- Years(Hist@OM, 'Projection')[4]

  PartFail <- function(Data) {
    if (Data@Misc$Sim == 2 && Data@Misc$AdviceYear >= FailYear) stop("sim 2 failed")
    CurrentEffort(Data)
  }
  class(PartFail) <- 'mp'

  for (chunks in list(NULL, 2)) {
    Hist@OM@Control$ProjectChunks <- chunks
    MSE <- suppressMessages(Project(Hist, MPs = list(PartFail = PartFail), silent = TRUE))

    expect_true(all(is.na(MSE@SBiomass[2, , , ])))
    expect_false(anyNA(MSE@SBiomass[-2, , , ]))
    expect_true(all(is.na(MSE@Landings[2, , , , ])))
    expect_true(all(is.na(MSE@Number[[1]][2, , , , ])))
    expect_true(length(MSE@Log$error) > 0)

    pm <- PM_SBSBMSY(MSE)
    expect_true(is.finite(pm@Mean[1, 1]))
  }
  expect_false(dir.exists('Log'))

  OldOpt <- options(MSEtool.WriteLog = TRUE)
  on.exit(options(OldOpt), add = TRUE)
  suppressMessages(Project(Hist, MPs = list(PartFail = PartFail), silent = TRUE))
  expect_length(list.files('Log', pattern = '^PartFail_.*[.]log$'), 1)

  options(MSEtool.WriteLog = file.path(TmpDir, 'my', 'logs'))
  suppressMessages(Project(Hist, MPs = list(PartFail = PartFail), silent = TRUE))
  expect_length(list.files(file.path(TmpDir, 'my', 'logs'), pattern = '^PartFail_.*[.]log$'), 1)

  options(MSEtool.WriteLog = 1)
  expect_error(suppressMessages(Project(Hist, MPs = list(PartFail = PartFail), silent = TRUE)),
               'MSEtool.WriteLog')
})

test_that("ReduceDims keeps the Sim dimension when only some sims are NA", {
  x <- array(c(1, NA, 1), dim = c(3, 1), dimnames = list(Sim = 1:3, Year = 2027))
  expect_false(MSEtool:::.IdenticalSims(x))
  expect_equal(unname(dim(ReduceDims(x))), c(3L, 1L))
  y <- array(NA_real_, dim = c(3, 1), dimnames = list(Sim = 1:3, Year = 2027))
  expect_true(MSEtool:::.IdenticalSims(y))
})
