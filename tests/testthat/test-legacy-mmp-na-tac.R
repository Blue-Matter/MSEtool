# Legacy multi-fleet projections (ProjectMOM) with NA TACs: a fleet with an NA
# TAC in every sim is managed by effort; NA TACs in some sims of a TAC fleet
# fall back to the last TAC (GitHub issue #161).

.LegacyMMP <- function(rec1, rec2) {
  f <- function(x, DataList, ...) lapply(seq_along(DataList), function(p) list(rec1(x), rec2(x)))
  class(f) <- "MMP"
  f
}
.RecTAC <- function(v) function(x) { Rec <- new("Rec"); Rec@TAC <- v; Rec }
.RecEffort <- function(x) { Rec <- new("Rec"); Rec@Effort <- 1; Rec }
.RecNA <- function(x) new("Rec")
.RecPartNA <- function(x) { Rec <- new("Rec"); Rec@TAC <- if (x %% 2 == 0) NA_real_ else 1; Rec }

test_that("ProjectMOM runs when a fleet's TAC is NA in all or some sims", {
  skip_on_cran()
  MPs <- list(
    NA_TAC = .LegacyMMP(.RecNA, .RecTAC(1)),
    E_TAC = .LegacyMMP(.RecEffort, .RecTAC(1)),
    Part_TAC = .LegacyMMP(.RecPartNA, .RecTAC(1)),
    TAC_E = .LegacyMMP(.RecTAC(1), .RecEffort),
    NA_TAC0 = .LegacyMMP(.RecNA, .RecTAC(0))
  )
  for (nm in names(MPs)) assign(nm, MPs[[nm]], envir = globalenv())
  withr::defer(rm(list = names(MPs), envir = globalenv()))

  MOM <- Albacore_TwoFleet
  MOM@nsim <- 4
  MOM@proyears <- 5
  MOM@interval <- 1
  Hist <- SimulateMOM(MOM, parallel = FALSE, silent = TRUE)
  MSE <- ProjectMOM(Hist, names(MPs), parallel = FALSE, silent = TRUE)
  nsim <- MOM@nsim

  expect_false(anyNA(MSE@SB_SBMSY))
  expect_false(anyNA(MSE@B))
  expect_false(anyNA(MSE@Catch))
  expect_false(anyNA(MSE@Removals))
  expect_false(anyNA(MSE@Effort))

  # NA-TAC fleet follows status-quo effort, identical to an Effort = 1 MP
  expect_equal(MSE@Catch[, 1, 1, 1, ], MSE@Catch[, 1, 1, 2, ])
  expect_equal(MSE@Effort[, 1, 1, 1, ], MSE@Effort[, 1, 1, 2, ])
  expect_equal(MSE@Catch[, 1, 2, 1, ], MSE@Catch[, 1, 2, 2, ])
  expect_equal(MSE@Catch[, 1, 2, 1, ], matrix(1, nsim, MOM@proyears), tolerance = 1e-3)

  # NA sims fall back to the last historical catch; others take the TAC
  NAsim <- which(seq_len(nsim) %% 2 == 0)
  LastCatch <- apply(Hist[[1]][[1]]@TSdata$Removals[NAsim, MOM@Fleets[[1]][[1]]@nyears, , drop = FALSE], 1, sum)
  expect_true(all(LastCatch > 1))
  expect_equal(MSE@Catch[NAsim, 1, 1, 3, ], matrix(LastCatch, length(NAsim), MOM@proyears), tolerance = 1e-3)
  expect_equal(MSE@Catch[-NAsim, 1, 1, 3, ], matrix(1, nsim - length(NAsim), MOM@proyears), tolerance = 1e-3)

  # TAC on one fleet and effort on the other
  expect_equal(MSE@Catch[, 1, 1, 4, ], matrix(1, nsim, MOM@proyears), tolerance = 1e-3)
  expect_equal(MSE@Effort[, 1, 2, 4, 1], rep(1, nsim))

  # zero TAC alongside an NA-TAC fleet
  expect_true(all(MSE@Catch[, 1, 2, 5, ] < 1e-3))
  expect_equal(MSE@Effort[, 1, 1, 5, 1], rep(1, nsim))
})

test_that("ProjectMOM: NA TACs after an effort year fall back to last year's catch", {
  skip_on_cran()
  .FirstYear <- function(Data) max(Data@Year) == Data@LHYear
  Switch_TAC <- function(x, DataList, ...) {
    first <- .FirstYear(DataList[[1]][[1]])
    lapply(seq_along(DataList), function(p) list(if (first) .RecEffort(x) else .RecPartNA(x), .RecTAC(1)(x)))
  }
  class(Switch_TAC) <- "MMP"
  assign("Switch_TAC", Switch_TAC, envir = globalenv())
  withr::defer(rm("Switch_TAC", envir = globalenv()))

  MOM <- Albacore_TwoFleet
  MOM@nsim <- 4
  MOM@proyears <- 5
  MOM@interval <- 1
  Hist <- SimulateMOM(MOM, parallel = FALSE, silent = TRUE)
  MSE <- ProjectMOM(Hist, "Switch_TAC", parallel = FALSE, silent = TRUE)
  nsim <- MOM@nsim

  expect_false(anyNA(MSE@B))
  expect_false(anyNA(MSE@Removals))
  NAsim <- which(seq_len(nsim) %% 2 == 0)
  Rem <- MSE@Removals[, 1, 1, 1, ]
  expect_equal(Rem[NAsim, -1], matrix(Rem[NAsim, 1], length(NAsim), MOM@proyears - 1), tolerance = 1e-3)
  expect_equal(Rem[-NAsim, -1], matrix(1, nsim - length(NAsim), MOM@proyears - 1), tolerance = 1e-3)
})

test_that("Project: NA TACs after an effort year fall back to last year's catch", {
  skip_on_cran()
  Switch_TAC1 <- function(x, Data, ...) {
    if (max(Data@Year) == Data@LHYear) return(.RecEffort(x))
    .RecPartNA(x)
  }
  class(Switch_TAC1) <- "MP"
  assign("Switch_TAC1", Switch_TAC1, envir = globalenv())
  withr::defer(rm("Switch_TAC1", envir = globalenv()))

  OM <- testOM
  OM@nsim <- 4
  OM@proyears <- 5
  OM@interval <- 1
  OM@TACSD <- c(0, 0)
  OM@TACFrac <- c(1, 1)
  MSE <- runMSE(OM, MPs = "Switch_TAC1", parallel = FALSE, silent = TRUE)

  expect_false(anyNA(MSE@SB_SBMSY))
  expect_false(anyNA(MSE@Removals))
  NAsim <- which(seq_len(OM@nsim) %% 2 == 0)
  Rem <- unname(MSE@Removals[, 1, ])
  # target is last year's catch, capped where unreachable
  expect_true(all(Rem[NAsim, 2] > 1 & Rem[NAsim, 2] <= Rem[NAsim, 1] * 1.001))
  expect_equal(Rem[NAsim, 3:OM@proyears], matrix(Rem[NAsim, 2], length(NAsim), OM@proyears - 2), tolerance = 1e-3)
  expect_equal(Rem[-NAsim, -1], matrix(1, OM@nsim - length(NAsim), OM@proyears - 1), tolerance = 1e-3)
})
