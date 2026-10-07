.PrepProjForChunks <- function(hist) {
  YearsHist <- Years(hist@OM, "Historical")
  YearsProj <- Years(hist@OM, "Projection")
  hist |>
    .CheckFleetAllocation() |>
    .CheckSeasonalAllocation() |>
    .CheckEffortAllocation() |>
    .PrepHistMisc() |>
    .CheckInterimAdvice() |>
    .ExtendHist(Years = c(YearsHist, YearsProj), silent = TRUE) |>
    .CalcFisheryDynamics(Years = utils::tail(YearsHist, 1), clone = 1)
}

# paths of arrays, vectors and lists that still carry the full sim count
.SimSizedPaths <- function(x, nSim, path = "") {
  if (is.null(x) || is.function(x) || is.environment(x)) return(character())
  if (isS4(x))
    return(unlist(lapply(methods::slotNames(x), \(s)
      .SimSizedPaths(methods::slot(x, s), nSim, paste0(path, "@", s)))))
  if (is.data.frame(x)) return(character())
  if (is.list(x)) {
    out <- if (length(x) == nSim) path else character()
    return(c(out, unlist(lapply(seq_along(x), \(i)
      .SimSizedPaths(x[[i]], nSim, paste0(path, "[[", i, "]]"))))))
  }
  if (any(dim(x) == nSim) || (is.null(dim(x)) && length(x) == nSim)) path else character()
}

.ChunkedProject <- function(hist, MPs, K) {
  hist@OM@Control$ProjectChunks <- K
  out <- Project(hist, MPs = MPs, silent = TRUE)
  out@OM@Control$ProjectChunks <- NULL
  out
}

.ExpectSameProjection <- function(Chunked, Ref) {
  for (sl in setdiff(methods::slotNames("timeseries"), "Misc"))
    expect_identical(methods::slot(Chunked, sl), methods::slot(Ref, sl), label = sl)
  expect_identical(Chunked@PPD, Ref@PPD)
  expect_identical(Chunked@Misc, Ref@Misc)
  expect_identical(Chunked@Hist, Ref@Hist)
}

test_that(".SplitSims() gives contiguous chunks differing in size by at most one", {
  Chunks <- .SplitSims(10, 3)
  expect_identical(unlist(Chunks), 1:10)
  expect_identical(lengths(Chunks), c(4L, 3L, 3L))
  expect_length(.SplitSims(5, 9), 5)
  expect_identical(.SplitSims(5, 1), list(1:5))
})

test_that(".SubsetSim() subsets sim-named vectors when nSim is given", {
  Eff <- stats::setNames(c(10, 20, 30, 40, 50), 1:5)
  expect_identical(unname(.SubsetSim(Eff, 3:4, nSim = 5)), c(30, 40))
  expect_identical(names(.SubsetSim(Eff, 3:4, nSim = 5)), c("1", "2"))
  expect_identical(.SubsetSim(Eff, 3:4), Eff)
})

test_that(".ChunkProj() leaves nothing sized to the full sim count", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 7
  set.seed(1)
  Proj <- Simulate(om, silent = TRUE) |> .PrepProjForChunks()

  Chunk <- .ChunkProj(Proj, 3:5)
  expect_identical(.SimSizedPaths(Chunk, 7), character())
  expect_identical(Chunk@OM@Misc$SimIDs, 3:5)
  expect_identical(names(Chunk@Data), c("1", "2", "3"))
  expect_identical(.ChunkProj(Proj, 1:7), Proj)
})

test_that("projecting in sim chunks matches an unchunked projection", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 7
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)

  StochCatch <- function(Data) {
    Adv <- CurrentCatch(Data)
    Adv@TAC <- Adv@TAC * stats::rlnorm(1, 0, 0.3)
    Adv
  }
  class(StochCatch) <- "mp"
  MPs <- list(CC = "CurrentCatch", IR = "IndexRate", SC = StochCatch)

  Ref <- Project(hist, MPs = MPs, silent = TRUE)
  .ExpectSameProjection(.ChunkedProject(hist, MPs, 3), Ref)
  .ExpectSameProjection(.ChunkedProject(hist, MPs, 7), Ref)
})

test_that("projecting in sim chunks matches an unchunked projection with interim advice", {
  skip_on_cran()
  data(TwoFleetOM, envir = environment())
  om <- TwoFleetOM
  om@nSim <- 7
  om@pYear <- 6
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  FirstYear <- floor(Years(hist, "P")[1])
  hist@OM@MPStartYear <- FirstYear + 2
  hist@OM@InterimAdvice <- data.frame(Year = rep(FirstYear + 0:1, each = 2),
                                      Fleet = FleetNames(hist), Type = "TAC",
                                      Mean = c(300, 150, 300, 150), CV = c(0.2, 0.3, 0.2, 0.3))

  Ref <- Project(hist, MPs = "CurrentCatch", silent = TRUE)
  .ExpectSameProjection(.ChunkedProject(hist, "CurrentCatch", 3), Ref)
})

test_that("sim chunks report MP failures against the same simulations", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 7
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)

  FailFirst <- function(Data) {
    if (Data@Misc$Sim <= 3) stop("deliberate failure")
    CurrentCatch(Data)
  }
  class(FailFirst) <- "mp"
  MPs <- list(CC = "CurrentCatch", FailFirst = FailFirst)

  OldDir <- setwd(tempdir())
  on.exit(setwd(OldDir), add = TRUE)
  Ref    <- suppressMessages(Project(hist, MPs = MPs, silent = TRUE))
  Chunked <- suppressMessages(.ChunkedProject(hist, MPs, 3))

  expect_identical(Chunked@Landings, Ref@Landings)
  FailedSims <- \(m) sort(unique(unlist(lapply(m@Log$error, `[[`, "sim"))))
  expect_identical(FailedSims(Chunked), 1:3)
  expect_identical(FailedSims(Chunked), FailedSims(Ref))
})

test_that("multi-stock targeting uses each sim's own StockTargeting covariance", {
  skip_on_cran()
  data(MultiStockOM, envir = environment())
  om <- MultiStockOM
  om@nSim <- 5
  om@pYear <- 5
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  expect_gt(dim(hist@OM@StockTargeting@Covariance)[1], 1)

  Slice <- .SliceSim(hist, 4, .DynamicsProbeSlots)
  expect_identical(unname(Slice@OM@StockTargeting@Covariance[1, , , ]),
                   unname(hist@OM@StockTargeting@Covariance[4, , , ]))

  Ref <- Project(hist, MPs = "CurrentCatch", silent = TRUE)
  .ExpectSameProjection(.ChunkedProject(hist, "CurrentCatch", 2), Ref)
})
