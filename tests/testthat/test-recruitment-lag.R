# Recruitment lag (`.RecLag()`): recruits spawned in step y enter at y + lag.

.SimNoRefs <- function(OM)
  Simulate(OM, silent = TRUE,
           control = SimControl(MSYRefs = FALSE, RefPoints = FALSE, MGT = FALSE,
                                DynamicUnfished = FALSE))

# drops age classes below `keep` from every Age/Class dimension
.DropYoungAges <- function(x, keep) {
  if (isS4(x)) {
    for (sl in methods::slotNames(x))
      methods::slot(x, sl, check = FALSE) <- .DropYoungAges(methods::slot(x, sl), keep)
    return(x)
  }
  if (is.list(x) && !is.data.frame(x)) {
    for (i in seq_along(x)) if (!is.null(x[[i]])) x[[i]] <- .DropYoungAges(x[[i]], keep)
    return(x)
  }
  if (is.array(x)) {
    for (d in which(names(dimnames(x)) %in% c('Age', 'Class')))
      if (dim(x)[d] == length(keep)) x <- abind::asub(x, which(keep), d, drop = FALSE)
  }
  x
}

test_that("projection starts with recruits when the recruitment lag is 2 or more", {
  skip_on_cran()
  OM <- SingleStockOM
  OM@nSim <- 3
  Ages <- OM@Stock[[1]]@Ages
  Ages@MinAge  <- 2
  Ages@Classes <- 2:20
  OM@Stock[[1]]@Ages <- Ages

  Hist <- .SimNoRefs(OM)
  expect_equal(unname(.RecLag(Hist@OM@Stock)), 2)
  MSE <- Project(Hist, MPs = 'CurrentEffort', silent = TRUE)

  Rec <- MSE@Number[[1]][, 1, 1:5, , 1, drop = FALSE] |> apply(c(1, 3), sum)
  expect_true(all(Rec > 0))
  HistRec <- apply(Hist@Number[[1]][, 1, , , drop = FALSE], c(1, 3), sum)
  expect_true(all(Rec[, 1] > 0.2 * apply(HistRec, 1, min)))
})

test_that("seasonal projection starts with recruits when the youngest age is one year", {
  skip_on_cran()
  OM <- SeasonalSpatialOM
  OM@nSim <- 2
  Stock <- OM@Stock[[1]]
  Keep  <- Stock@Ages@Classes >= 1
  Stock <- .DropYoungAges(Stock, Keep)
  Stock@Ages@MinAge  <- 12
  Stock@Ages@Classes <- OM@Stock[[1]]@Ages@Classes[Keep]
  OM@Stock[[1]] <- Stock
  OM@Fleet <- .DropYoungAges(OM@Fleet, Keep)

  Hist <- .SimNoRefs(OM)
  expect_equal(unname(.RecLag(Hist@OM@Stock)), OM@Seasons)
  MSE <- Project(Hist, MPs = 'CurrentEffort', silent = TRUE)

  Seasons <- OM@Seasons
  nHist   <- dim(Hist@Number[[1]])[3]
  ProjRec <- apply(MSE@Number[[1]][, 1, seq_len(Seasons), , 1, drop = FALSE], c(1, 3), sum)
  HistRec <- apply(Hist@Number[[1]][, 1, nHist - Seasons + seq_len(Seasons), , drop = FALSE], c(1, 3), sum)
  expect_true(all(ProjRec > 0))
  expect_true(all(rowSums(ProjRec) > 0.2 * rowSums(HistRec)))
})

test_that("SRR@SpawnLag sets the initial recruits that are filled", {
  skip_on_cran()
  OM <- SingleStockOM
  OM@nSim <- 3
  OM@Stock[[1]]@SRR@SpawnLag <- 2

  Hist <- .SimNoRefs(OM)
  expect_equal(.CalcRecruitmentAgeIndex(Hist, 1), 2)
  Rec <- apply(Hist@Number[[1]][, 1, 1:4, , drop = FALSE], c(1, 3), sum)
  expect_true(all(Rec > 0))
})

test_that("a SpawnLag below the youngest age fills only the steps the lag skips", {
  skip_on_cran()
  OM <- SingleStockOM
  OM@nSim <- 3
  Ages <- OM@Stock[[1]]@Ages
  Ages@MinAge  <- 3
  Ages@Classes <- 3:20
  OM@Stock[[1]]@Ages <- Ages
  OM@Stock[[1]]@SRR@SpawnLag <- 1

  Hist <- .OM2Hist(OM, silent = TRUE)
  Hist@Unfished@Equilibrium <- CalcUnfished_Equilibrium(Hist@OM, silent = TRUE)
  Init <- .CalcDynamicInitial(Hist)
  expect_true(all(Init@Number[[1]][, 1, 1, ] > 0))
  expect_true(all(Init@Number[[1]][, 1, 2:3, ] == 0))

  Rec <- apply(.SimNoRefs(OM)@Number[[1]][, 1, 1:4, , drop = FALSE], c(1, 3), sum)
  expect_true(all(Rec > 0))
})
