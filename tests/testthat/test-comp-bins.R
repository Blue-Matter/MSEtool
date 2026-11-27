# Composition data in bins other than the OM classes: OM compositions are
# summed into the data bins for conditioning and projected data.

test_that("OM age classes are summed into age bins, with lumped first and last bins", {
  Map <- MSEtool:::.CompBinMap(0:6, c(1, 2, 4, 6), isAge = TRUE)
  expect_equal(Map, cbind(c(1, 1, 0, 0, 0, 0, 0), c(0, 0, 1, 1, 0, 0, 0),
                          c(0, 0, 0, 0, 1, 1, 0), c(0, 0, 0, 0, 0, 0, 1)))
})

test_that("OM size classes are split across size bins by overlap", {
  # OM 2 cm classes 10-22 (last class 22-24); data 5 cm bins: <15, 15-20, 20+
  Map <- MSEtool:::.CompBinMap(c(10, 12, 14, 16, 18, 20, 22), c(10, 15, 20), isAge = FALSE)
  expect_equal(Map[, 1], c(1, 1, 0.5, 0, 0, 0, 0))
  expect_equal(Map[, 2], c(0, 0, 0.5, 1, 1, 0, 0))
  expect_equal(Map[, 3], c(0, 0, 0, 0, 0, 1, 1))

  # data bins starting above the OM classes lump the smaller classes into the first
  Map <- MSEtool:::.CompBinMap(c(10, 12, 14, 16, 18), c(15, 20), isAge = FALSE)
  expect_equal(Map[, 1], rep(1, 5))
})

test_that(".MapCompBins keeps totals and leaves identical bins unchanged", {
  Classes <- c(10, 12, 14, 16, 18, 20, 22)
  Comp <- array(seq_len(2 * 7 * 3), c(2, 7, 3), list(Sim = 1:2, Class = Classes, Year = 2001:2003))
  expect_identical(MSEtool:::.MapCompBins(Comp, Classes, FALSE), Comp)
  expect_identical(MSEtool:::.MapCompBins(Comp, NULL, FALSE), Comp)

  Out <- MSEtool:::.MapCompBins(Comp, c(10, 15, 20), FALSE)
  expect_equal(dim(Out), c(2, 3, 3))
  expect_equal(dimnames(Out)$Class, c("10", "15", "20"))
  expect_equal(apply(Out, c(1, 3), sum), apply(Comp, c(1, 3), sum))
  expect_equal(Out[1, "10", 1], Comp[1, 1, 1] + Comp[1, 2, 1] + Comp[1, 3, 1] / 2)
})

test_that("compositions in coarser age bins are conditioned and projected in those bins", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  OM <- SingleStockOM
  nSim(OM) <- 2
  Fleet   <- FleetNames(OM)
  Complex <- ComplexNames(Populate(OM, silent = TRUE))
  OM@Obs  <- stats::setNames(list(stats::setNames(list(
    Obs(LandingsAtAge = CompObs(SampleSize = 200))), Fleet)), Complex)
  H <- Simulate(OM, silent = TRUE)
  Sim1 <- H@Data[[1]][[1]]@LandingsAtAge
  Ages <- Sim1@Classes

  # real data in bins: the first age with age 0, single ages to 5, then 6+
  Bins <- c(1, 2, 3, 4, 5, 6)
  Ind  <- pmax(findInterval(Ages, Bins), 1)
  Real <- apply(Sim1@Value, c(1, 2), \(x) tapply(x, Ind, sum))
  Real <- aperm(Real, c(2, 3, 1))
  dimnames(Real) <- list(Year = dimnames(Sim1@Value)[[1]], Fleet = Fleet, Age = Bins)

  OM2 <- OM
  OM2@Obs[[1]][[Fleet]]@LandingsAtAge <- CompObs()
  OM2@Data <- stats::setNames(list(Data(Name = "Real", Years = Years(OM, "H"),
    LandingsAtAge = CompData(Name = Fleet, Value = Real, Classes = Bins, Units = "years"))), Complex)
  H2 <- Simulate(OM2, silent = TRUE)
  Cond <- H2@OM@Obs[[1]][[Fleet]]@LandingsAtAge
  expect_equal(dim(Cond@Shift)[3], length(Bins))

  M <- Project(H2, MPs = "FMSYref", silent = TRUE)
  PPD  <- M@PPD[[1]][[1]][[1]]@LandingsAtAge
  expect_equal(PPD@Classes, Bins)
  Last <- tail(dimnames(PPD@Value)[[1]], 1)
  expect_equal(sum(PPD@Value[Last, Fleet, ]), 200)
})

test_that("age classes outside the OM ages are rejected", {
  Value <- array(1, c(2, 2, 4), list(Year = 2001:2002, Fleet = c("A", "B"), Age = c(1, 2, 3, 4)))
  expect_silent(MSEtool:::.CheckCompAgeBins(Value, 0:10, "x"))

  Odd <- Value
  dimnames(Odd)$Age <- c(1, 2, 3, 4.5)
  expect_error(MSEtool:::.CheckCompAgeBins(Odd, 0:10, "x"), "not in .*Ages@Classes")
  expect_error(MSEtool:::.CheckCompAgeBins(Value, 0:3, "x"), "not currently supported")
})

test_that("predicted tails are added to each year's first and last reported bins", {
  Counts <- rbind(c(NA, 2, 3, NA), c(1, 2, 3, 4), rep(NA, 4))
  Range  <- MSEtool:::.CompTailRange(Counts)
  expect_equal(Range, rbind(c(2, 3), c(1, 4), c(NA, NA)))

  Comp <- array(1:12, c(1, 4, 3))
  Out  <- MSEtool:::.LumpCompTails(Comp, Range)
  expect_equal(Out[1, , 1], c(0, 1 + 2, 3 + 4, 0))
  expect_equal(Out[1, , 2], 5:8)
  expect_equal(Out[1, , 3], 9:12)

  Value <- array(NA_real_, c(3, 1, 4), list(Year = 2001:2003, Fleet = "F", Age = 1:4))
  Value[1, 1, ] <- 1
  Value[2, 1, ] <- c(NA, 1, 1, NA)
  cd <- CompData(Name = "F", Value = Value)
  expect_equal(MSEtool:::.CompLastRange(cd, 1, 4), c(2, 3))
  expect_equal(MSEtool:::.CompLastRange(CompData(Name = "F", Value = Value[3, , , drop = FALSE]), 1, 4),
               c(1, 4))
})

test_that("compositions with accumulated tails are conditioned and projected over the reported bins", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  OM <- SingleStockOM
  nSim(OM) <- 2
  Fleet   <- FleetNames(OM)
  Complex <- ComplexNames(Populate(OM, silent = TRUE))
  OM@Obs  <- stats::setNames(list(stats::setNames(list(
    Obs(LandingsAtAge = CompObs(SampleSize = 200))), Fleet)), Complex)
  H    <- Simulate(OM, silent = TRUE)
  Real <- H@Data[[1]][[1]]@LandingsAtAge
  nAge <- dim(Real@Value)[3]

  # tails accumulated into ages 2 and 10 in every year
  V <- Real@Value
  V[, , 3] <- V[, , 3] + V[, , 1] + V[, , 2]
  V[, , 11] <- V[, , 11] + apply(V[, , 12:nAge, drop = FALSE], c(1, 2), sum)
  V[, , c(1:2, 12:nAge)] <- NA
  Real@Value <- V

  OM2 <- OM
  OM2@Obs[[1]][[Fleet]]@LandingsAtAge <- CompObs()
  OM2@Data <- stats::setNames(list(Data(Name = "Real", Years = Years(OM, "H"), LandingsAtAge = Real)), Complex)
  H2   <- Simulate(OM2, silent = TRUE)
  Cond <- H2@OM@Obs[[1]][[Fleet]]@LandingsAtAge
  Outside <- Cond@Shift[, , c(1:2, 12:nAge)]
  expect_true(any(!is.na(Outside)))
  expect_true(all(Outside[!is.na(Outside)] == 0))
  expect_true(stats::median(Cond@ESS) > 50)

  M <- Project(H2, MPs = "FMSYref", silent = TRUE)
  PPD  <- M@PPD[[1]][[1]][[1]]@LandingsAtAge
  Last <- tail(dimnames(PPD@Value)[[1]], 1)
  expect_true(all(is.na(PPD@Value[Last, Fleet, c(1:2, 12:nAge)])))
  expect_equal(sum(PPD@Value[Last, Fleet, ], na.rm = TRUE), 200)
})

test_that("Populate rejects age compositions with classes that are not OM ages", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  OM <- SingleStockOM
  Fleet <- FleetNames(OM)
  Value <- array(1, c(length(Years(OM, "H")), 1, 2),
                 list(Year = Years(OM, "H"), Fleet = Fleet, Age = c(1, 2.5)))
  OM@Data <- list(Data(Name = "Real", Years = Years(OM, "H"),
                       LandingsAtAge = CompData(Name = Fleet, Value = Value, Classes = c(1, 2.5))))
  expect_error(Populate(OM, silent = TRUE), "not currently supported")
})

test_that("SS3 compositions are imported in the SS3 data bins", {
  skip_on_cran()
  Simple <- "C:/Users/Admin/Documents/GitHub/SS3-CaseStudies/SS_Simple"
  skip_if_not(dir.exists(Simple))
  RepList <- ImportSSReport(Simple, silent = TRUE)
  D <- ImportSSData(RepList, silent = TRUE)
  expect_equal(D@SurveyAtAge[[1]]@Classes, RepList[[1]]$agebins)
  expect_equal(D@LandingsAtSize[[1]]@Classes[[1]], RepList[[1]]$lbins)

  # every observed length bin with SS3 data is populated (no gaps from mapping)
  Obs <- RepList[[1]]$lendbase
  Filled <- apply(D@LandingsAtSize[[1]]@Value[, 1, ], 2, \(x) any(x > 0, na.rm = TRUE))
  expect_true(all(Filled[match(unique(Obs$Bin[Obs$Fleet == 1 & Obs$Obs > 0 & Obs$Sex == 1]),
                               RepList[[1]]$lbins)]))
})
