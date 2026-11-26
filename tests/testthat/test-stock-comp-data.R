# Stock-specific and survey composition data: data/obs slots holding a list
# named by the complex and/or stocks, Joint sampling across stocks, survey
# age/size compositions, and SS3 import of both.

skip_on_cran()

.JointComplexOM <- function(Joint = TRUE, SampleSize = 100) {
  data(ComplexOM, envir = environment())
  OM <- ComplexOM
  nSim(OM) <- 2
  Stocks <- StockNames(OM)
  Obs <- Obs(LandingsAtAge = stats::setNames(list(CompObs(SampleSize = SampleSize, Joint = Joint),
                                                  CompObs(SampleSize = SampleSize, Joint = Joint)),
                                             Stocks))
  OM@Obs <- list(StockComplex = stats::setNames(list(Obs), FleetNames(OM)))
  OM
}

.RowTotals <- function(cd, Fleet) rowSums(cd@Value[, Fleet, , drop = FALSE], na.rm = TRUE)

test_that("a list of stock CompObs generates stock-specific compositions drawn as one sample", {
  OM <- .JointComplexOM(Joint = TRUE)
  H  <- Simulate(OM, silent = TRUE)
  Comp <- H@Data[[1]][[1]]@LandingsAtAge
  Stocks <- StockNames(OM)
  Fleet  <- FleetNames(OM)

  expect_type(Comp, "list")
  expect_named(Comp, Stocks)
  expect_true(all(purrr::map_lgl(Comp, \(cd) isTRUE(cd@Joint[Fleet]))))
  Total <- .RowTotals(Comp[[1]], Fleet) + .RowTotals(Comp[[2]], Fleet)
  expect_true(all(Total %in% c(0, 100)))
  expect_true(any(Total == 100))
  expect_true(any(.RowTotals(Comp[[1]], Fleet)[Total > 0] != 100))
  # stocks keep their own age classes
  expect_equal(Comp[[1]]@Classes, as.numeric(OM@Stock[[1]]@Ages@Classes))
  expect_equal(Comp[[2]]@Classes, as.numeric(OM@Stock[[2]]@Ages@Classes))
})

test_that("stock CompObs without Joint draw a sample for each stock", {
  OM <- .JointComplexOM(Joint = FALSE, SampleSize = 50)
  H  <- Simulate(OM, silent = TRUE)
  Comp  <- H@Data[[1]][[1]]@LandingsAtAge
  Fleet <- FleetNames(OM)
  expect_true(all(.RowTotals(Comp[[1]], Fleet) %in% c(0, 50)))
  expect_true(all(.RowTotals(Comp[[2]], Fleet) %in% c(0, 50)))
  expect_true(any(.RowTotals(Comp[[1]], Fleet) == 50))
  expect_length(Comp[[1]]@Joint, 0)
})

test_that("stock-specific compositions condition a list of CompObs with shared sample size", {
  OM <- .JointComplexOM(Joint = TRUE)
  H0 <- Simulate(OM, silent = TRUE)
  Real <- H0@Data[[1]][[1]]

  OM2 <- OM
  OM2@Obs[[1]][[1]]@LandingsAtAge <- CompObs()
  OM2@Data <- list(StockComplex = Data(Name = "Real", Years = Years(OM, "H"),
                                        LandingsAtAge = Real@LandingsAtAge))
  H <- Simulate(OM2, silent = TRUE)
  Cond <- H@OM@Obs[[1]][[1]]@LandingsAtAge
  expect_type(Cond, "list")
  expect_named(Cond, StockNames(OM), ignore.order = TRUE)
  expect_true(all(purrr::map_lgl(Cond, \(o) isTRUE(o@Joint))))
  expect_identical(Cond[[1]]@SampleSize, Cond[[2]]@SampleSize)
  expect_equal(max(Cond[[1]]@SampleSize), 100)
  expect_equal(dim(Cond[[1]]@Shift)[3], length(OM@Stock[[1]]@Ages@Classes))
  expect_equal(dim(Cond[[2]]@Shift)[3], length(OM@Stock[[2]]@Ages@Classes))

  M <- Project(H, MPs = "FMSYref", silent = TRUE)
  PPD   <- M@PPD[[1]][[1]][[1]]@LandingsAtAge
  Fleet <- FleetNames(OM)
  Last  <- tail(dimnames(PPD[[1]]@Value)[[1]], 1)
  expect_equal(sum(PPD[[1]]@Value[Last, Fleet, ], PPD[[2]]@Value[Last, Fleet, ], na.rm = TRUE), 100)
})

test_that("Landings() returns stock-specific compositions with byStock", {
  OM <- .JointComplexOM(Joint = TRUE)
  H  <- Simulate(OM, silent = TRUE)
  D  <- H@Data[[1]][[1]]
  df <- Landings(D, byAge = TRUE, byStock = TRUE)
  expect_true("Stock" %in% names(df))
  expect_setequal(unique(df$Stock), StockNames(OM))

  Same <- D
  Same@LandingsAtAge[[2]] <- Same@LandingsAtAge[[1]]
  Combined <- Landings(Same, byAge = TRUE)
  One      <- Landings(Same, byAge = TRUE, byStock = TRUE)
  expect_equal(sum(Combined$Value, na.rm = TRUE), sum(One$Value, na.rm = TRUE))
  # stocks with different age classes are combined by age
  All <- Landings(D, byAge = TRUE)
  expect_equal(sum(All$Value, na.rm = TRUE), sum(df$Value, na.rm = TRUE))
  expect_setequal(unique(All$Age), unique(df$Age))
})

test_that("element names of stock-specific data and obs are validated", {
  OM <- .JointComplexOM()
  names(OM@Obs[[1]][[1]]@LandingsAtAge)[1] <- "Not a stock"
  expect_error(Populate(OM, silent = TRUE), "not unique or not in the OM")
})

test_that("stock-specific compositions round-trip through the data CSV", {
  OM <- .JointComplexOM(Joint = TRUE)
  H  <- Simulate(OM, silent = TRUE)
  D  <- H@Data[[1]][[1]]
  f  <- tempfile(fileext = ".csv")
  WriteDataCSV(D, f, silent = TRUE)
  csv <- utils::read.csv(f)
  expect_true(all(c("Stock", "Joint") %in% names(csv)))
  D2 <- ReadDataCSV(f, silent = TRUE)
  expect_type(D2@LandingsAtAge, "list")
  expect_named(D2@LandingsAtAge, names(D@LandingsAtAge))
  Fleet <- FleetNames(OM)
  for (st in names(D@LandingsAtAge)) {
    expect_true(isTRUE(D2@LandingsAtAge[[st]]@Joint[Fleet]))
    expect_equal(unname(.RowTotals(D2@LandingsAtAge[[st]], Fleet)),
                 unname(.RowTotals(D@LandingsAtAge[[st]], Fleet)))
  }
})

test_that("survey age and size compositions are simulated, conditioned, and projected", {
  data(SingleStockOM, envir = environment())
  OM <- SingleStockOM
  nSim(OM) <- 2
  Fleet <- FleetNames(OM)
  Complex <- ComplexNames(Populate(OM, silent = TRUE))
  OM@Obs <- stats::setNames(list(stats::setNames(list(
    Obs(),
    Obs(Survey = IndicesObs(CV = 0.2, Selectivity = "Biomass"),
        SurveyAtAge = CompObs(SampleSize = 100),
        SurveyAtSize = CompObs(SampleSize = 150))
  ), c(Fleet, "Acoustic"))), Complex)

  H <- Simulate(OM, silent = TRUE)
  D <- H@Data[[1]][[1]]
  expect_equal(dimnames(D@SurveyAtAge@Value)[[2]], "Acoustic")
  expect_true(all(.RowTotals(D@SurveyAtAge, "Acoustic") == 100))
  expect_true(all(.RowTotals(D@SurveyAtSize, "Acoustic") == 150))
  expect_true(nrow(Survey(D, byAge = TRUE)) > 0)

  # predicted survey composition is the selected numbers-at-age
  True <- MSEtool:::.TrueComp(H, "SurveyAtAge", 1, "Acoustic", Years(OM, "H"), 1)
  N    <- apply(H@Number[[1]][1, , , , drop = FALSE], c(2, 3), sum)
  expect_equal(unname(True[1, , ]), unname(N), tolerance = 1e-8)

  OM2 <- OM
  OM2@Obs[[1]][["Acoustic"]]@SurveyAtAge <- CompObs()
  OM2@Data <- stats::setNames(list(Data(Name = "Real", Years = Years(OM, "H"),
                                        SurveyAtAge = D@SurveyAtAge)), Complex)
  H2 <- Simulate(OM2, silent = TRUE)
  Cond <- H2@OM@Obs[[1]][["Acoustic"]]@SurveyAtAge
  expect_equal(unname(Cond@SampleSize[1, 1]), 100)
  expect_false(is.null(Cond@Shift))

  M <- Project(H2, MPs = "FMSYref", silent = TRUE)
  PPD  <- M@PPD[[1]][[1]][[1]]@SurveyAtAge
  Last <- tail(dimnames(PPD@Value)[[1]], 1)
  expect_equal(sum(PPD@Value[Last, "Acoustic", ]), 100)
})

test_that("combining stock elements sums counts by fleet", {
  v <- array(1, c(2, 1, 3), list(Year = 2000:2001, Fleet = "F", Age = 1:3))
  x <- list(A = CompData(Name = "F", Value = v, Classes = 1:3),
            B = CompData(Name = "F", Value = v * 2, Classes = 1:3, Joint = c(F = TRUE)))
  Out <- MSEtool:::.CombineCompElements(x)
  expect_s4_class(Out, "compdata")
  expect_equal(unname(Out@Value[, "F", ]), matrix(3, 2, 3))
  expect_length(Out@Joint, 0)
})

test_that("SS3 survey and sex-specific compositions are imported", {
  hoki    <- 'G:/My Drive/1_PROJECTS/openMSE_dev/CaseStudies/hoki_1.00_SS3'
  dogfish <- 'G:/My Drive/1_PROJECTS/openMSE_dev/CaseStudies/dogfish_A0'
  skip_if_not(dir.exists(hoki) && dir.exists(dogfish))

  D <- ImportSSData(ImportSSReport(hoki, silent = TRUE), silent = TRUE)
  Value <- D@SurveyAtAge@Value[, "Acoustic_Survey", ]
  expect_equal(sum(rowSums(Value, na.rm = TRUE) > 0), 24)
  expect_equal(sum(colSums(Value, na.rm = TRUE) > 0), 14)
  expect_true(sum(rowSums(D@LandingsAtAge@Value, na.rm = TRUE) > 0) > 0)

  D <- ImportSSData(ImportSSReport(dogfish, silent = TRUE), silent = TRUE)
  expect_named(D@LandingsAtSize, c("Female", "Male"))
  expect_true(isTRUE(D@LandingsAtSize$Female@Joint["SYN"]))
  expect_true(isTRUE(D@LandingsAtSize$Male@Joint["SYN"]))
})

.StockCatchOM <- function() {
  data(ComplexOM, envir = environment())
  OM <- ComplexOM
  nSim(OM) <- 2
  St <- StockNames(OM)
  OM@Obs <- list(StockComplex = stats::setNames(list(
    Obs(Landings = stats::setNames(list(CatchObs(CV = 0.1), CatchObs(CV = 0.1)), St)),
    Obs(Survey = stats::setNames(list(IndicesObs(CV = 0.2, Selectivity = "Biomass")), St[1]))
  ), c(FleetNames(OM), "SpawnSurvey")))
  OM
}

test_that("stock-specific catch and indices are simulated, conditioned, and projected", {
  OM <- .StockCatchOM()
  St <- StockNames(OM)
  H  <- Simulate(OM, silent = TRUE)
  D  <- H@Data[[1]][[1]]
  expect_named(D@Landings, St)
  expect_named(D@Survey, St[1])
  # accessors combine the stocks
  expect_equal(unname(Landings(D)@Value[, 1]),
               unname(D@Landings[[1]]@Value[, 1] + D@Landings[[2]]@Value[, 1]))
  expect_identical(Landings(D, byStock = TRUE), D@Landings)
  expect_equal(Survey(D)@Name, "SpawnSurvey")

  # the index follows the biomass of its stock
  B1 <- apply(H@Biomass[1, 1, , drop = FALSE], 3, sum)
  I1 <- D@Survey[[1]]@Value[, 1]
  expect_gt(stats::cor(I1, B1[seq_along(I1)], use = "complete.obs"), 0.9)

  OM2 <- OM
  OM2@Data <- list(StockComplex = Data(Name = "Real", Years = Years(OM, "H"),
                                       Landings = D@Landings, Survey = D@Survey))
  H2 <- Simulate(OM2, silent = TRUE)
  expect_named(H2@OM@Obs[[1]][[1]]@Landings, St, ignore.order = TRUE)
  expect_named(H2@OM@Obs[[1]][["SpawnSurvey"]]@Survey, St[1])

  M <- Project(H2, MPs = c("FMSYref", "CurrentCatch", "IndexRate"), silent = TRUE)
  expect_null(M@Log$error)
  PPD <- M@PPD[[1]][[1]][[1]]
  expect_named(PPD@Landings, St)
  expect_true(all(is.finite(tail(PPD@Survey[[1]]@Value[, 1], 3))))
})

test_that("catch held for both the complex and a stock in the same year is rejected", {
  OM <- .StockCatchOM()
  H  <- Simulate(OM, silent = TRUE)
  D  <- H@Data[[1]][[1]]
  Both <- c(D@Landings, list(StockComplex = Landings(D)))
  OM@Data <- list(StockComplex = Data(Name = "Real", Years = Years(OM, "H"), Landings = Both))
  expect_error(Populate(OM, silent = TRUE), "both the complex and its stocks")
})

test_that("stock-specific catch round-trips through the data CSV", {
  OM <- .StockCatchOM()
  D  <- Simulate(OM, silent = TRUE)@Data[[1]][[1]]
  f  <- tempfile(fileext = ".csv")
  WriteDataCSV(D, f, silent = TRUE)
  D2 <- ReadDataCSV(f, silent = TRUE)
  expect_named(D2@Landings, names(D@Landings))
  expect_equal(unname(D2@Landings[[2]]@Value[, 1]), unname(D@Landings[[2]]@Value[, 1]))
  expect_named(D2@Survey, names(D@Survey))
})
