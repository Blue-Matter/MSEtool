test_that(".CatchUnitType classifies generic, mass and count units", {
  expect_equal(.CatchUnitType(c("Biomass", "Number", "t", "KG", "1000 lb", "n", "1000 n", NA)),
               c("Biomass", "Number", "Biomass", "Biomass", "Biomass", "Number", "Number", "Biomass"))
  expect_null(.CatchUnitType(NULL))
  expect_error(.CatchUnitType(c("t", "tonnes")), "tonnes")
})

test_that(".ResolveUnits validates catch units even when length matches nFleet", {
  cd <- CatchData(Name = c("A", "B"), Units = c("t", "kg"))
  expect_identical(.ResolveUnits(cd, 2)@Units, c("t", "kg"))
  cd@Units <- c("t", "bananas")
  expect_error(.ResolveUnits(cd, 2), "bananas")
})

test_that("CheckCatch flags differing mass units between Landings and Discards", {
  mk <- function(u) CatchData(Name = "F1", Units = u,
                              Value = array(1, c(1, 1), list(Year = 2000, Fleet = "F1")))
  d <- new("data")
  d@Years <- 2000; d@YearLH <- 2000; d@Misc$Sim <- 1
  d@Landings <- mk("t"); d@Discards <- mk("kg")
  expect_message(CheckCatch(d), "not in the same units")
  d@Discards <- mk("Biomass")
  expect_no_message(CheckCatch(d))
  d@Discards <- mk("Number")
  expect_message(CheckCatch(d), "not in the same units")
})

test_that("physical catch units give the same simulated data as generic units", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 3

  run <- function(units) {
    for (fl in seq_along(om@Obs[[1]]))
      om@Obs[[1]][[fl]]@Landings@Units <- units
    set.seed(1)
    hist <- Simulate(om, silent = TRUE)
    mse  <- Project(hist, MPs = "CurrentCatch", parallel = FALSE, silent = TRUE)
    list(hist = hist, mse = mse)
  }
  generic <- run("Biomass")
  physical <- run("t")

  hL <- purrr::map(physical$hist@Data, \(d) d[[1]]@Landings)
  expect_true(all(purrr::map_lgl(hL, \(l) all(l@Units == "t"))))
  expect_equal(purrr::map(hL, \(l) l@Value),
               purrr::map(generic$hist@Data, \(d) d[[1]]@Landings@Value))
  expect_equal(physical$mse@Landings, generic$mse@Landings)
})

test_that("Advice stores physical TAC units as Biomass or Number", {
  expect_identical(TACUnit(Advice(TAC = 100, TACUnit = "t")), "Biomass")
  expect_identical(TACUnit(Advice(TAC = 100, TACUnit = c("kg", "1000 n"))), c("Biomass", "Number"))
  expect_error(Advice(TAC = 100, TACUnit = "tonnes"), "TACUnit")
  adv <- Advice(TAC = 100)
  TACUnit(adv) <- "n"
  expect_identical(TACUnit(adv), "Number")
})
