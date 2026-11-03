# Fleet Selectivity, Retention, and DiscardMortality schedules supplied as
# numeric vectors at-age or at-length.

skip_on_cran()

data(SingleStockOM, envir = environment())

.VecOM <- function() {
  OM <- SingleStockOM
  nSim(OM) <- 2
  pYear(OM) <- 2
  OM
}

.VecHist0 <- Simulate(.VecOM(), silent = TRUE)
.VecLC    <- .VecHist0@OM@Stock[[1]]@Length@Classes
.VecAges  <- .VecHist0@OM@Stock[[1]]@Ages@Classes
.VecYear  <- dimnames(.VecHist0@OM@Stock[[1]]@Length@ALK)$Year[1]

.VecFleet <- function(slotName, value) {
  OM <- .VecOM()
  Fl <- OM@Fleet[[1]][[1]]
  slot(Fl, slotName) <- value
  OM@Fleet[[1]][[1]] <- Fl
  Simulate(OM, silent = TRUE)@OM@Fleet[[1]][[1]]
}

.AtLengthArray <- function(x)
  array(x, dim = c(1, length(.VecLC), 1),
        dimnames = list(Sim = 1, Class = .VecLC, Year = .VecYear))

.AtAgeArray <- function(x)
  array(x, dim = c(1, length(.VecAges), 1),
        dimnames = list(Sim = 1, Age = .VecAges, Year = .VecYear))

.Schedules <- list(
  Selectivity = list(
    fn = Selectivity,
    len = pmin(1, .VecLC / 60),
    age = pmin(1, .VecAges / 4)),
  Retention = list(
    fn = Retention,
    len = as.numeric(.VecLC >= 75),
    age = as.numeric(.VecAges >= 5)),
  DiscardMortality = list(
    fn = DiscardMortality,
    len = ifelse(.VecLC >= 75, 0.2, 0.6),
    age = ifelse(.VecAges >= 5, 0.2, 0.6))
)

for (nm in names(.Schedules)) {
  sc <- .Schedules[[nm]]

  test_that(paste(nm, "at-length vector matches the equivalent array"), {
    Vec <- .VecFleet(nm, sc$fn(MeanAtLength = sc$len, Classes = .VecLC))
    Arr <- .VecFleet(nm, sc$fn(MeanAtLength = .AtLengthArray(sc$len), Classes = .VecLC))
    expect_equal(slot(Vec, nm)@MeanAtAge, slot(Arr, nm)@MeanAtAge)
    expect_equal(slot(Vec, nm)@MeanAtLength, slot(Arr, nm)@MeanAtLength)
  })

  test_that(paste(nm, "at-age vector matches the equivalent array"), {
    Vec <- .VecFleet(nm, sc$fn(MeanAtAge = sc$age))
    Arr <- .VecFleet(nm, sc$fn(MeanAtAge = .AtAgeArray(sc$age)))
    expect_equal(slot(Vec, nm)@MeanAtAge, slot(Arr, nm)@MeanAtAge)
  })

  test_that(paste(nm, "vectors of the wrong length are rejected"), {
    expect_error(.VecFleet(nm, sc$fn(MeanAtLength = sc$len[1:3], Classes = .VecLC)),
                 'must have length matching `Classes`')
    expect_error(.VecFleet(nm, sc$fn(MeanAtAge = sc$age[1:3])), 'nAge')
  })
}
