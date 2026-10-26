test_that("combining fleets ignores units of fleets with no data", {
  om <- TwoFleetOM
  Fleets <- FleetNames(om)
  Landings <- CatchData(Name = Fleets)
  Landings@Value <- array(c(1, 2, NA, NA), c(2, 2),
                          dimnames = list(Year = 2001:2002, Fleet = Fleets))
  Landings@Units <- setNames(c("Biomass", NA), Fleets)
  Data <- Data(Name = "x")
  Data@Landings <- Landings
  om@Data <- list(Data)
  FleetList <- list(Comb = rev(Fleets))

  expect_no_error(.ValidateFleetListUnits(om, FleetList))
  out <- .CombineFleetsDataSum(om, FleetList, type = "Landings", silent = TRUE)@Data[[1]]@Landings
  expect_equal(out@Name, "Comb")
  expect_equal(as.numeric(out@Value), c(1, 2))
  expect_equal(unname(out@Units), "Biomass")

  om@Data[[1]]@Landings@Units[] <- c("Biomass", "Number")
  expect_error(.ValidateFleetListUnits(om, FleetList), "different")
})

test_that("AddFleet() adds an NA data column for the new fleet", {
  om <- TwoFleetOM
  Fleets <- FleetNames(om)
  Landings <- CatchData(Name = Fleets)
  Landings@Value <- array(c(1, 2, 3, 4), c(2, 2),
                          dimnames = list(Year = 2001:2002, Fleet = Fleets))
  Landings@CV <- Landings@Value * 0 + 0.1
  Landings@Units <- setNames(c("Biomass", "Biomass"), Fleets)
  Data <- Data(Name = "x")
  Data@Years <- 2001:2002
  Data@Landings <- Landings
  om@Data <- list(Data)

  om <- AddFleet(om, "New")
  om <- AddFleet(om, "New2")
  out <- om@Data[[1]]@Landings
  expect_equal(out@Name, c(Fleets, "New", "New2"))
  expect_equal(colnames(out@Value), c(Fleets, "New", "New2"))
  expect_true(all(is.na(out@Value[, c("New", "New2")])))
  expect_equal(unname(out@Units), c("Biomass", "Biomass", NA, NA))
  expect_equal(dim(out@CV), dim(out@Value))
})
