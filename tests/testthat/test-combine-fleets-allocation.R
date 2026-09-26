
test_that(".CombineFleetsMap follows the final fleet order and keeps fleets not combined", {
  map <- .CombineFleetsMap(c("A", "B", "C", "D"), list(X = c("C", "D"), Y = c("B", "A")))
  expect_equal(names(map), c("Y", "X"))
  expect_equal(map$Y, c(B = 2L, A = 1L))
  expect_equal(map$X, c(C = 3L, D = 4L))

  x <- matrix(c(0.1, 0.2, 0.3, 0.4,
                0.4, 0.3, 0.2, 0.1), 2, byrow = TRUE)
  out <- .CombineFleetShares(x, map)
  expect_equal(dimnames(out)$Fleet, c("Y", "X"))
  expect_equal(unname(out), matrix(c(0.3, 0.7, 0.7, 0.3), 2, byrow = TRUE))

  partial <- .CombineFleetsMap(c("A", "B", "C"), list(AB = c("A", "B")))
  out <- .CombineFleetShares(matrix(c(0.2, 0.3, 0.5), 1), partial)
  expect_equal(dimnames(out)$Fleet, c("AB", "C"))
  expect_equal(unname(out[1, ]), c(0.5, 0.5))
})

test_that("SeasonalAllocation is FleetAllocation-weighted, or must be identical without it", {
  data(TwoFleetOM, envir = environment())
  om <- TwoFleetOM
  map <- .CombineFleetsMap(c("A", "B", "C"), list(AB = c("A", "B")))
  SA <- array(c(0.1, 0.2, 0.3, 0.4,
                0.4, 0.3, 0.2, 0.1,
                0.25, 0.25, 0.25, 0.25), c(1, 4, 3),
              dimnames = list(Sim = 1, Season = 1:4, Fleet = c("A", "B", "C")))
  om@SeasonalAllocation <- list(SA)
  om@FleetAllocation <- list(matrix(c(0.2, 0.6, 0.2,
                                      0.5, 0.0, 0.5), 2, byrow = TRUE))

  out <- .CombineFleetsAllocation(om, map)
  sa <- out@SeasonalAllocation[[1]]
  expect_equal(dim(sa), c(2, 4, 2), ignore_attr = TRUE)
  expect_equal(dimnames(sa)$Fleet, c("AB", "C"))
  expect_equal(sa[1, , "AB"], (0.2 * SA[1, , "A"] + 0.6 * SA[1, , "B"]) / 0.8, ignore_attr = TRUE)
  expect_equal(sa[2, , "AB"], SA[1, , "A"], ignore_attr = TRUE)
  expect_equal(sa[, , "C"], rbind(SA[1, , "C"], SA[1, , "C"]), ignore_attr = TRUE)
  expect_equal(unname(out@FleetAllocation[[1]]), matrix(c(0.8, 0.2, 0.5, 0.5), 2, byrow = TRUE))

  om@FleetAllocation <- list()
  expect_error(.CombineFleetsAllocation(om, map), "FleetAllocation` is not set")

  SA[1, , "B"] <- SA[1, , "A"]
  om@SeasonalAllocation <- list(SA)
  out <- .CombineFleetsAllocation(om, map)
  expect_equal(unname(out@SeasonalAllocation[[1]][1, , "AB"]), unname(SA[1, , "A"]))
})

test_that("InterimAdvice TAC rows are merged per timestep; NA totals and other fleets are unchanged", {
  data(TwoFleetOM, envir = environment())
  om <- TwoFleetOM
  om@InterimAdvice <- data.frame(
    Year  = c(2025, 2025, 2025, 2026, 2027, 2027, 2027.25, 2027.25, 2027),
    Fleet = c("A", "C", "B", NA, "A", "B", "A", "B", "C"),
    Type  = "TAC",
    Mean  = c(300, 50, 100, 900, 10, 1, 20, 2, 5),
    CV    = c(0.2, 0.1, NA, 0.3, NA, NA, NA, NA, NA),
    Max   = c(400, NA, NA, NA, NA, NA, NA, NA, NA)
  )
  IA <- .CombineFleetsInterimAdvice(om, list(AB = c("A", "B")))@InterimAdvice

  expect_equal(IA$Fleet, c("AB", "C", NA, "AB", "AB", "C"))
  expect_equal(IA$Year, c(2025, 2025, 2026, 2027, 2027.25, 2027))
  expect_equal(IA$Mean, c(400, 50, 900, 11, 22, 5))
  expect_equal(IA$CV[1], sqrt((0.2 * 300)^2) / 400)
  expect_equal(IA$Max[1], 400 + 100)
  expect_true(all(is.na(IA$CV[4:5])) && all(is.na(IA$Max[4:5])))
  expect_equal(IA[2:3, ], om@InterimAdvice[c(2, 4), ], ignore_attr = TRUE)
})

test_that("InterimAdvice rows that cannot be combined are an error", {
  data(TwoFleetOM, envir = environment())
  om <- TwoFleetOM
  grp <- list(AB = c("A", "B"))
  merge <- function(df) { om@InterimAdvice <- df; .CombineFleetsInterimAdvice(om, grp)@InterimAdvice }

  expect_error(merge(data.frame(Year = 2025, Fleet = c("A", NA), Type = "TAC", Mean = 10)),
               "same timesteps")
  expect_error(merge(data.frame(Year = c(2025, 2025, 2025.5), Fleet = c("A", "B", "B"), Type = "TAC", Mean = 10)),
               "same timesteps")
  expect_error(merge(data.frame(Year = 2025, Fleet = c("A", "B"), Type = "TAC", Mean = 10,
                                TACType = c("Removals", "Landings"))),
               "TACType")
  expect_error(merge(data.frame(Year = 2025, Fleet = c("A", "B"), Type = "TAC", Mean = 10,
                                TACUnit = c("t", "n"))),
               "TACUnit")
  expect_error(merge(data.frame(Year = 2025, Fleet = c("A", "B"), Type = "Effort", Mean = 1)),
               "absolute Effort")
  expect_error(merge(data.frame(Year = 2025, Fleet = c("A", "B"), Type = "Effort", Mean = c(1, 1.2),
                                EffType = "Rel")),
               "same `Mean`")

  IA <- merge(data.frame(Year = 2025, Fleet = c("A", "B"), Type = c("TAC", "TAC", "Effort", "Effort"),
                         Mean = c(10, 20, 1.1, 1.1), TACUnit = c("t", "Biomass", NA, NA),
                         EffType = c(NA, NA, "Rel", "Rel")))
  expect_equal(IA$Fleet, c("AB", "AB"))
  expect_equal(IA$Type, c("TAC", "Effort"))
  expect_equal(IA$Mean, c(30, 1.1))
})

.CombineAllocationOM <- function(Seasons = 1) {
  data(TwoFleetOM, envir = environment())
  om <- TwoFleetOM
  om@nSim <- 3
  om@pYear <- 4
  om@Seasons <- Seasons
  om@MPStartYear <- CurrentYear(om) + 3
  om <- Populate(om, silent = TRUE)
  # avoid timesteps where every fleet has zero effort
  for (f in 1:2) {
    e <- om@Fleet[[1]][[f]]@Effort@Effort
    e[e == 0] <- 1e-6
    om@Fleet[[1]][[f]]@Effort@Effort <- e
  }
  om
}

.InterimRemovals <- function(om) {
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  mse <- Project(hist, MPs = "CurrentCatch", parallel = FALSE, silent = TRUE)
  yp <- Years(hist, "P")
  interim <- as.character(yp[floor(yp) < om@MPStartYear])
  rem <- ArraySum(mse@Landings, mse@Discards)
  apply(rem[, , interim, , 1, drop = FALSE], c(1, 3, 4), sum)
}

test_that("per-fleet InterimAdvice projects as the summed TAC for the combined fleet", {
  skip_on_cran()
  om <- .CombineAllocationOM()
  yr <- CurrentYear(om) + 1:2
  om@InterimAdvice <- data.frame(Year = rep(yr, each = 2), Fleet = FleetNames(om),
                                 Type = "TAC", Mean = c(300, 700))
  omc <- CombineFleets(om, silent = TRUE)
  expect_equal(omc@InterimAdvice$Fleet, c("Combined", "Combined"))

  rem <- .InterimRemovals(omc)
  expect_equal(unname(rem[, , "Combined"]), matrix(1000, 3, 2), tolerance = 1e-3)
})

test_that("seasonal OM: combined fleet reproduces the per-fleet seasonal interim catch", {
  skip_on_cran()
  om <- .CombineAllocationOM(Seasons = 4)
  fl <- FleetNames(om)
  SA <- array(c(0.1, 0.2, 0.3, 0.4,
                0.4, 0.3, 0.2, 0.1), c(1, 4, 2),
              dimnames = list(Sim = 1, Season = 1:4, Fleet = fl))
  om@SeasonalAllocation <- list(SA)
  om@FleetAllocation <- list(matrix(c(0.3, 0.7), 1, dimnames = list(Sim = 1, Fleet = fl)))
  names(om@SeasonalAllocation) <- names(om@FleetAllocation) <- names(om@Complexes)
  om@InterimAdvice <- data.frame(Year = rep(CurrentYear(om) + 1:2, each = 2), Fleet = fl,
                                 Type = "TAC", Mean = c(300, 700))

  rem <- .InterimRemovals(CombineFleets(om, silent = TRUE))
  expected <- rep(300 * SA[1, , 1] + 700 * SA[1, , 2], 2)
  for (sim in 1:3)
    expect_equal(unname(rem[sim, , "Combined"]), unname(expected), tolerance = 1e-3)
})
