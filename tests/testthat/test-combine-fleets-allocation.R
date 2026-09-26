
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

test_that("HistoricalWeight is FleetAllocation-weighted where SeasonalAllocation is derived", {
  data(TwoFleetOM, envir = environment())
  om <- TwoFleetOM
  map <- .CombineFleetsMap(c("A", "B", "C"), list(AB = c("A", "B")))
  om@HistoricalWeight <- list(c(A = 1, B = 0.5, 0.2))
  om@FleetAllocation <- list(matrix(c(0.2, 0.6, 0.2), 1))

  hw <- .CombineFleetsAllocation(om, map)@HistoricalWeight[[1]]
  expect_equal(hw, c(AB = (0.2 * 1 + 0.6 * 0.5) / 0.8, C = 0.2))

  om@FleetAllocation <- list()
  expect_error(.CombineFleetsAllocation(om, map), "HistoricalWeight` for fleets")

  om@HistoricalWeight <- list(c(A = 0.5, B = 0.5, C = 1))
  expect_equal(.CombineFleetsAllocation(om, map)@HistoricalWeight[[1]], c(AB = 0.5, C = 1))

  # unused when SeasonalAllocation is set
  om@HistoricalWeight <- list(c(A = 1, B = 0.5))
  om@SeasonalAllocation <- list(array(0.25, c(1, 4, 3)))
  expect_equal(.CombineFleetsAllocation(om, map)@HistoricalWeight[[1]], c(A = 1, B = 0.5))
})

test_that("fleets not combined keep their own Imp", {
  data(TwoFleetOM, envir = environment())
  fl <- FleetNames(TwoFleetOM)
  om <- AddFleet(TwoFleetOM, "Third", Imp = Imp(Name = "ImpThird")) |> Populate(silent = TRUE)
  om@Imp[[1]][[1]]@Name <- "ImpA"
  om@Imp[[1]][[2]]@Name <- "ImpB"

  omc <- CombineFleets(om, list(AB = fl), silent = TRUE)
  expect_equal(names(omc@Imp[[1]]), c("AB", "Third"))
  expect_equal(purrr::map_chr(omc@Imp[[1]], \(x) x@Name), c(AB = "ImpA", Third = "ImpThird"))
})

test_that("Closure is open where any combined fleet is open", {
  closure <- function(x) {
    fleet <- Fleet()
    fleet@Closure <- array(x, c(1, 2, 2), dimnames = list(Sim = 1, Year = c(2000, 2010), Area = 1:2))
    fleet
  }
  out <- .CombineFleetsClosure(list(closure(c(1, 0, 1, 1)), closure(c(1, 1, 1, 1))))
  expect_true(all(out == 1))
  out <- .CombineFleetsClosure(list(closure(c(1, 0, 1, 1)), closure(c(1, 0, 0, 1))))
  expect_equal(out[1, "2010", ], c(`1` = 0, `2` = 1))
  expect_equal(out[1, "2000", ], c(`1` = 1, `2` = 1))
})

.CombineSettingsOM <- function() {
  data(TwoFleetOM, envir = environment())
  om <- TwoFleetOM
  om@nSim <- 3
  om@pYear <- 5
  om
}

.ZeroEffortFix <- function(om) {
  om <- Populate(om, silent = TRUE)
  # avoid timesteps where every fleet has zero effort
  for (f in seq_along(om@Fleet[[1]])) {
    e <- om@Fleet[[1]][[f]]@Effort@Effort
    e[e == 0] <- 1e-6
    om@Fleet[[1]][[f]]@Effort@Effort <- e
  }
  om
}

test_that("fleet effort settings are F-weighted, or an error when they cannot be combined", {
  om <- .CombineSettingsOM()
  om@Fleet[[1]][[1]]@Effort@Targeting <- 1.5
  om@Fleet[[1]][[2]]@Effort@Targeting <- 0.5
  om <- Populate(om, silent = TRUE)
  om@Fleet[[1]][[1]]@Effort@Units <- "days"
  src <- om@Fleet[[1]]
  HistYears <- Years(om, "Historical")
  w <- .CombineFleetsWeights(src, HistYears)
  expect_equal(Reduce(`+`, w), array(1, dim(w[[1]])), ignore_attr = TRUE)

  nf <- CombineFleets(om, silent = TRUE)@Fleet[[1]][[1]]
  expect_equal(nf@Effort@Units, "days")
  expect_equal(nf@Effort@Targeting, 1.5 * w[[1]] + 0.5 * w[[2]], ignore_attr = TRUE)
  expect_true(all(is.na(nf@Effort@Distribution)))

  D <- array(c(0.2, 0.8), c(1, 1, 2), dimnames = list(Sim = 1, Year = HistYears[1], Area = 1:2))
  om2 <- om
  om2@Fleet[[1]][[1]]@Effort@Distribution <- D
  expect_true(all(is.na(CombineFleets(om2, silent = TRUE)@Fleet[[1]][[1]]@Effort@Distribution)))
  om2@Fleet[[1]][[2]]@Effort@Distribution <- D[, , 2:1, drop = FALSE] |> `dimnames<-`(dimnames(D))
  dist <- CombineFleets(om2, silent = TRUE)@Fleet[[1]][[1]]@Effort@Distribution
  expect_equal(dist[, , 1], 0.2 * w[[1]] + 0.8 * w[[2]], ignore_attr = TRUE)
  expect_equal(dist[, , 1] + dist[, , 2], array(1, dim(w[[1]])), ignore_attr = TRUE)

  om3 <- om
  om3@Fleet[[1]][[2]]@Effort@Mode <- "Biomass"
  expect_error(CombineFleets(om3, silent = TRUE), "`Mode` differs")
  om3 <- om
  om3@Fleet[[1]][[2]]@Effort@Theta <- array(2, c(1, 1), dimnames = list(Sim = 1, Year = HistYears[1]))
  expect_error(CombineFleets(om3, silent = TRUE), "bag-limit")
})

test_that("projected catchability change is the F-weighted mean of the fleets' change", {
  skip_on_cran()
  om <- .ZeroEffortFix(.CombineSettingsOM())
  om@Fleet[[1]][[1]]@Catchability@qInc <- 2
  cy <- CurrentYear(om)
  apicalF <- purrr::map_dbl(om@Fleet[[1]], \(fleet)
    fleet@Effort@Effort[1, as.character(cy)] * fleet@Catchability@Efficiency[1, as.character(cy)])
  w1 <- apicalF[1] / sum(apicalF)

  omc <- CombineFleets(om, silent = TRUE)
  q <- omc@Fleet[[1]][[1]]@Catchability@Efficiency
  expect_equal(unname(q[1, as.character(cy + 1:5)] / q[1, as.character(cy)]),
               unname(w1 * 1.02^(1:5) + (1 - w1)), tolerance = 1e-8)

  set.seed(1)
  hist <- Simulate(omc, silent = TRUE)
  expect_no_error(Project(hist, MPs = "CurrentEffort", parallel = FALSE, silent = TRUE))
})

.CombineDataOM <- function(...) {
  data(TwoFleetOM, envir = environment())
  om <- TwoFleetOM
  d <- Data()
  for (sl in names(list(...))) slot(d, sl) <- list(...)[[sl]]
  om@Data <- list(d)
  om
}

.YearFleet <- function(x, fleets = c("A", "B", "C")) {
  matrix(x, 3, dimnames = list(Year = 2001:2003, Fleet = fleets))
}

test_that("catch and effort data are summed with NA where no fleet has data; CV and Ref combined", {
  om <- .CombineDataOM(
    Landings = CatchData(Name = c("A", "B", "C"),
                         Value = .YearFleet(c(NA, 10, 10, NA, NA, 20, 1, 2, 3)),
                         CV = .YearFleet(c(NA, 0.1, 0.1, NA, NA, 0.2, 0.1, 0.1, 0.1)),
                         Units = rep("t", 3),
                         Ref = .YearFleet(c(5, 5, 5, 1, 1, 1, 2, 2, 2))),
    Effort = EffortData(Name = c("A", "B", "C"), Value = .YearFleet(c(1, 2, 3, 3, 2, 1, 1, 1, 1)),
                        Units = c("days", "days", "hours"))
  )
  d <- .CombineFleetsData(om, list(AB = c("A", "B")), silent = TRUE)@Data[[1]]

  expect_equal(d@Landings@Name, c("AB", "C"))
  expect_equal(unname(d@Landings@Value[, "AB"]), c(NA, 10, 30))
  expect_equal(unname(d@Landings@CV[, 1]), c(NA, 0.1, sqrt(1^2 + 4^2) / 30))
  expect_equal(colnames(d@Landings@CV), c("AB", "C"))
  expect_equal(unname(d@Landings@Ref[, 1]), c(6, 6, 6))
  expect_equal(.SumCV(matrix(0, 1, 2), matrix(c(0.2, 0.4), 1)), 0.3)
  expect_equal(d@Landings@Units, c("t", "t"))
  expect_equal(unname(d@Landings@Value[, "C"]), c(1, 2, 3))
  expect_equal(d@Effort@Name, c("AB", "C"))
  expect_equal(unname(d@Effort@Value[, "AB"]), c(4, 4, 4))
  expect_equal(d@Effort@Units, c("days", "hours"))

  om@Data[[1]]@Effort@Units <- c("days", "hours", "hours")
  expect_message(d <- .CombineFleetsData(om, list(AB = c("A", "B")))@Data[[1]], "different units")
  expect_equal(d@Effort@Name, "C")
})

test_that("data held by only some fleets of a group is dropped", {
  om <- .CombineDataOM(
    Landings = CatchData(Name = c("A", "C"), Value = .YearFleet(1:6, c("A", "C")), Units = c("t", "t")),
    LandingsAtAge = CompData(Name = c("B", "C"),
                             Value = array(1:12, c(3, 2, 2), dimnames = list(Year = 2001:2003, Fleet = c("B", "C"), Age = 1:2)))
  )
  msgs <- capture_messages(d <- .CombineFleetsData(om, list(AB = c("A", "B")))@Data[[1]])
  expect_length(grep("no data", msgs), 2)
  expect_equal(d@Landings@Name, "C")
  expect_equal(unname(d@Landings@Value[, "C"]), 4:6)
  expect_equal(d@LandingsAtAge@Name, "C")
  expect_equal(dimnames(d@LandingsAtAge@Value)$Fleet, "C")
})

.IndexMatrix <- function(x, Years, Names) {
  matrix(x, length(Years), length(Names), dimnames = list(Year = Years, Fleet = Names))
}

test_that("CPUE of combined fleets moves to Survey with its fleet's selectivity from before combining", {
  om <- .CombineSettingsOM() |> Populate(silent = TRUE)
  fl <- FleetNames(om)
  Years <- Years(om, "Historical")
  val <- .IndexMatrix(c(seq_len(20) / 10, rep(NA, 5), seq_len(15) / 10), Years, fl)
  d <- Data()
  d@CPUE <- IndicesData(Name = fl, Value = val, CV = val * 0 + 0.2,
                        Units = c("Biomass", "Number"), Timing = c(0.25, 0.75))
  d@Survey <- IndicesData(Name = "Acoustic", Value = .IndexMatrix(1, Years, "Acoustic"),
                          Units = "Biomass", Timing = 0.5, Selectivity = "SBiomass")
  om@Data <- list(d)

  omc <- CombineFleets(om, silent = TRUE)
  dc <- omc@Data[[1]]
  expect_null(dc@CPUE@Value)
  expect_equal(dc@Survey@Name, c("Acoustic", fl))
  expect_equal(dc@Survey@Units, c("Biomass", "Biomass", "Number"))
  expect_equal(dc@Survey@Timing, c(0.5, 0.25, 0.75))
  expect_equal(dc@Survey@Selectivity, "SBiomass")
  expect_equal(dc@Survey@Value[, fl], val, ignore_attr = TRUE)
  expect_equal(dc@Survey@CV[, fl], val * 0 + 0.2, ignore_attr = TRUE)
  expect_true(all(is.na(dc@Survey@CV[, "Acoustic"])))
  expect_equal(names(omc@Obs[[1]]), c("Combined", fl))
  for (f in fl)
    expect_equal(omc@Obs[[1]][[f]]@Survey@Selectivity[[1]],
                 om@Fleet[[1]][[f]]@Selectivity@MeanAtAge)

  omc <- CombineFleets(om, list(AsympExFleet = fl), silent = TRUE)
  expect_equal(omc@Data[[1]]@Survey@Name, c("Acoustic", "AsympExFleet CPUE", "DomeExFleet"))
  expect_equal(names(omc@Obs[[1]]), c("AsympExFleet", "AsympExFleet CPUE", "DomeExFleet"))
})

test_that("fleet-named Survey indices are kept; index selectivity and fleets not combined are respected", {
  data(TwoFleetOM, envir = environment())
  fl <- FleetNames(TwoFleetOM)
  om <- AddFleet(TwoFleetOM, "Third") |> Populate(silent = TRUE)
  Years <- Years(om, "Historical")
  d <- Data()
  d@CPUE <- IndicesData(Name = c(fl[1], "Third", fl[2]), Value = .IndexMatrix(1, Years, c(fl[1], "Third", fl[2])),
                        Units = c("Biomass", "Number", "Biomass"), Timing = c(0.1, 0.2, 0.3),
                        Selectivity = c("SBiomass", "Biomass"))
  d@Survey <- IndicesData(Name = fl[2], Value = .IndexMatrix(2, Years, fl[2]))
  om@Data <- list(d)

  # the survey's own selectivity is kept
  expect_equal(om@Obs[[1]][[fl[2]]]@Survey@Selectivity, "Biomass")
  omc <- CombineFleets(om, list(AB = fl), silent = TRUE)
  expect_equal(omc@Obs[[1]][[fl[2]]]@Survey@Selectivity, "Biomass")

  om@Obs[[1]][[fl[2]]]@Survey@Selectivity <- NULL
  omc <- CombineFleets(om, list(AB = fl), silent = TRUE)
  dc <- omc@Data[[1]]
  expect_equal(dc@CPUE@Name, "Third")
  expect_equal(dc@CPUE@Units, "Number")
  expect_equal(dc@CPUE@Timing, 0.2)
  expect_equal(dc@CPUE@Selectivity, "Biomass")
  expect_equal(dc@Survey@Name, c(fl[2], fl[1], paste(fl[2], "CPUE")))
  expect_equal(unname(dc@Survey@Value[1, ]), c(2, 1, 1))
  expect_equal(names(omc@Obs[[1]]), c("AB", "Third", fl[2], fl[1], paste(fl[2], "CPUE")))

  fleet_sel <- om@Fleet[[1]][[fl[2]]]@Selectivity@MeanAtAge
  expect_equal(omc@Obs[[1]][[fl[2]]]@Survey@Selectivity[[1]], fleet_sel)
  expect_equal(omc@Obs[[1]][[fl[1]]]@Survey@Selectivity, "SBiomass")
  expect_equal(omc@Obs[[1]][[paste(fl[2], "CPUE")]]@Survey@Selectivity[[1]], fleet_sel)
})

test_that("moved fleet CPUE conditions as it did before combining", {
  skip_on_cran()
  om <- .CombineSettingsOM()
  set.seed(1)
  d <- Simulate(om, silent = TRUE)@Data[[1]]
  while (!isS4(d)) d <- d[[1]]
  fl <- FleetNames(om)
  d@CPUE <- d@Survey
  d@CPUE@Value[1:10, fl[2]] <- NA
  d@Survey <- new("indicesdata")
  om <- .ZeroEffortFix(om)
  om@Data <- list(d)

  set.seed(2)
  before <- Simulate(om, silent = TRUE)
  omc <- CombineFleets(om, silent = TRUE)
  set.seed(2)
  after <- Simulate(omc, silent = TRUE)
  for (f in fl)
    expect_equal(after@OM@Obs[[1]][[f]]@Survey@Efficiency, before@OM@Obs[[1]][[f]]@CPUE@Efficiency,
                 tolerance = 0.02, ignore_attr = TRUE)

  mse <- Project(after, MPs = "CurrentEffort", parallel = FALSE, silent = TRUE)
  ppd <- mse@PPD[[1]][[1]]
  while (!isS4(ppd)) ppd <- ppd[[1]]
  expect_true(all(fl %in% ppd@Survey@Name))
  expect_false(anyNA(utils::tail(ppd@Survey@Value[, fl], 5)))
})


test_that("combined OM with partial-coverage fleet data simulates and projects", {
  skip_on_cran()
  om <- .CombineSettingsOM()
  set.seed(1)
  d <- Simulate(om, silent = TRUE)@Data[[1]]
  while (!isS4(d)) d <- d[[1]]
  fl <- FleetNames(om)
  d@Landings@Value[1:8, fl[2]] <- NA
  d@Landings@Value[3:4, fl[1]] <- NA
  d@Survey@Value[1:10, fl[2]] <- NA
  om <- .ZeroEffortFix(om)
  om@Data <- list(d)

  omc <- CombineFleets(om, silent = TRUE)
  dc <- omc@Data[[1]]
  expect_true(all(is.na(dc@Landings@Value[3:4, "Combined"])))
  expect_false(anyNA(dc@Discards@CV))
  set.seed(2)
  hist <- Simulate(omc, silent = TRUE)
  expect_no_error(Project(hist, MPs = "CurrentEffort", parallel = FALSE, silent = TRUE))
})

test_that("composition years with no sample in any combined fleet stay NA", {
  val <- array(NA_real_, c(3, 3, 2), dimnames = list(Year = 2001:2003, Fleet = c("A", "B", "C"), Age = 1:2))
  val[2, "A", ] <- c(1, 2)
  val[3, "A", ] <- c(1, 2)
  val[3, "B", ] <- c(3, 4)
  val[, "C", ] <- 1
  om <- .CombineDataOM(LandingsAtAge = CompData(Name = c("A", "B", "C"), Value = val))
  d <- .CombineFleetsData(om, list(AB = c("A", "B")), silent = TRUE)@Data[[1]]@LandingsAtAge
  expect_true(all(is.na(d@Value[1, "AB", ])))
  expect_equal(unname(d@Value[2, "AB", ]), c(1, 2))
  expect_equal(unname(d@Value[3, "AB", ]), c(4, 6))
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
