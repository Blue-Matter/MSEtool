# Index values in historical and projection years against a nominal index computed here from
# numbers-at-age and the selectivity as specified: Value = Efficiency * Nom^Beta * Error

skip_on_cran()

# selectivity-at-age of an index for one stock, sim and time step
.RefSel <- function(mse, cx, Name, Type, io, st, x, y, Gen) {
  Ages <- mse@OM@Stock[[st]]@Ages@Classes
  StockName <- StockNames(mse@OM)[st]
  spec <- io@Selectivity
  if (is.list(spec)) spec <- spec[[StockName]]
  Key <- paste(Type, Name)
  if (!is.null(Gen[[Key]])) return(Gen[[Key]](StockName, Ages, y))
  if (is.null(spec)) return(.AtYear(mse@OM@Fleet[[st]][[Name]]@Selectivity@MeanAtAge, x, y))
  if (identical(spec, "Biomass")) return(rep(1, length(Ages)))
  if (identical(spec, "SBiomass")) return(.AtYear(mse@OM@Stock[[st]]@Maturity@MeanAtAge, x, y))
  if (!is.null(dimnames(spec)$Year)) stop("no selectivity generator for index ", Name)
  out <- stats::setNames(rep(0, length(Ages)), Ages)
  out[names(spec)] <- spec
  out
}

# value of a [Sim, Age, Year, (Area)] array for sim x and time step y: [Age] or [Age, Area]
.AtYear <- function(arr, x, y) {
  a <- .ArraySubsetYear(arr, y)
  a <- abind::asub(a, min(x, dim(a)[1]), 1, drop = FALSE)
  abind::adrop(a, drop = which(names(dimnames(a)) %in% c("Sim", "Year")))
}

# [Age, Area] for the index areas
.ByArea <- function(v, Areas) {
  if (is.null(dim(v)) || length(dim(v)) == 1) return(matrix(as.numeric(v), length(v), length(Areas)))
  v[, Areas, drop = FALSE]
}

# Number-at-age [Age, Area] of stock st, sim x, time step y
.RefNumber <- function(hist, mse, st, x, y) {
  N <- hist@Number[[st]]
  if (!as.character(y) %in% dimnames(N)$Year) N <- mse@Number[[st]][, , , , 1]
  N[x, , as.character(y), ]
}

.RefNom <- function(hist, mse, cx, Name, Type, io, Units, x, y, Gen, RecAge) {
  Areas <- io@Areas %||NA% seq_len(nArea(mse))
  stocks <- .ResolveComplexes(mse@OM)[[cx]]
  sum(vapply(stocks, \(st) {
    N   <- .RefNumber(hist, mse, st, x, y)[, Areas, drop = FALSE]
    Sel <- .ByArea(.RefSel(mse, cx, Name, Type, io, st, x, y, Gen), Areas)
    if (Units == "Recruitment") {
      a <- RecAge[[st]][x]
      return(sum(Sel[a, ] * N[a, ]))
    }
    W <- if (Units == "Biomass") .ByArea(.AtYear(mse@OM@Stock[[st]]@Weight@MeanAtAge, x, y), Areas) else 1
    sum(Sel * W * N)
  }, numeric(1)))
}

# first age class with fish in the observed historical time steps, per stock and sim
.RecAge <- function(hist, cx, ObsYears) {
  lapply(stats::setNames(nm = seq_along(hist@Number)), \(st) {
    N <- hist@Number[[st]][, , as.character(ObsYears), , drop = FALSE]
    apply(N, 1, \(n) which(apply(n, 1, sum) > 0)[1])
  })
}

# [Sim, Year, Period, Value, Expected, Name, Type] for every observed value of every index
.IndexContinuity <- function(hist, mse, Gen = list()) {
  LastHist <- max(Years(hist, "Historical"))
  out <- list()
  for (cx in seq_along(.ResolveComplexes(mse@OM))) {
    for (type in c("CPUE", "Survey")) {
      for (x in seq_len(nSim(mse))) {
        D <- slot(mse@PPD[[1]][[x]][[cx]], type)
        if (EmptyObject(D)) next
        for (j in seq_along(D@Name)) {
          Name <- D@Name[j]
          v <- D@Value[, j]
          yr <- as.numeric(names(v))
          ok <- is.finite(v)
          if (!any(ok)) next
          io <- slot(mse@OM@Obs[[cx]][[Name]], type)
          Units <- D@Units[j]
          RecAge <- if (Units == "Recruitment") .RecAge(hist, cx, yr[ok & yr <= LastHist])
          Beta <- .SimValues(io@Beta %||% 1, nSim(mse))[x]
          Eff  <- .SimValues(io@Efficiency, nSim(mse))[x]
          qInc <- if (is.null(io@qInc)) 0 else .SimValues(io@qInc, nSim(mse))[x]
          Exp <- vapply(yr[ok], \(y) {
            Nom <- .RefNom(hist, mse, cx, Name, type, io, Units, x, y, Gen, RecAge)
            Err <- io@Error[min(x, nrow(io@Error)), as.character(y)]
            Eff * Nom^Beta * Err * (1 + qInc / 100)^max(0, y - LastHist)
          }, numeric(1))
          out[[length(out) + 1]] <- data.frame(
            Complex = cx, Type = type, Name = Name, Sim = x, Year = yr[ok],
            Period = ifelse(yr[ok] <= LastHist, "Historical", "Projection"),
            Timing = if (length(D@Timing) >= j && is.finite(D@Timing[j])) D@Timing[j] else 0,
            Value = v[ok], Expected = Exp)
        }
      }
    }
  }
  do.call(rbind, out)
}

.ExpectIndexContinuity <- function(d, Names, Fixture) {
  expect_setequal(unique(d$Name), Names)
  for (nm in Names) {
    dn <- d[d$Name == nm, ]
    lab <- paste(Fixture, nm)
    expect_setequal(unique(dn$Period), c("Historical", "Projection"))
    expect_true(all(dn$Value > 0), label = paste(lab, "positive values"))
    if (all(dn$Timing == 0)) {
      expect_equal(dn$Value, dn$Expected, tolerance = 1e-6, label = lab)
    } else {
      # within-step decay is not in Expected: its ratio carries across the boundary
      r <- log(dn$Value / dn$Expected)
      expect_true(all(r < 0), label = paste(lab, "decayed to Timing"))
      Last  <- tapply(r[dn$Period == "Historical"], dn$Sim[dn$Period == "Historical"], \(z) tail(z, 1))
      First <- tapply(r[dn$Period == "Projection"], dn$Sim[dn$Period == "Projection"], \(z) z[1])
      expect_lt(max(abs(First - Last)), log(1.5), label = lab)
    }
  }
}

.Continuity <- function(om, Gen = list(), ...) {
  set.seed(1)
  hist <- Simulate(om, silent = TRUE, ...)
  set.seed(2)
  mse <- suppressWarnings(Project(hist, MPs = "CurrentEffort", parallel = FALSE, silent = TRUE))
  list(hist = hist, mse = mse, d = .IndexContinuity(hist, mse, Gen))
}

# observed index values: total biomass of the complex in sim 1 at the observed time steps, with noise
.ObsValues <- function(h0, cx, ObsYears, sd = 0.2) {
  Tot <- apply(h0@Biomass[1, .ResolveComplexes(h0@OM)[[cx]], , drop = FALSE], 3, sum)
  stats::setNames(Tot[as.character(ObsYears)] * exp(stats::rnorm(length(ObsYears), 0, sd)), ObsYears)
}

# selectivity array over historical years from a generator
.SelArray <- function(Gen, StockName, Ages, Years) {
  array(vapply(Years, \(y) Gen(StockName, Ages, y), numeric(length(Ages))),
        c(length(Ages), length(Years)), list(Age = Ages, Year = Years))
}

# ---- annual, single stock ----

test_that("annual single-stock indices are continuous from historical to projection years", {
  data(SingleStockOM, envir = environment())
  om <- UpdateObject(SingleStockOM)
  om@nSim <- 3
  om@pYear <- 4
  fl <- FleetNames(om)[1]
  om@Obs[[1]][[fl]]@Survey <- IndicesObs(CV = 0.2, Selectivity = "SBiomass")

  set.seed(10)
  h0 <- Simulate(om, silent = TRUE)
  HY <- Years(om, "Historical")
  PY <- Years(om, "Projection")
  Val <- \() .ObsValues(h0, 1, HY)
  Ages <- om@Stock[[1]]@Ages@Classes

  # selectivity shifts to older ages in the last 10 historical years
  Gen <- list(`Survey YearArray` = \(StockName, Ages, y) as.numeric(Ages >= ifelse(y > max(HY) - 10, 4, 2)))

  om <- AddIndex(om, Val(), Name = "Biomass", Units = "Biomass", Selectivity = "Biomass")
  om <- AddIndex(om, Val(), Name = "AgeVector", Units = "Number", Selectivity = c("3" = 0.5, "4" = 1, "5" = 1))
  om <- AddIndex(om, Val(), Name = "YearArray", Units = "Biomass",
                 Selectivity = .SelArray(Gen$`Survey YearArray`, StockNames(om), Ages, HY))
  om <- AddIndex(om, Val(), Name = "Recruitment", Units = "Recruitment", Selectivity = "Biomass")
  om <- AddIndex(om, Val(), Name = "Timing", Timing = 0.5, Selectivity = "Biomass")
  om <- AddIndex(om, Val(), Name = "qInc", Selectivity = "Biomass", IndexObs = IndicesObs(qInc = 3))
  om <- AddIndex(om, Val(), Name = "Beta", Selectivity = "Biomass", IndexObs = IndicesObs(Beta = 0.7))
  om <- AddIndex(om, Val(), Name = fl, Type = "CPUE")

  # values supplied for projection years are used as-is
  User <- stats::setNames(c(1.5, 2.5, 3.5, 4.5)[seq_along(PY)], PY)
  om <- AddIndex(om, Val(), Name = "UserProjection", Selectivity = "Biomass")
  S <- om@Data[[1]]@Survey
  NewRows <- matrix(NA_real_, length(PY), ncol(S@Value), dimnames = list(PY, colnames(S@Value)))
  NewRows[, "UserProjection"] <- User
  S@Value <- rbind(S@Value, NewRows)
  if (!is.null(S@CV)) S@CV <- rbind(S@CV, NewRows * NA)
  om@Data[[1]]@Survey <- S
  om@Data[[1]]@Years <- c(HY, PY)

  fx <- .Continuity(om, Gen)
  d <- fx$d
  Checked <- d[d$Name != "UserProjection", ]
  .ExpectIndexContinuity(Checked, c(fl, "Biomass", "AgeVector", "YearArray", "Recruitment",
                                    "Timing", "qInc", "Beta"), "annual")
  expect_setequal(Checked$Type[Checked$Name == fl], c("CPUE", "Survey"))

  Up <- d[d$Name == "UserProjection", ]
  expect_equal(Up$Value[Up$Period == "Historical"], Up$Expected[Up$Period == "Historical"], tolerance = 1e-6)
  for (x in seq_len(nSim(om))) {
    Proj <- Up[Up$Sim == x & Up$Period == "Projection", ]
    expect_equal(stats::setNames(Proj$Value, Proj$Year), User[as.character(Proj$Year)])
  }
})

# ---- seasonal, multi-stock complex, two areas ----

# season-1 observations; selectivity differs by season and stock
.SeasonalGen <- function(StockName, Ages, y) {
  Season <- round((y %% 1) * 4) + 1
  Peak <- if (grepl("Butterfish", StockName)) 2 else 4
  s <- stats::plogis(Ages - Peak + Season)
  s / max(s)
}

.SeasonalComplexOM <- function(SameAges) {
  data(MultiStockOM, envir = environment())
  om <- MultiStockOM
  om@Seasons <- 4
  om@nSim <- 3
  om@pYear <- 3
  om@Complexes <- list(Both = 1:2)
  om@Obs <- list()
  om@Imp <- list()
  if (SameAges) Ages(om@Stock[[2]]) <- Ages(om@Stock[[1]])
  om
}

.SeasonalComplexIndices <- function(om, Full = TRUE) {
  Fleets <- FleetNames(om)
  set.seed(10)
  h0 <- Simulate(om, silent = TRUE)
  HY <- Years(om, "Historical")
  S1 <- HY[HY %% 1 == 0]
  Val <- \() .ObsValues(h0, 1, S1)
  SN <- StockNames(om)
  SelList <- stats::setNames(lapply(seq_along(SN), \(st)
    .SelArray(.SeasonalGen, SN[st], om@Stock[[st]]@Ages@Classes, HY)), SN)
  Gen <- list(`Survey SeasonalNumber` = .SeasonalGen, `Survey SeasonalBiomass` = .SeasonalGen)

  om <- PopulateOM(om, silent = TRUE)
  om@Obs[[1]][[Fleets[1]]]@CPUE <- IndicesObs(CV = 0.2)
  om <- AddIndex(om, Val(), Name = "SeasonalNumber", Units = "Number", Selectivity = SelList)
  om <- AddIndex(om, Val(), Name = "VectorList", Units = "Number",
                 Selectivity = stats::setNames(list(c("2" = 1, "3" = 1), c("1" = 0.5, "2" = 1)), SN))
  Names <- c(Fleets[1], "SeasonalNumber", "VectorList")
  if (Full) {
    om@Obs[[1]][[Fleets[2]]]@Survey <- IndicesObs(CV = 0.2, Selectivity = SelList)
    Gen[[paste("Survey", Fleets[2])]] <- .SeasonalGen
    om <- AddIndex(om, Val(), Name = "SeasonalBiomass", Units = "Biomass", Selectivity = SelList)
    om <- AddIndex(om, Val(), Name = "Area2", Selectivity = "Biomass", IndexObs = IndicesObs(Areas = 2))
    om <- AddIndex(om, Val(), Name = "Recruitment", Units = "Recruitment", Selectivity = "Biomass")
    om <- AddIndex(om, Val(), Name = Fleets[2], Type = "CPUE")
    Names <- c(Names, "SeasonalBiomass", "Area2", "Recruitment")
  }
  list(om = om, Gen = Gen, Names = Names)
}

test_that("seasonal multi-stock complex indices are continuous from historical to projection years", {
  ix <- .SeasonalComplexIndices(.SeasonalComplexOM(SameAges = TRUE))
  fx <- .Continuity(ix$om, ix$Gen)
  .ExpectIndexContinuity(fx$d, unique(c(ix$Names, FleetNames(ix$om))), "seasonal complex")
  # simulated indices are observed every time step; real ones in season 1 only
  Real <- fx$d[fx$d$Name %in% c("SeasonalNumber", "Area2"), ]
  expect_true(all(Real$Year %% 1 == 0))
})

test_that("indices of a seasonal complex whose stocks have different ages are continuous", {
  ix <- .SeasonalComplexIndices(.SeasonalComplexOM(SameAges = FALSE), Full = FALSE)
  fx <- .Continuity(ix$om, ix$Gen)
  .ExpectIndexContinuity(fx$d, ix$Names, "different ages")
})

# ---- monthly, spatial, single stock ----

test_that("monthly spatial indices observed in two seasons are continuous", {
  data(SeasonalSpatialOM, envir = environment())
  om <- UpdateObject(SeasonalSpatialOM)
  om@nSim <- 3
  om@pYear <- 2
  fl <- FleetNames(om)[1]
  om@Obs[[1]][[fl]]@CPUE <- IndicesObs(CV = 0.2)

  set.seed(10)
  h0 <- Simulate(om, silent = TRUE)
  HY <- Years(om, "Historical")
  Season <- (seq_along(HY) - 1) %% Seasons(om) + 1
  ObsYears <- HY[Season %in% c(3, 9)]
  Ages <- om@Stock[[1]]@Ages@Classes
  Gen <- list(`Survey Monthly` = \(StockName, Ages, y) {
    s <- round((y %% 1) * 12) + 1
    as.numeric(Ages >= min(Ages) + (s %% 3))
  })
  om <- AddIndex(om, .ObsValues(h0, 1, ObsYears), Name = "Monthly", Units = "Number",
                 Selectivity = .SelArray(Gen$`Survey Monthly`, StockNames(om), Ages, HY),
                 IndexObs = IndicesObs(Areas = c(1, 3)))
  om <- AddIndex(om, .ObsValues(h0, 1, ObsYears), Name = "MonthlyBiomass", Selectivity = "Biomass")
  om <- AddIndex(om, .ObsValues(h0, 1, ObsYears), Name = "TimingAreas", Selectivity = "Biomass",
                 Timing = 0.5, IndexObs = IndicesObs(Areas = c(1, 3)))

  fx <- .Continuity(om, Gen)
  .ExpectIndexContinuity(fx$d, c(fl, "Monthly", "MonthlyBiomass", "TimingAreas"), "monthly spatial")
  Obs <- fx$d[fx$d$Name == "Monthly", ]
  AllYears <- Years(om)
  AllSeason <- (seq_along(AllYears) - 1) %% Seasons(om) + 1
  expect_true(all(Obs$Year %in% AllYears[AllSeason %in% c(3, 9)]))
})
