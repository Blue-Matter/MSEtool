# Conditioning composition observation error on real LandingsAtAge/LandingsAtSize
# data (R/condition-obs-comp.R), then projecting with the conditioned Shift/ESS.

skip_on_cran()

# Sim-1 catch as [Year x Bin] counts; NAYears and zero-catch years left all-NA
.RealComp <- function(catch, bin, Classes, HistYears, FleetName, NAYears, n = 200) {
  dn   <- names(dimnames(catch))
  obs  <- apply(abind::asub(catch, 1, match('Sim', dn), drop = FALSE),
                match(c('Year', bin), dn), sum)
  comp <- array(round(obs / rowSums(obs) * n), dim = c(length(HistYears), 1, length(Classes)),
                dimnames = list(Year = HistYears, Fleet = FleetName, Class = Classes))
  comp[as.character(NAYears), , ] <- NA
  comp[!is.finite(apply(comp, 1, sum)) | apply(comp, 1, sum) == 0, , ] <- NA
  comp
}

.BuildConditionedComp <- function() {
  data(SingleStockOM, envir = environment())
  OM <- SingleStockOM
  nSim(OM) <- 3

  H0 <- Simulate(OM, silent = TRUE, control = SimControl(CalcCatchAtSize = TRUE))
  HistYears <- Years(OM, 'Historical')
  FleetName <- FleetNames(OM)
  NAYears   <- HistYears[c(1, 10, length(HistYears))]

  Ages <- as.numeric(dimnames(H0@LandingsAtAge[[1]])$Age)
  Lens <- as.numeric(dimnames(H0@LandingsAtSize[[1]][[1]])$Class)

  caa <- .RealComp(H0@LandingsAtAge[[1]][, , , 1, , drop = FALSE], 'Age', Ages,
                   HistYears, FleetName, NAYears)
  cal <- .RealComp(H0@LandingsAtSize[[1]][[1]], 'Class', Lens,
                   HistYears, FleetName, NAYears)

  Data(OM) <- Data(Name = 'Real', Years = HistYears,
                   LandingsAtAge  = CompData(Name = FleetName, Value = caa, Classes = Ages,
                                             Units = 'years'),
                   LandingsAtSize = CompData(Name = FleetName, Value = cal,
                                             Classes = setNames(list(Lens), FleetName),
                                             Units = 'cm'))

  Hist <- Simulate(OM, silent = TRUE)
  list(Hist = Hist, NAYears = NAYears, HistYears = HistYears,
       ProjYears = Years(OM, 'Projection'), nSim = nSim(OM))
}

built <- .BuildConditionedComp()

test_that("conditioned comp ESS, SampleSize and Shift span every year for all sims", {
  AllYears <- c(built$HistYears, built$ProjYears)
  for (type in c('LandingsAtAge', 'LandingsAtSize')) {
    co <- slot(Obs(built$Hist@OM)[[1]][[1]], type)

    expect_equal(unname(dim(co@ESS)), c(built$nSim, length(AllYears)))
    expect_true(all(is.finite(co@ESS)), label = paste(type, "ESS finite"))
    expect_true(all(co@ESS > 0), label = paste(type, "ESS positive"))
    ss  <- .SubsetYear(co@SampleSize, AllYears)[1, ]
    obs <- ss > 0
    expect_true(all(co@ESS[, obs] <= rep(ss[obs], each = built$nSim) + 1e-8))

    expect_equal(names(dimnames(co@Shift))[1:2], c('Sim', 'Year'))
    expect_equal(unname(dim(co@Shift)[1:2]), c(built$nSim, length(AllYears)))
    expect_true(all(is.finite(co@Shift[, as.character(built$ProjYears), ])))

    expect_true(all(co@SampleSize[, as.character(built$NAYears)] == 0))
    expect_true(all(co@SampleSize[, as.character(built$ProjYears)] > 0))
  }
})

test_that("Project() generates comp data from conditioned LandingsAtAge/LandingsAtSize obs", {
  MSE <- Project(built$Hist, 'CurrentCatch', silent = TRUE)
  expect_s4_class(MSE, 'mse')

  PPD <- Data(MSE)[[1]]
  expect_length(PPD, built$nSim)
  for (type in c('LandingsAtAge', 'LandingsAtSize')) {
    ProjYr <- intersect(as.character(built$ProjYears),
                        dimnames(slot(PPD[[1]][[1]], type)@Value)$Year)
    expect_true(length(ProjYr) > 0)
    ss <- slot(Obs(built$Hist@OM)[[1]][[1]], type)@SampleSize[1, ProjYr]
    for (x in seq_len(built$nSim)) {
      val <- slot(PPD[[x]][[1]], type)@Value[ProjYr, 1, , drop = FALSE]
      expect_true(all(is.finite(val)), label = paste(type, "sim", x, "projected comps finite"))
      expect_equal(unname(apply(val, 1, sum)), unname(round(ss)))
    }
  }
})
