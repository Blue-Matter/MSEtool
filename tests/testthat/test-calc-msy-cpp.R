# Validation of the parallel C++ MSY implementation (R/calc-msy-cpp.R,
# src/calc_msy_cpp.cpp) against the existing, untouched R implementation
# (R/calc-msy.R, R/calc-per-recruit.R). CalcMSY() itself is not modified or
# exercised any differently here -- this only tests the new CalcMSYCpp() and
# its internals against it.
#
# .CalcPerRecruitFScalar()'s (R) output arrays have a harmless, pre-existing
# quirk: `dim(x)` sometimes carries a partially-blank `names` attribute (e.g.
# c("Sim", "", "Year")) inherited from AddDimension()/.Aperm() -- dimnames()
# itself (the functionally meaningful part) is unaffected. CalcMSYCpp()'s
# arrays don't have this incidental artifact. Stripped before comparison at
# the unit level below; confirmed inconsequential since the end-to-end
# CalcMSY()/CalcMSYCpp() comparison matches cleanly without any stripping.
.StripDimNames <- function(x) {
  if (is.array(x)) names(dim(x)) <- NULL
  x
}
.StripPerRecruitDimNames <- function(pr) {
  for (sl in c('NPR0', 'NPR0_SP', 'SPR0', 'NPRF', 'NPRF_SP', 'SPRF', 'SPR',
              'Biomass', 'SBiomass', 'SProduction', 'Removals', 'Landings')) {
    val <- slot(pr, sl)
    if (!is.null(val)) slot(pr, sl) <- .StripDimNames(val)
  }
  pr
}

skip_on_cran()

.msy_om_battery <- function() {
  list(
    single = list(data = "SingleStockOM", nSim = 8),
    multi  = list(data = "MultiStockOM",  nSim = 4),
    complex = list(data = "ComplexOM",    nSim = 4),
    herm   = list(data = "HermOM",        nSim = 2)
  )
}

.load_om <- function(data_name, nSim, spawn_time_frac = NULL) {
  data(list = data_name, envir = environment())
  om <- get(data_name, envir = environment())
  om@nSim <- nSim
  if (!is.null(spawn_time_frac)) om@Stock[[1]]@SRR@SpawnTimeFrac <- spawn_time_frac
  om
}

# ---- Unit-level: .CalcPerRecruitFScalarCpp() vs .CalcPerRecruitFScalar() ----
# across a grid of apicalF values including near-zero and stock-collapse-large.

test_that(".CalcPerRecruitFScalarCpp matches .CalcPerRecruitFScalar across an F grid (annual OMs)", {
  skip_on_cran()

  Fgrid <- c(1e-6, 1e-3, 0.01, 0.05, 0.1, 0.3, 0.5, 1, 2, 5, 10, 50)

  check_one <- function(data_name, nSim, spawn_time_frac = NULL) {
    om <- .load_om(data_name, nSim, spawn_time_frac)
    set.seed(1)
    hist_no_msy <- Simulate(om, silent = TRUE, control = SimControl(MSYRefs = FALSE))
    MSYYears <- utils::tail(Years(hist_no_msy@OM, 'Historical'), 1)

    StockList      <- hist_no_msy@OM@Stock
    FleetList      <- hist_no_msy@OM@Fleet
    SPR0_Full_List <- MSEtool:::Array2List(MSEtool:::CalcSPR0(hist_no_msy, silent = TRUE))
    StockList_sim  <- MSEtool:::Subset(StockList, Sims = 1)
    FleetList_sim  <- MSEtool:::Subset(FleetList, Sims = 1)
    SPR0_List_sim  <- MSEtool:::Subset(SPR0_Full_List, Sims = 1)

    inputs <- MSEtool:::.PrepPerRecruitInputs(StockList_sim, FleetList_sim, SPR0_List_sim, MSYYears)
    flat   <- MSEtool:::.FlattenPerRecruitInputsForCpp(inputs)

    for (f in Fgrid) {
      pr_r <- MSEtool:::.CalcPerRecruitFScalar(
        f,
        StockFleetAllocation      = inputs$StockFleetAllocation,
        NaturalMortalityList      = inputs$NaturalMortalityList,
        PlusGroupList             = inputs$PlusGroupList,
        MaturityList              = inputs$MaturityList,
        SemelparousList           = inputs$SemelparousList,
        WeightList                = inputs$WeightList,
        SpawnTimeFracList         = inputs$SpawnTimeFracList,
        SPFrom                    = inputs$SPFrom,
        SPR0List                  = inputs$SPR0List,
        FecundityList              = inputs$FecundityList,
        WeightFleetRetainedList    = inputs$WeightFleetRetainedList,
        WeightFleetSelectedList    = inputs$WeightFleetSelectedList,
        SelectivityFleetList       = inputs$SelectivityFleetList,
        RetentionFleetList         = inputs$RetentionFleetList,
        DiscardMortalityFleetList  = inputs$DiscardMortalityFleetList,
        FleetNames                 = inputs$FleetNames,
        Years                      = inputs$Years,
        NPR0List                   = inputs$NPR0List,
        NPR0_SPList                = inputs$NPR0_SPList
      )
      pr_cpp <- MSEtool:::.CalcPerRecruitFScalarCpp(f, inputs, flat)

      expect_equal(.StripPerRecruitDimNames(pr_r), .StripPerRecruitDimNames(pr_cpp),
                  tolerance = 1e-8,
                  info = sprintf("%s, F=%g", data_name, f))
    }
  }

  check_one("SingleStockOM", 3)
  check_one("MultiStockOM", 3)
  check_one("ComplexOM", 3)
  check_one("HermOM", 2)
  check_one("SingleStockOM", 2, spawn_time_frac = 0.5)
})

test_that(".CalcPerRecruitFScalarCpp handles single-fleet, Retention<1 (discards), and PlusGroup off", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 2
  set.seed(1)
  hist_no_msy <- Simulate(om, silent = TRUE, control = SimControl(MSYRefs = FALSE))
  # Exercise the discard-ratio divide path: retention < 1 and nonzero discard
  # mortality on the (only) fleet, so dead discards are actually nonzero.
  # Set post-Simulate() -- Fleet@Retention/@DiscardMortality get regenerated
  # from other parameters during simulation, so an OM-level edit beforehand
  # doesn't survive.
  hist_no_msy@OM@Fleet[[1]][[1]]@Retention@MeanAtAge[] <- 0.7
  hist_no_msy@OM@Fleet[[1]][[1]]@DiscardMortality@MeanAtAge[] <- 0.3
  MSYYears <- utils::tail(Years(hist_no_msy@OM, 'Historical'), 1)

  StockList      <- hist_no_msy@OM@Stock
  FleetList      <- hist_no_msy@OM@Fleet
  SPR0_Full_List <- MSEtool:::Array2List(MSEtool:::CalcSPR0(hist_no_msy, silent = TRUE))
  StockList_sim  <- MSEtool:::Subset(StockList, Sims = 1)
  FleetList_sim  <- MSEtool:::Subset(FleetList, Sims = 1)
  SPR0_List_sim  <- MSEtool:::Subset(SPR0_Full_List, Sims = 1)

  inputs <- MSEtool:::.PrepPerRecruitInputs(StockList_sim, FleetList_sim, SPR0_List_sim, MSYYears)
  flat   <- MSEtool:::.FlattenPerRecruitInputsForCpp(inputs)

  for (f in c(0.05, 0.2, 0.8)) {
    pr_r <- MSEtool:::.CalcPerRecruitFScalar(
      f,
      StockFleetAllocation      = inputs$StockFleetAllocation,
      NaturalMortalityList      = inputs$NaturalMortalityList,
      PlusGroupList             = inputs$PlusGroupList,
      MaturityList              = inputs$MaturityList,
      SemelparousList           = inputs$SemelparousList,
      WeightList                = inputs$WeightList,
      SpawnTimeFracList         = inputs$SpawnTimeFracList,
      SPFrom                    = inputs$SPFrom,
      SPR0List                  = inputs$SPR0List,
      FecundityList              = inputs$FecundityList,
      WeightFleetRetainedList    = inputs$WeightFleetRetainedList,
      WeightFleetSelectedList    = inputs$WeightFleetSelectedList,
      SelectivityFleetList       = inputs$SelectivityFleetList,
      RetentionFleetList         = inputs$RetentionFleetList,
      DiscardMortalityFleetList  = inputs$DiscardMortalityFleetList,
      FleetNames                 = inputs$FleetNames,
      Years                      = inputs$Years,
      NPR0List                   = inputs$NPR0List,
      NPR0_SPList                = inputs$NPR0_SPList
    )
    pr_cpp <- MSEtool:::.CalcPerRecruitFScalarCpp(f, inputs, flat)
    expect_equal(.StripPerRecruitDimNames(pr_r), .StripPerRecruitDimNames(pr_cpp), tolerance = 1e-8)
    expect_true(as.numeric(pr_r@Removals) > as.numeric(pr_r@Landings))  # discards actually present
  }
})

# ---- End-to-end: CalcMSYCpp() vs CalcMSY() ----

test_that("CalcMSYCpp matches CalcMSY across the annual OM battery", {
  skip_on_cran()

  check_msy <- function(data_name, nSim, spawn_time_frac = NULL) {
    om <- .load_om(data_name, nSim, spawn_time_frac)
    set.seed(1)
    hist_no_msy <- Simulate(om, silent = TRUE, control = SimControl(MSYRefs = FALSE))
    MSYYears <- utils::tail(Years(hist_no_msy@OM, 'Historical'), 1)

    msy_r   <- CalcMSY(hist_no_msy, Years = MSYYears, type = 'Removals', parallel = FALSE, silent = TRUE)
    msy_cpp <- MSEtool:::CalcMSYCpp(hist_no_msy, Years = MSYYears, type = 'Removals', parallel = FALSE, silent = TRUE)

    expect_equal(msy_r, msy_cpp, tolerance = 1e-6, info = data_name)
  }

  check_msy("SingleStockOM", 8)
  check_msy("MultiStockOM", 4)
  check_msy("ComplexOM", 4)
  check_msy("HermOM", 2)
  check_msy("SingleStockOM", 4, spawn_time_frac = 0.5)
})


# ---- Seasonal path ----
#
# MultiStockOM + Seasons (and .make_herm_seasonal_fixture(), built on it) used
# to hit a real, pre-existing bug in the untouched R original: `@SRR@SPFrom`
# was baked into a global (whole-OM) numeric stock index by `.OM2Hist()`
# (R/constructor-hist.R), but consumed as if local to whatever per-complex
# subset of stocks `.CalcPerRecruitFScalarSeasonal()` was given -- "subscript
# out of bounds" whenever a stock's complex didn't contain every stock its
# SPFrom could reference (MultiStockOM has no `@Complexes` grouping, so each
# stock is processed alone). Fixed by storing `@SRR@SPFrom` as a portable
# stock name instead (R/constructor-hist.R, R/utils-prep-hist-misc.R) --
# `MultiStockOM` + `Seasons > 1` is included in the battery below now that
# it's fixed.

test_that(".CalcPerRecruitFScalarCpp matches .CalcPerRecruitFScalarSeasonal across an F grid", {
  skip_on_cran()

  Fgrid <- c(1e-6, 0.01, 0.05, 0.2, 0.5, 2, 10)

  check_one_seasonal <- function(data_name, seasons, nSim, spawn_time_frac = NULL) {
    data(list = data_name, envir = environment())
    om <- get(data_name, envir = environment())
    om@Seasons <- seasons
    om@nSim <- nSim
    set.seed(1)
    hist_no_msy <- Simulate(om, silent = TRUE, control = SimControl(MSYRefs = FALSE))
    if (!is.null(spawn_time_frac))
      hist_no_msy@OM@Stock[[1]]@SRR@SpawnTimeFrac <- spawn_time_frac
    MSYYears <- unique(floor(utils::tail(Years(hist_no_msy@OM, 'Historical'), 1)))

    StockList      <- hist_no_msy@OM@Stock
    FleetList      <- hist_no_msy@OM@Fleet
    SPR0_Full_List <- MSEtool:::Array2List(MSEtool:::CalcSPR0(hist_no_msy, silent = TRUE))
    StockList_sim  <- MSEtool:::Subset(StockList, Sims = 1)
    FleetList_sim  <- MSEtool:::Subset(FleetList, Sims = 1)
    SPR0_List_sim  <- MSEtool:::Subset(SPR0_Full_List, Sims = 1)

    inputs <- MSEtool:::.PrepPerRecruitInputs(StockList_sim, FleetList_sim, SPR0_List_sim, MSYYears)
    flat   <- MSEtool:::.FlattenPerRecruitInputsForCpp(inputs)

    for (f in Fgrid) {
      pr_r <- MSEtool:::.CalcPerRecruitFScalarSeasonal(
        apicalF                   = f,
        StockFleetAllocation      = inputs$StockFleetAllocation,
        NaturalMortalityList      = inputs$NaturalMortalityList,
        PlusGroupList             = inputs$PlusGroupList,
        MaturityList              = inputs$MaturityList,
        SemelparousList           = inputs$SemelparousList,
        WeightList                = inputs$WeightList,
        SpawnTimeFracList         = inputs$SpawnTimeFracList,
        SPFrom                    = inputs$SPFrom,
        SPR0List                  = inputs$SPR0List,
        FecundityList              = inputs$FecundityList,
        WeightFleetRetainedList    = inputs$WeightFleetRetainedList,
        WeightFleetSelectedList    = inputs$WeightFleetSelectedList,
        SelectivityFleetList       = inputs$SelectivityFleetList,
        RetentionFleetList         = inputs$RetentionFleetList,
        DiscardMortalityFleetList  = inputs$DiscardMortalityFleetList,
        FleetNames                 = inputs$FleetNames,
        Years                      = inputs$Years,
        nSeason                    = inputs$nSeason,
        SeasonalWeightsList        = inputs$SeasonalWeightsList,
        CalendarYears              = inputs$CalendarYears,
        RefSeason                  = inputs$RefSeason
      )
      pr_cpp <- MSEtool:::.CalcPerRecruitFScalarCpp(f, inputs, flat)

      expect_equal(.StripPerRecruitDimNames(pr_r), .StripPerRecruitDimNames(pr_cpp),
                  tolerance = 1e-8,
                  info = sprintf("%s, Seasons=%d, F=%g", data_name, seasons, f))
      expect_equal(as.numeric(pr_r@Misc$F_annual_apical[[1]]),
                  as.numeric(pr_cpp@Misc$F_annual_apical[[1]]),
                  tolerance = 1e-8,
                  info = sprintf("F_annual_apical: %s, Seasons=%d, F=%g", data_name, seasons, f))
    }
  }

  check_one_seasonal("SeasonalSpatialOM", 12, 2)
  check_one_seasonal("SeasonalSpatialOM", 12, 1, spawn_time_frac = 0.3)
  check_one_seasonal("ComplexOM", 4, 2)
  check_one_seasonal("HermOM", 4, 1)
  check_one_seasonal("MultiStockOM", 2, 2)
})

test_that("CalcMSYCpp matches CalcMSY for seasonal OMs", {
  skip_on_cran()

  check_msy_seasonal <- function(data_name, seasons, nSim) {
    data(list = data_name, envir = environment())
    om <- get(data_name, envir = environment())
    om@Seasons <- seasons
    om@nSim <- nSim
    set.seed(1)
    hist_no_msy <- Simulate(om, silent = TRUE, control = SimControl(MSYRefs = FALSE))
    MSYYears <- unique(floor(utils::tail(Years(hist_no_msy@OM, 'Historical'), 1)))

    msy_r   <- CalcMSY(hist_no_msy, Years = MSYYears, type = 'Removals', parallel = FALSE, silent = TRUE)
    msy_cpp <- MSEtool:::CalcMSYCpp(hist_no_msy, Years = MSYYears, type = 'Removals', parallel = FALSE, silent = TRUE)

    expect_equal(msy_r, msy_cpp, tolerance = 1e-6,
                info = sprintf("%s, Seasons=%d", data_name, seasons))
  }

  check_msy_seasonal("SeasonalSpatialOM", 12, 2)
  check_msy_seasonal("ComplexOM", 4, 2)
  check_msy_seasonal("HermOM", 4, 1)
  check_msy_seasonal("MultiStockOM", 2, 2)
})

test_that("SRR@SPFrom is stored as a portable stock name after .OM2Hist(), not a global numeric index", {
  # Regression test for the SPFrom bug itself (R/constructor-hist.R,
  # R/utils-prep-hist-misc.R) -- general Hist-construction infrastructure,
  # not specific to the C++ MSY path, but discovered and fixed alongside it.
  skip_on_cran()
  data(MultiStockOM, envir = environment())
  om <- MultiStockOM
  om@Seasons <- 2
  om@nSim <- 1
  set.seed(1)
  hist_no_msy <- Simulate(om, silent = TRUE, control = SimControl(MSYRefs = FALSE))

  stock_names <- StockNames(hist_no_msy)
  for (i in seq_along(hist_no_msy@OM@Stock)) {
    spf <- hist_no_msy@OM@Stock[[i]]@SRR@SPFrom
    expect_type(spf, "character")
    expect_true(spf %in% stock_names)
  }

  # Hist@Misc$SPFrom is the compensating fix -- the C++ dynamics engine reads
  # this as a global numeric index over the full stock list, so it must stay
  # numeric even though @SRR@SPFrom is now a name.
  # (Misc is stripped by the time Simulate() returns; recompute it directly.)
  hist_prepped <- MSEtool:::.PrepHistMisc(hist_no_msy)
  expect_type(hist_prepped@Misc$SPFrom, "integer")
  expect_true(all(hist_prepped@Misc$SPFrom %in% seq_along(stock_names)))

  # The actual reported bug: each stock alone in its own complex (no
  # `@Complexes` grouping) used to throw "subscript out of bounds".
  MSYYears <- unique(floor(utils::tail(Years(hist_no_msy@OM, 'Historical'), 1)))
  expect_no_error(
    CalcMSY(hist_no_msy, Years = MSYYears, type = 'Removals', parallel = FALSE, silent = TRUE)
  )
})
