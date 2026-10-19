.ResolveFleetNames <- function(DataSlot) {
  if (!is.null(DataSlot@Name)) return(DataSlot@Name)
  paste("Fleet", seq_len(dim(DataSlot@Value)[2]))
}

.EmptyFleetArray <- function(DataYear, FleetNames) {
  array(NA,
        dim = c(1, length(FleetNames)),
        dimnames = list(Year = DataYear, Fleet = FleetNames)
  )
}

.ResolveUnits <- function(DataSlot, nFleet, default='Biomass', valid=c('Biomass', 'Number')) {
  isCatch <- is(DataSlot, 'catchdata')
  if (length(DataSlot@Units) == nFleet && !isCatch) return(DataSlot)
  if (length(DataSlot@Units) != nFleet) {
    DataSlot@Units <- if (is.null(DataSlot@Units)) {
      rep(default, nFleet)
    } else {
      rep(DataSlot@Units, nFleet)[seq_len(nFleet)]
    }
  }

  if (isCatch) {
    .CatchUnitType(DataSlot@Units)
    return(DataSlot)
  }

  if (any(is.na(DataSlot@Units)) || !all(DataSlot@Units %in% valid))
    cli::cli_abort(
      c("Invalid {.val Units} in {.cls {class(DataSlot)}} object.",
        "i" = "Valid units are: {.val {valid}}.",
        "x" = "Found: {.val {unique(DataSlot@Units)}}."
      ),
      .internal = TRUE
    )
  
  DataSlot
}


.ResolveValue <- function(Proj, slotname, i, fl, TSIndex, Obs, nSim, DataYear) {
  omData <- Proj@OM@Data[[i]]

  if (!is.null(omData)) {
    omDataSlot <- slot(omData, slotname)@Value
    if (!is.null(omDataSlot) &&
        nrow(omDataSlot) >= TSIndex &&
        ncol(omDataSlot) >= fl) {
      return(rep(omDataSlot[TSIndex, fl], nSim))
    }
  }

  obsError <- .ArraySubsetYear(Obs@Error, DataYear)
  sim_ind  <- pmin(seq_len(nSim), nrow(obsError))
  obsError <- obsError[sim_ind]

  sim_ind_bias <- pmin(seq_len(nSim), length(Obs@Bias))
  obsBias      <- Obs@Bias[sim_ind_bias]

  projValue <- slot(Proj, slotname)[seq_len(nSim), TSIndex, fl]
  projValue * obsError * obsBias
}

.ResolveCV <- function(Proj, slotname, i, fl, TSIndex, DataObject, DataYear, Obs = NULL) {
  omData <- Proj@OM@Data[[i]]

  if (!is.null(omData)) {
    omDataSlot <- slot(omData, slotname)@CV

    if (!is.null(omDataSlot) &&
        nrow(omDataSlot) >= TSIndex &&
        ncol(omDataSlot) >= fl &&
        !is.na(omDataSlot[TSIndex, fl])) {
      return(omDataSlot[TSIndex, fl])
    }
  }

  ObsCV <- .DataCVYears(Obs, DataYear, NA_real_)
  if (!is.na(ObsCV)) return(ObsCV)

  # use the CV from last time step
  previouscv <- DataObject@CV[, fl]
  previouscv <- previouscv[!is.na(previouscv)]
  if (length(previouscv))
    return(as.numeric(utils::tail(previouscv, 1)))

  .DefaultDataCV(Proj, slotname)
}

.DefaultDataCV <- function(object, type) {
  DataCV <- .ExpandDataCV(object@OM@Control$DataCV %||% 0.2)
  DataCV[[type]]
}

# reported CV per year: the Obs `DataCV` (scalar, or values from each named year onwards), else `Default`
.DataCVYears <- function(Obs, Years, Default) {
  DataCV <- if (!is.null(Obs) && methods::.hasSlot(Obs, "DataCV")) Obs@DataCV
  out    <- rep(Default, length(Years))
  if (!length(DataCV)) return(out)
  if (is.null(names(DataCV))) return(rep(DataCV[1], length(Years)))
  ChangeYears <- as.numeric(names(DataCV))
  Ord <- order(ChangeYears)
  Ind <- findInterval(as.numeric(Years), ChangeYears[Ord])
  out[Ind > 0] <- DataCV[Ord][Ind[Ind > 0]]
  out
}

.ResolveCatchNumber <- function(Real_Catch_Number_All, fl, nSim) {
  per_stock <- purrr::map(Real_Catch_Number_All, \(catch_n) {
    catch_n[, , fl, , drop = FALSE] |> abind::adrop(drop = 3)  # [Sim, Age, Area]
  })
  totals_by_stock <- purrr::map(per_stock, \(a) apply(a, 1, sum))  # length-nSim per stock
  Reduce(`+`, totals_by_stock)
}

.ResolveCatchBiomass <- function(Proj, stocks, x, TSIndex, fl, nArea, Real_Catch_Number,
                                type = c('Landings', 'Discards')) {
  type <- match.arg(type)
  weight_slot <- if (type == 'Landings') 'WeightFleetRetained' else 'WeightFleetSelected'
  purrr::map2(Real_Catch_Number, Proj@OM@Fleet[stocks], \(catch_n, FleetList) {
    fleet       <- FleetList[[fl]]
    catch_fleet <- catch_n[, fl, , drop = FALSE] |> abind::adrop(2)
    fleetwght   <- slot(fleet, weight_slot)
    flwsim      <- min(dim(fleetwght)[1], x)
    fleetwght   <- slot(fleet, weight_slot)[flwsim, , TSIndex, drop = FALSE] |>
      abind::adrop(c(1, 3), one.d.array = TRUE) |>
      AddDimension("Area") |>
      ExtendAreas(1:nArea)
    ArrayMultiply(catch_fleet, fleetwght) |> SumOverAge()
  }) |>
    List2Array("Stock") |>
    sum()
}

# calendar years between the last historical year and the last historical observation; Inf if none
.ObsGap <- function(Years, Observed, LastHistYear) {
  Years <- as.numeric(Years)
  Hist  <- Observed & Years <= LastHistYear
  if (!any(Hist, na.rm = TRUE)) return(Inf)
  floor(LastHistYear) - floor(max(Years[which(Hist)]))
}

.ExceedsMaxGap <- function(object, Gap) {
  MaxGap <- if (methods::.hasSlot(object, "MaxGap")) object@MaxGap
  MaxGap <- MaxGap %||% 5
  !is.na(MaxGap) && Gap > MaxGap
}

# composition series with no historical observations are not subject to `MaxGap`
.CompExceedsMaxGap <- function(CompData, fl, Obs, LastHistYear) {
  Value  <- CompData@Value
  Totals <- apply(Value[, fl, , drop = FALSE], 1, sum, na.rm = TRUE)
  Gap    <- .ObsGap(dimnames(Value)[[1]], Totals > 0, LastHistYear)
  is.finite(Gap) && .ExceedsMaxGap(Obs, Gap)
}

.CompShiftAt <- function(Shift, x, DataYear, nBin) {
  if (is.null(Shift)) return(rep(0, nBin))
  Shift <- .ArraySubsetYear(Shift, DataYear)
  dn    <- names(dimnames(Shift))
  dims  <- match(c('Sim', 'Year'), dn)
  sim   <- min(x, dim(Shift)[dims[1]])
  as.numeric(abind::asub(Shift, list(sim, 1), dims = dims, drop = FALSE))[seq_len(nBin)]
}
