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
  if (length(DataSlot@Units) == nFleet) return(DataSlot)
  DataSlot@Units <- if (is.null(DataSlot@Units)) {
    rep(default, nFleet)
  } else {
    rep(DataSlot@Units, nFleet)[seq_len(nFleet)]
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


.ResolveValue <- function(Proj, slotname, i, fl, TSIndex, Obs, x, DataYear) {
  omData <- Proj@OM@Data[[i]]
  
  if (!is.null(omData)) {
    omDataSlot <- slot(omData, slotname)@Value
    
    if (!is.null(omDataSlot) &&
        nrow(omDataSlot) >= TSIndex &&
        ncol(omDataSlot) >= fl) {
      return(omDataSlot[TSIndex, fl])
    }
  }
  
  obsError <- .ArraySubsetYear(Obs@Error, DataYear)
  sim_ind <- min(x, nrow(obsError))
  obsError <- obsError[sim_ind]
  
  sim_ind <- min(x, length(Obs@Bias))
  obsBias  <- Obs@Bias[sim_ind]
  
  projValue <- slot(Proj, slotname)[x, TSIndex, fl]
  projValue * obsError * obsBias
}

.ResolveCV <- function(Proj, slotname, i, fl, TSIndex, DataObject, DataYear, default=0.2) {
  omData <- Proj@OM@Data[[i]]
  
  if (!is.null(omData)) {
    omDataSlot <- slot(omData, slotname)@CV
    
    if (!is.null(omDataSlot) &&
        nrow(omDataSlot) >= TSIndex &&
        ncol(omDataSlot) >= fl) {
      return(omDataSlot[TSIndex, fl])
    }
  }
  
  # use the CV from last time step
  previouscv <- DataObject@CV[,fl]
  if (is.null(previouscv)) 
    return(default)
  
  previouscv <- previouscv[!is.na(previouscv)] |> utils::tail(1) |> as.numeric()    
  previouscv
}

.ResolveCatchNumber <- function(Real_Catch_Number, fl) {
  purrr::map(Real_Catch_Number, \(catch_n) {
    catch_n[,fl,, drop=FALSE] |> sum()
  }) |> List2Array('Stock') |> sum()
}

.ResolveCatchBiomass <- function(Proj, stocks, x, TSIndex, fl, nArea, Real_Catch_Number,
                                type = c('Landings', 'Discards')) {
  type <- match.arg(type)
  # Landings use the retention-weighted schedule; Discards use the
  # selectivity-weighted schedule -- see the equivalent note in
  # .GenHistDataCatch().
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

.ResolveSelectivity <- function(Proj, stocks, StockNames, Obs, FleetNames, fl,
                               x, TSIndex, nArea) {
  SelectivityAtAge <- Obs@Selectivity
  
  if (is.character(SelectivityAtAge)) {
    switch(SelectivityAtAge,
           Biomass = purrr::map(seq_along(stocks), \(st) {
             AgeClasses <- Proj@OM@Stock[[stocks[st]]]@Ages@Classes
             array(1, c(length(AgeClasses), nArea),
                   dimnames = list(Age = AgeClasses, Area = seq_len(nArea)))
           }) |> setNames(StockNames[stocks]),
           
           SBiomass = purrr::map(seq_along(stocks), \(st) {
             Proj@OM@Stock[[stocks[st]]]@Maturity@MeanAtAge[x, , TSIndex, drop = FALSE] |>
               AddDimension("Area") |>
               DropDimension(c("Sim", "Year")) |>
               ExtendAreas(Areas = seq_len(nArea))
           }) |> setNames(StockNames[stocks]),
           
           Obs = purrr::map(Obs@Selectivity, \(stock) {
             stock[x, , TSIndex, drop = FALSE] |>
               AddDimension("Area") |>
               DropDimension(c("Sim", "Year")) |>
               ExtendAreas(Areas = seq_len(nArea))
           }),
           
           cli::cli_abort("Unknown selectivity type: {.val {SelectivityAtAge}}", .internal = TRUE)
    )
  } else {
    purrr::map(Proj@OM@Fleet[stocks], \(fleet_list) {
      sel <- fleet_list[[FleetNames[fl]]]@Selectivity@MeanAtAge
      dd <- dim(sel)
      x_sim <- pmin(x, dd[1])
      sel[x_sim, , TSIndex, , drop = FALSE] |>
        DropDimension(c("Sim", "Year"))
    })
  }
}
