resolveFleetNames <- function(DataSlot) {
  if (!is.null(DataSlot@Name)) return(DataSlot@Name)
  paste("Fleet", seq_len(dim(DataSlot@Value)[2]))
}

emptyFleetArray <- function(DataYear, FleetNames) {
  array(NA,
        dim = c(1, length(FleetNames)),
        dimnames = list(Year = DataYear, Fleet = FleetNames)
  )
}

resolveUnits <- function(DataSlot, nFleet, default='Biomass', valid=c('Biomass', 'Number')) {
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


resolveValue <- function(Proj, slotname, i, fl, TSIndex, Obs, x, DataYear) {
  omData <- Proj@OM@Data[[i]]
  
  if (!is.null(omData)) {
    omDataSlot <- slot(omData, slotname)@Value
    
    if (!is.null(omDataSlot) &&
        nrow(omDataSlot) >= TSIndex &&
        ncol(omDataSlot) >= fl) {
      return(omDataSlot[TSIndex, fl])
    }
  }
  
  obsError <- ArraySubsetYear(Obs@Error, DataYear)[x]
  obsBias  <- Obs@Bias[x]
  
  projValue <- slot(Proj, slotname)[x, TSIndex, fl]
  projValue * obsError * obsBias
  
}

resolveCV <- function(Proj, slotname, i, fl, TSIndex, DataObject, DataYear, default=0.2) {
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
  
  previouscv <- previouscv[!is.na(previouscv)] |> tail(1) |> as.numeric()    
  previouscv
}

resolveCatchNumber <- function(Real_Catch_Number, fl) {
  purrr::map(Real_Catch_Number, \(catch_n) {
    catch_n[,fl,, drop=FALSE] |> sum()
  }) |> List2Array('Stock') |> sum()
}

resolveCatchBiomass <- function(Proj, stocks, x, TSIndex, fl, nArea, Real_Catch_Number) {
  purrr::map2(Real_Catch_Number, Proj@OM@Fleet[stocks], \(catch_n, FleetList) {
    fleet       <- FleetList[[fl]]
    catch_fleet <- catch_n[, fl, , drop = FALSE] |> abind::adrop(2)
    fleetwght   <- fleet@WeightFleet
    flwsim      <- min(dim(fleetwght)[1], x)
    fleetwght   <- fleet@WeightFleet[flwsim, , TSIndex, drop = FALSE] |>
      abind::adrop(c(1, 3), one.d.array = TRUE) |>
      AddDimension("Area") |>
      ExtendAreas(1:nArea)
    ArrayMultiply(catch_fleet, fleetwght)
  }) |>
    List2Array("Stock") |>
    sum()
}

resolveSelectivity <- function(Proj, stocks, StockNames, Obs, FleetNames, fl,
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
      fleet_list[[FleetNames[fl]]]@Selectivity@MeanAtAge[x, , TSIndex, , drop = FALSE] |>
        DropDimension(c("Sim", "Year"))
    })
  }
}
