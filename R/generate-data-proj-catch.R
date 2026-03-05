#' Generate Projected Catch Data for a Stock
#'
#' Appends a new year of observed catch data (`Value` and `CV`) to the existing
#' landings or discards data object for a given simulation and stock/complex.
#'
#' @param x Integer. Simulation index.
#' @param Proj A `Hist` object used in the projection
#' @param DataYear Numeric. The calendar year to generate data for.
#' @param YearsAll Numeric vector. All calendar years in the historical
#'  and projection years.
#' @param i Integer. Stock/complex index.
#' @param stocks Integer vector. Stock indices used to subset catch-at-age arrays.
#' @param type Character. One of `"Landings"` or `"Discards"`.
#'
#' @return The updated `Landings` or `Discards` object. 
#' @keywords internal
GenProjData_Catch <- function(x, 
                              Proj, 
                              DataYear,
                              YearsAll,
                              i,
                              stocks, 
                              type=c('Landings', 'Discards')) {
  
  type      <- match.arg(type)
  CatchData <- slot(Proj@Data[[x]][[i]], type)
  
  if (EmptyObject(CatchData)) return(CatchData)
  if (DataYear %in% dimnames(CatchData@Value)[[1]]) return(CatchData)
  
  TSIndex    <- match(DataYear, YearsAll)
  nArea      <- nArea(Proj)
  Value      <- CatchData@Value
  CV         <- CatchData@CV
  FleetNames <- resolveFleetNames(CatchData)
  nFleet     <- length(FleetNames)
  CatchData  <- resolveUnits(CatchData, nFleet)
  
  Real_Catch_Number <- purrr::map( slot(Proj, paste0(type, 'AtAge'))[stocks],
                                   \(catch_n) {
                                     catch_n[x,,TSIndex,,,drop=FALSE] |> 
                                       abind::adrop(drop=c(1,3))
  }) 
  
  NewValue <- emptyFleetArray(DataYear, FleetNames)
  NewCV    <- emptyFleetArray(DataYear, FleetNames)
  
  for (fl in seq_len(nFleet)) {
    Obs <- slot(Proj@OM@Obs[[i]][[fl]], type)
    if (EmptyObject(Obs) || length(Obs@Error) < 1) next
    
    omData   <- Proj@OM@Data[[i]]
    
    hasOMVal <- !is.null(omData) &&
      !is.null(slot(omData, type)@Value) &&
      nrow(slot(omData, type)@Value) >= TSIndex
    
    if (hasOMVal) {
      NewValue[, fl] <- slot(omData, type)@Value[TSIndex, fl]
    } else {
      error <- ArraySubsetYear(Obs@Error, DataYear)[x]
      bias  <- Obs@Bias[x]
      
      NewValue[, fl] <- switch(CatchData@Units[fl],
                               Number  = resolveCatchNumber(Real_Catch_Number, fl) * error * bias,
                               Biomass = resolveCatchBiomass(Proj, stocks, x, TSIndex, fl, nArea,
                                                             Real_Catch_Number) * error * bias
      )
      
    }
    NewCV[, fl] <- resolveCV(Proj, type, i, fl, TSIndex, CatchData, DataYear)
  } 
  
  CatchData@Value <- abind::abind(Value, NewValue, along=1, use.dnns=TRUE)
  CatchData@CV <- abind::abind(CV, NewCV, along=1, use.dnns=TRUE)
  CatchData
}