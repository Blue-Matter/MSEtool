
#' Generate Projected Effort Data for a Stock/Complex
#'
#' Appends a new year of observed effort data (`Value` and `CV`) to the existing
#' effort data object for a given simulation and stock/complex.
#'
#' @param x Integer. Simulation index.
#' @param Proj A `Hist` object used in the projection
#' @param DataYear Numeric. The calendar year to generate data for.
#' @param YearsAll Numeric vector. All calendar years in the historical
#'  and projection years.
#' @param i Integer. Stock/complex index.
#'
#' @return The updated `Effort` data object.
#' @keywords internal
GenProjData_Effort <- function(x, Proj, DataYear, YearsAll, i) {
  
  EffortData <- Proj@Data[[x]][[i]]@Effort
  
  if (EmptyObject(EffortData)) return(EffortData)
  if (DataYear %in% dimnames(EffortData@Value)[[1]]) return(EffortData)
  
  TSIndex     <- match(DataYear, YearsAll)
  Value       <- EffortData@Value
  CV          <- EffortData@CV
  FleetNames  <- resolveFleetNames(EffortData)
  nFleet      <- length(FleetNames)
  
  NewValue <- emptyFleetArray(DataYear, FleetNames)
  NewCV    <- emptyFleetArray(DataYear, FleetNames)

  for (fl in seq_len(nFleet)) {
    Obs <- Proj@OM@Obs[[i]][[fl]]@Effort
    if (EmptyObject(Obs) || length(Obs@Error) < 1) next
    
    NewValue[, fl] <- resolveValue(Proj, 'Effort', i, fl, TSIndex, Obs, x, DataYear)
    NewCV[, fl]    <- resolveCV(Proj, 'Effort', i, fl, TSIndex, EffortData, DataYear)
  }
  
  EffortData@Value <- abind::abind(Value, NewValue, along = 1, use.dnns = TRUE)
  EffortData@CV    <- abind::abind(CV, NewCV, along = 1, use.dnns = TRUE)
  EffortData
}