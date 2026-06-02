#' Add a Fleet to an Operating Model
#'
#' Adds a new fleet to an [om-class] object, appending a 
#' [fleet-class], an [obs-class], and an [imp-class] object for every 
#' stock in the OM.
#'
#' @param OM An object of class `om`.
#' @param FleetName A length-1 character string giving the name of the new
#'   fleet. Must not already exist in `OM`.
#' @param Fleet Optional. An object of class `fleet`. If `NULL` (default), an
#'   empty `fleet` object is created with `Name = FleetName`.
#' @param Obs Optional. An [obs-class] object. If `NULL` (default), an
#'   empty `obs` object is created with `Name = FleetName`.
#' @param Imp Optional. An [imp-class] object. If `NULL` (default), an
#'   empty `imp` object is created with `Name = FleetName`
#' @return The `om` object with the new fleet appended to `OM@Fleet` and
#'   `OM@Obs` for every stock.
#' @examples
#' OM <- SingleStockOM
#' FleetNames(OM)
#' OM2 <- AddFleet(OM, 'DummyFleet')
#' FleetNames(OM2)
#' 
#' @export
AddFleet <- function(OM, FleetName, Fleet=NULL, Obs=NULL, Imp=NULL) {
  
  CheckClass(OM)
  OM <- Populate(OM, silent=TRUE)
  CheckClass(FleetName, 'character', 'FleetName')
  
  if (length(FleetName)!=1)
    cli::cli_abort('`FleetName` must be length 1')
  
  exists <- FleetName %in% FleetNames(OM)
  
  if (any(exists))
    cli::cli_abort(c('x'='FleetName {.val {FleetName}} already exists in this OM object'))
  
  # Add Fleet
  if (is.null(Fleet)) 
    Fleet <- Fleet(Name=FleetName) # empty `fleet-class` object
    
  Fleet <- Fleet |> Add_Dummy_Effort(OM) |> Add_Dummy_Selectivity(OM)
    
  CheckClass(Fleet, 'fleet', 'Fleet')
  
  newFleet <- list(Fleet)
  names(newFleet) <- FleetName
  
  OM@Fleet <- purrr::map(OM@Fleet, \(FleetList)
                         c(FleetList, newFleet))
  
  # Add Obs
  if (is.null(Obs))
    Obs <- Obs(Name=FleetName) # empty `obs-class` object
  
  CheckClass(Obs, 'obs', 'Obs') 
  
  newObs <- list(Obs)
  names(newObs) <- FleetName
  OM@Obs <- purrr::map(OM@Obs, \(ObsList)
                       c(ObsList, newObs))
  
  # Add Imp
  if (is.null(Imp))
    Imp <- Imp(Name=FleetName) # empty `imp-class` object
  
  CheckClass(Imp, 'imp', 'Imp') 
  
  newImp <- list(Imp)
  names(newImp) <- FleetName
  OM@Imp <- purrr::map(OM@Imp, \(ImpList)
                       c(ImpList, newImp))
 
  
  OM
}

Add_Dummy_Effort <- function(Fleet, OM) {
  if (!EmptyObject(Fleet@Effort))
    return(Fleet)
  
  if (!is.null(Fleet@Effort@Effort))
    return(Fleet)
  
  copy_fleet <- OM@Fleet[[1]][[1]]
  copy_fleet@Effort@Effort[] <- 0
  Fleet@Effort@Effort <- ReduceDims(copy_fleet@Effort@Effort)
  
  copy_fleet@Catchability@Efficiency[] <- 1
  Fleet@Catchability@Efficiency <- ReduceDims(copy_fleet@Catchability@Efficiency)
  
  Fleet
}

Add_Dummy_Selectivity <- function(Fleet, OM) {
  if (!EmptyObject(Fleet@Selectivity))
    return(Fleet)
  
  if (!is.null(Fleet@Selectivity@MeanAtAge))
    return(Fleet)
  
  copy_fleet <- OM@Fleet[[1]][[1]]
  copy_fleet@Selectivity@MeanAtAge[] <- 0
  Fleet@Selectivity@MeanAtAge <- ReduceDims(copy_fleet@Selectivity@MeanAtAge)

  Fleet
}
