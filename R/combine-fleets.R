
# library(MSEtool)


# OM <- readRDS('../NPSWO/Objects_OM/Base.om')
 

# Name <- "JPN_WCNPO_OSDWCOLL_Area1"
# FleetInds <- c(6, 1)
# 
# FleetInds <- c('F6_JPN_WCNPO_OSDWLL_early_Area1',
#                'F1_JPN_WCNPO_OSDWCOLL_late_Area1')

CombineFleets <- function(OM, Name, FleetInds, silent=FALSE) {
  fleetnames <- FleetNames(OM)
  
  if (is.character(FleetInds)) {
    fleetInd <- match(FleetInds, fleetnames)
    if (any(is.na(fleetInd)))
      cli::cli_abort(c("x"="Could not match `FleetInds` with existing fleet names",
                       "i"="FleetInds: {.val {FleetInds}}",
                       "i"="Existing Fleet names: {.val {fleetnames}}"
                       ))
    FleetInds <- fleetInd
  }
    
  if (is.numeric(FleetInds)) {
    if (!all(FleetInds %in% seq_along(fleetnames)))
      cli::cli_abort(c("x"="Could not match `FleetInds` with existing fleet indices",
                       "i"="FleetInds: {.val {FleetInds}}",
                       "i"="Existing Fleets: {.val {seq_along(fleetnames)}}"
      ))
  }
  
  combfleets <- fleetnames[FleetInds]
  
  if (!silent)
    cli::cli_alert_info('Combining fleets {.val {combfleets}} into new fleet {.val {Name}}')
  
  nstock <- nStock(OM)
  
  
  
  
  

}

CombineFleets_Stock <- function(st, OM, Name, FleetInds) {
  FleetList <- OM@Fleet[[st]][FleetInds]
  NewFleet <- Fleet(Name=Name)
  
  apicalFList <- purrr::map(FleetList, \(fleet) {
    ArrayMultiply(fleet@Effort@Effort, fleet@Catchability@Efficiency)
  })
  
  totalApicalF <- purrr::reduce(apicalFList, `+`)
  
  # Use q from first fleet
  Efficiency <- FleetList[[1]]@Catchability@Efficiency
  
  Effort(Effort=ArrayDivide(totalApicalF, Efficiency))
  
  

  
  Catchability(Efficiency = FleetList[[1]]@Catchability@Efficiency)
  
  total_effort <- purrr::reduce(
    purrr::map(FleetList, ~ .x@Effort@Effort), `+`
  )
  
  cbind(FleetList[[1]]@Catchability@Efficiency[1,],
        FleetList[[2]]@Catchability@Efficiency[1,])
  
  
  
  
}

