#' Check OM object is complete
#'
#' For an [om-class] object, reports which `Stock`, `Fleet`, `Obs`, and `Imp`
#' components are missing or incomplete, as a nested list matching the
#' `[[stock/complex]][[fleet]]` structure of `OM@Fleet`/`OM@Obs`/`OM@Imp`.
#' `Stock`/`Fleet` list required slots that [PopulateOM()] cannot fill on its
#' own; `Obs`/`Imp` have no such required slots (every unset slot legitimately
#' means "no observation"/"perfect implementation").
#'
#' @param OM An object of class `OM`
#' @param msg Logical. Display messages?
#' @param stop_if_missing Logical. Stop with error is values are missing and there is no default?
#'
#' @return For an [om-class] `OM`: a list with elements `MissingStock`,
#'   `MissingFleet`, `MissingObs`, `MissingImp` (each `NULL` if nothing is
#'   missing). 
#' @export
#'
#' @examples
#' SingleStockOM <- CheckOM(SingleStockOM)
CheckOM <- function(OM, msg=TRUE, stop_if_missing=TRUE) {
  .CheckClass(OM, c('OM', 'om'))
  
  if (inherits(OM, 'OM'))
    return(
      CheckOM_legacy(OM, msg, stop_if_missing)
    )
  
  OM <- UpdateObject(OM)
  
  # Stock 
  MissingStock <- .CheckStock(Stock=OM@Stock)
  if (!length(MissingStock))
    MissingStock <- NULL
  
  # Fleet
  MissingFleet <- .CheckFleet(OM@Fleet)
  if (!length(MissingFleet))
    MissingFleet <- NULL

  # Obs
  MissingObs <- .CheckOMObs(OM@Obs)
  if (!length(MissingObs))
    MissingObs <- NULL

  # Imp
  MissingImp <- .CheckOMImp(OM@Imp)
  if (!length(MissingImp))
    MissingImp <- NULL

  list(MissingStock = MissingStock,
       MissingFleet = MissingFleet,
       MissingObs   = MissingObs,
       MissingImp   = MissingImp)
}

.CheckStock <- function(Stock) {
  if (is.null(Stock))
    return('None Specified')
  
  if (is.list(Stock)) {
    nms <- names(Stock)
    if (is.null(nms))
      nms <- paste('Stock', 1:length(Stock))
    
    list <- MakeNamedList(nms)
    for (j in seq_along(list)) {
      list[[j]] <- Recall(Stock[[j]])
    }
    return(list)
  }
  
  # Required Slots 
  Missing <- Slot <- NULL # CRAN check hacks
  df <- data.frame(Slot    = c('Ages',
                               'Weight', 
                               'NaturalMortality',
                               'Maturity', 
                               'SRR'),
                   Missing = FALSE)
  
  for (i in seq_len(nrow(df))) {
    if (EmptyObject(slot(Stock, df$Slot[i])))
      df$Missing[i] <- TRUE
  }
  dplyr::filter(df, Missing==TRUE) |> dplyr::pull(Slot)
}

.CheckFleet <- function(Fleet) {
  if (is.null(Fleet))
    return('None Specified')
  
  if (is.list(Fleet)) {
    if (!is.list(Fleet[[1]])) {
      Fleet <- list(Fleet)
    }

    stocknames <- names(Fleet)
    if (is.null(stocknames))
      stocknames <- paste('Stock', 1:length(Fleet))
    
    stockfleetlist <- MakeNamedList(stocknames)
    
    for (i in seq_along(stocknames)) {
      stockfleet <- Fleet[[i]]
      fleetnames <- names(stockfleet)
      if (is.null(fleetnames))
        fleetnames <- paste('Fleet', 1:length(stockfleet))
      
      stockfleetlist[[i]] <- MakeNamedList(fleetnames)
      for (j in seq_along(fleetnames)) {
        
        stockfleetlist[[i]][[j]] <- Recall(stockfleet[[j]])
      }
    }
    return(stockfleetlist)
  }
  
  # Required Slots
  Missing <- Slot <- NULL # CRAN check hacks
  df <- data.frame(Slot= c('Effort', 'Selectivity'),
                   Missing= FALSE)

  for (i in seq_len(nrow(df))) {
    if (EmptyObject(slot(Fleet, df$Slot[i])))
      df$Missing[i] <- TRUE
  }
  dplyr::filter(df, Missing==TRUE) |> dplyr::pull(Slot)
}

.CheckOMObs <- function(Obs) {
  if (is.null(Obs))
    return('None Specified')

  if (is.list(Obs)) {
    if (!is.list(Obs[[1]]))
      Obs <- list(Obs)

    complexnames <- names(Obs)
    if (is.null(complexnames))
      complexnames <- paste('Complex', seq_along(Obs))

    complexlist <- MakeNamedList(complexnames)
    for (i in seq_along(complexnames)) {
      fleetobs   <- Obs[[i]]
      fleetnames <- names(fleetobs)
      if (is.null(fleetnames))
        fleetnames <- paste('Fleet', seq_along(fleetobs))

      complexlist[[i]] <- MakeNamedList(fleetnames)
      for (j in seq_along(fleetnames))
        complexlist[[i]][[j]] <- Recall(fleetobs[[j]])
    }
    return(complexlist)
  }

  if (EmptyObject(Obs))
    return('No observation model specified (perfect information)')

  character(0)
}

.CheckOMImp <- function(Imp) {
  if (is.null(Imp))
    return('None Specified')

  if (is.list(Imp)) {
    if (!is.list(Imp[[1]]))
      Imp <- list(Imp)

    complexnames <- names(Imp)
    if (is.null(complexnames))
      complexnames <- paste('Complex', seq_along(Imp))

    complexlist <- MakeNamedList(complexnames)
    for (i in seq_along(complexnames)) {
      fleetimp   <- Imp[[i]]
      fleetnames <- names(fleetimp)
      if (is.null(fleetnames))
        fleetnames <- paste('Fleet', seq_along(fleetimp))

      complexlist[[i]] <- MakeNamedList(fleetnames)
      for (j in seq_along(fleetnames))
        complexlist[[i]][[j]] <- Recall(fleetimp[[j]])
    }
    return(complexlist)
  }

  if (EmptyObject(Imp))
    return('No implementation error specified (perfect implementation)')

  character(0)
}
