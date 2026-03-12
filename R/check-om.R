#' Check OM object is complete
#'
#' @param OM An object of class `OM` 
#' @param msg Logical. Display messages?
#' @param stop_if_missing Logical. Stop with error is values are missing and there is no default?
#'
#' @return The OM object with default values (if needed)
#' @export
#'
#' @examples
#' ExampleOM <- CheckOM(ExampleOM)
CheckOM <- function(OM, msg=TRUE, stop_if_missing=TRUE) {
  CheckClass(OM, c('OM', 'om'))
  
  if (inherits(OM, 'OM'))
    return(
      CheckOM_legacy(OM, msg, stop_if_missing)
    )
  
  OM <- UpdateObject(OM)
  
  # Stock 
  MissingStock <- CheckStock(Stock=OM@Stock)
  if (!length(MissingStock))
    MissingStock <- NULL
  
  # Fleet 
  MissingFleet <- CheckFleet(OM@Fleet)
  if (!length(MissingFleet))
    MissingFleet <- NULL
  
  list(MissingStock = MissingStock,
       MissingFleet = MissingFleet)
}

CheckStock <- function(Stock) {
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
  df <- data.frame(Slot= c('Ages', 'Length', 'Weight', 'NaturalMortality', 'Maturity', 'SRR'),
                   Missing= FALSE)
  
  for (i in seq_len(nrow(df))) {
    if (EmptyObject(slot(Stock, df$Slot[i])))
      df$Missing[i] <- TRUE
  }
  dplyr::filter(df, Missing==TRUE) |> dplyr::pull(Slot)
}

CheckFleet <- function(Fleet) {
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

