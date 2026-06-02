#' Access Stock or Fleet Names 
#'
#' @details
#' These functions extract stock or fleet names from objects used in the MSE
#' framework, including [om-class], [hist-class], [stock-class], [fleet-class], and [mse-class]
#' objects
#'
#' **StockNames**
#'
#' Returns the names of stocks contained in the object.
#'
#' **FleetNames**
#'
#' Returns the names of fleets contained in the object. For objects containing
#' multiple stocks, a list of character vectors is returned.
#'
#' @param object An [om-class], [hist-class], [stock-class], [fleet-class], or [mse-class] object.
#'
#' @return
#' * `StockNames()`: a character vector of stock names
#' * `FleetNames()`: a character vector of fleet names (always taken from the first stock)
#'
#'
#' @examples
#' StockNames(SingleStockOM)
#' FleetNames(SingleStockOM)
#'
#' @name name-accessors
#' @rdname name-accessors
#' @export
StockNames <- function(object) {
  if (inherits(object, c("hist", "mse"))) {
    return(names(object@OM@Stock))
  }
  
  if (inherits(object, "om")) {
    if (inherits(object@Stock, 'stock')) 
      return(object@Stock@Name)
      
    nms <- names(object@Stock)
    if (!is.null(nms))
      return(nms)
    
    return(
      purrr::map_chr(object@Stock, Name)  
    )
    
  }
  
  if (inherits(object, "StockList")) {
    return(names(object))
  }
  
  NULL
}

#' @rdname name-accessors
#' @param IncSurvey Logical. Include names of survey (non-fishing) fleets? Default: FALSE
#' @export
FleetNames <- function(object, IncSurvey = FALSE) {
  
  if (inherits(object, c("hist", "mse")))
    return(Recall(object@OM, IncSurvey = IncSurvey))
  
  if (inherits(object, "om")) {
    
    if (inherits(object@Fleet, "fleet")) {
      fleetnames <- object@Fleet@Name
    } else if (is.list(object@Fleet)) {
      fleetnames <- names(object@Fleet[[1]])  
      if (is.null(fleetnames)) {
        fleetnames <- purrr::map_chr(object@Fleet[[1]], Name)
      }
    } else {
      cli::cli_alert_warning("Unrecognised structure in {.val object@Fleet}; cannot extract fleet names.")
      return(NULL)
    }
    
    if (IncSurvey && !is.null(object@Data)) {
      surveynames <- lapply(object@Data, \(st) st@Survey@Name) |>
        unlist() |> unique()
      
      cpuenames <- lapply(object@Data, \(st) st@CPUE@Name) |>
        unlist() |> unique()
      
      fleetnames <- unique(c(fleetnames, surveynames, cpuenames))
    }
    return(fleetnames)
  }
 
  if (inherits(object, "FleetList"))
    return(names(object))
  
  if (inherits(object, "StockList"))
    return(lapply(object, names))  
  
  cli::cli_alert_warning("No FleetNames method for object of class {.cls {class(object)}}")
  
  invisible(NULL)
}