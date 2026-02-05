#' Check and log errors from a Management Procedure
#'
#' Internal helper function to standardize error handling for Management Procedures (MPs).  
#' If the MP returns a valid `Advice` object, nothing happens. If the MP returns a different  
#' type or a `try-error`, a descriptive error message is thrown including the MP name, data,  
#' simulation, year, and original error message.
#'
#' @param MPAdvice The result returned by the MP. Should be of class `advice`.
#' @param MPName Character. Name of the Management Procedure.
#' @param Data An object containing the simulation data (typically an OM or Data object).
#' @param Sim Integer. Simulation number.
#' @param Year Integer. Current simulation year.
#'
#' @return
#' Stops execution with a descriptive error if `MPAdvice` is invalid. 
#' Returns `NULL` if `MPAdvice` is a valid advice object.
#'
#' @keywords internal
Log_MPError <- function(MPAdvice, MPName, Data, Sim, Year) {
  if (inherits(MPAdvice, 'advice')) {
    return(invisible(NULL))
  }
  
  if (!inherits(MPAdvice, 'try-error')) {
    stop(paste0("\nMP `", MPName, " `did not return an `Advice()` object\nData: ",  
                Data@Name, 
                "\nSimulation: ", Sim, 
                '\nYear: ', Year))
  }
  stop(paste0("\nMP `", MPName, " Error\nData: ",  
              Data@Name, 
              "\nSimulation: ", Sim, 
              '\nYear: ', Year,
              '\nError: ', MPAdvice)
  )
}