#' Check Obs Objects in an Operating Model
#'
#' Checks that all [Obs()] objects are present and non-empty for each
#' stock-fleet combination in the `OM` object. Issues warnings for any missing
#' or empty [Obs()] objects. No data will be generated for stock-fleet
#' combinations without a populated [Obs()] object.
#'
#' @param OM An operating model object containing an `@Obs` slot.
#' @param silent Logical; if `TRUE`, suppresses all output and returns
#'   invisibly. Default is `FALSE`.
#'
#' @return Logical; `TRUE` if all [Obs()] objects are present and non-empty,
#'   `FALSE` if any are missing or empty.
#' @keywords internal
CheckObs <- function(OM, silent=FALSE, Proj=FALSE) {

  if (is.null(OM@Obs)) {
    if (!silent) {
      cli::cli_text('')
      cli::cli_alert_warning(
        "No {.help MSEtool::Obs} object has been provided in the `OM` object.
       No Data will be generated.",
        wrap=TRUE
      )
    }
    return(FALSE)
  }
  
  Empty <- purrr::map(OM@Obs, \(stock)
                      purrr::map(stock, EmptyObject)
  )
  
  if (!any(unlist(Empty)))
    return(TRUE)
  
  for (i in seq_along(Empty)) {
    for (j in seq_along(Empty[[i]])) {
      if (Empty[[i]][[j]]) {
        if (!silent) {
          cli::cli_text('')
          cli::cli_alert_warning("No {.help MSEtool::Obs} object found for the following:")
          cli::cli_li("Stock: {.val {names(Empty)[i]}} and Fleet: {.val {names(Empty[[i]])[j]}}. ")
          
        }
          
      }
    }
  }
  
  if (!silent) {
    cli::cli_text('')
    cli::cli_alert_warning("No Data will be generated for these stocks/fleets")
    cli::cli_text('')
  }
    
  
  return(FALSE)
  
}