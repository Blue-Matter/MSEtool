#' Check that management procedures are of class `mp`
#'
#' Verifies that all supplied management procedure (MP) names correspond
#' to functions of class `mp`. Throws an error if any MP does not meet this requirement.
#'
#' @param MPs Character vector of names of management procedure functions to check.
#'
#' The function retrieves each function by name using `get()`, checks its class,
#' and ensures that all are of class `mp`. If any function is not of class `mp`,
#' the function will abort with a clear error message.
#'
#' @return
#' Invisibly returns `NULL`. The function is used for validation and does not modify inputs.
#'
#' @keywords internal
CheckMPClass <- function(MPs) {
  CheckClass(MPs, 'character', 'MPs')
  
  MPFunctions <- purrr::map(MPs, get)
  names(MPFunctions) <- MPs
  MPClass <- purrr::map(MPFunctions, class) |> unlist()
  if (any(MPClass != 'mp')) 
    cli::cli_abort("Currently only MPs of class `mp` are supported", call=NULL)
  
  NULL
}