#' `dynamics` Object
#'
#' Stores Fleet Dynamics information
#' 
#' Currently not used
#'
#' @slot Misc Miscellaneous list.
#'
#' @export
setClass(
  "dynamics",
  slots = c(
    Misc  = "list"
  )
)