#' Load Function Arguments into an Environment
#'
#' Assigns the default argument values of a function to `envir`, skipping any
#' argument that already exists in that environment as a non-function object.
#' Useful during interactive development for populating the workspace with a
#' function's expected inputs without overwriting variables already set.
#'
#' @param fun Character string naming a function, or a function object.
#'   Default `"Simulate"`.
#' @param envir Environment to assign default values into. Default
#'   `.GlobalEnv`.
#' @param debug Logical. If `TRUE`, emits a message for each argument
#'   indicating whether it already exists in `envir` and whether it is being
#'   assigned. Default `FALSE`.
#'
#' @return `NULL` invisibly. Called for its side effect of populating `envir`.
#'
#' @examples
#' # Populate the global workspace with the default arguments of [CalcSurvival()],
#' # without overwriting any variables that are already defined:
#' \dontrun{
#' NaturalMortality <- my_array   # already set; will not be overwritten
#' LoadArgs("CalcSurvival")
#' # FishingMortality, PlusGroup, SpawnTimeFrac, and Semelparous are now
#' # assigned their defaults in .GlobalEnv; NaturalMortality is unchanged.
#' }
#'
#' @export
LoadArgs <- function(fun="Simulate", envir=.GlobalEnv, debug=FALSE) {
  if (is.function(fun))
    fun <- deparse(substitute(fun))
  
  if (!is.character(fun))
    cli::cli_abort('{.arg fun} must be a character string or a function object.')
  
  formals_list <- formals(get(fun))
  arg_names    <- names(formals_list)
  
  for (i in seq_along(arg_names)) {
    nm <- arg_names[i]
    
    already_exists <- exists(nm, envir=envir, inherits=FALSE) &&
      !is.function(get(nm, envir=envir, inherits=FALSE))
    
    if (debug) {
      cli::cli_text("{.val {nm}}")
      if (already_exists)
        cli::cli_alert_info("Already exists in environment \u2014 skipping.")
    }
    
    if (already_exists)
      next
    
    value <- formals_list[[i]]
    value <- if (missing(value)) NULL else if (is.call(value)) eval(value) else value
    
    if (debug)
      cli::cli_alert_info("Assigning default for {.val {nm}}.")
    
    assign(nm, value, envir=envir)
  }
  invisible(NULL)
}

