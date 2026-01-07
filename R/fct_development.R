# Handy functions to use during development
# use devtools::load_all() to load


#' @export
LoadArgs <- function(fun='Simulate', envir = .GlobalEnv, debug=FALSE) {
  CheckClass(fun, c('character', 'function'), fun)
  if (inherits(fun, 'function')) {
    fun <-  deparse(substitute(fun))
  }
 
  formals <- get(fun) |> formals()
  args <- names(formals)
  for (i in seq_along(args)) {
    if (debug) {
      cli::cli_text("{.val {args[i]}}")
      if (exists(eval(args[i]), envir=envir)) 
        cli::cli_alert_info("Exists in Global")
      if (inherits(get(args[i], envir=envir), 'function'))
        cli::cli_alert_info("Is a function")
    }
      
    if (exists(eval(args[i]), envir=envir) && !inherits(get(args[i], envir=envir), 'function')) { 
      next()
    }
    if (debug)
      cli::cli_alert_info("Assigning default argument")
    value <- formals[[i]]
    if (missing(value))
      value <- NULL
    
    if (inherits(value, 'call'))
      value <- eval(value)
    assign(args[i], value, envir = envir)
  }
}


#' @export
la <- function() {
  CheckPackage('devtools')
  devtools::load_all()
}