# Handy functions to use during development
# use devtools::load_all() to load


LoadArgs <- function(fun='Simulate', envir = .GlobalEnv) {
  formals <- get(fun) |> formals()
  args <- names(formals)
  for (i in seq_along(args)) {
    if (exists(eval(args[i]), envir=envir) && !inherits(get(args[i]), 'function'))
      next()
    value <- formals[[i]]
    if (missing(value))
      value <- NULL
    
    if (inherits(value, 'call'))
      value <- eval(value)
    assign(args[i], value, envir = envir)
  }
}