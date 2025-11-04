# Handy functions to use during development
# use devtools::load_all() to load


LoadArgs <- function(fun='Simulate', envir = .GlobalEnv) {
  formals <- get(fun) |> formals()
  args <- names(formals)
  for (i in seq_along(args))
    assign(args[i], formals[[i]], envir = envir)
}