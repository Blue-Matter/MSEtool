with_future_plan <- function(parallel, workers = NULL, expr) {
  
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  
  if (!isTRUE(parallel)) {
    future::plan(future::sequential)
  } else {
    if (is.null(workers)) {
      workers <- future::availableCores()
    }
    
    future::plan(
      future::multisession,
      workers = workers
    )
  }
  
  eval(substitute(expr), envir = parent.frame())
}
