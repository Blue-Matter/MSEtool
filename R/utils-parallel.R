#' Set Up Parallel Processing for openMSE Workflows
#'
#' Configure the global parallel processing strategy used throughout the
#' openMSE framework via the `future` backend.
#' 
#' These functions are intended to be called at the beginning (and optionally
#' the end) of a workflow:
#'
#' - `SetupParallel()` establishes a parallel execution plan
#' - `DisableParallel()` restores sequential execution
#' - `CheckParallel()` validates that a parallel plan is active
#'
#' By default, `SetupParallel()` selects a safe, platform-appropriate backend:
#'
#' - **Windows**: `multisession`
#' - **macOS / Linux**: `multisession` (default), with optional `multicore` support
#' 
#' Although `future::multicore` is often faster on Unix-like systems,
#' `multisession` is used by default because it is more robust.
#' 
#' ## Global side effects
#'
#' `SetupParallel()` modifies the **global future plan** for the current R
#' session. All packages relying on `future` (including `furrr`) will inherit
#' this plan until it is changed or reset with `DisableParallel()`.
#' 
#' ## Backend options
#'
#' - `"multisession"` — background R sessions; safe and portable on all platforms
#' - `"multicore"` — forked processes (macOS / Linux only); faster for large
#'   workloads but may be unsafe with certain compiled code or external pointers
#' - `"sequential"` — disables parallel processing entirely
#' 
#' 
#' ## Recommended usage
#'
#' ```r
#' SetupParallel()          # start of workflow
#' # ... analysis code ...
#' DisableParallel()        # restore sequential execution
#' ```
#'
#' @param workers    Integer. Number of parallel workers. Defaults to
#'                   `future::availableCores()`.
#' @param backend    Character. Parallel backend to use. One of `"auto"`
#'                   (default), `"multisession"`, `"multicore"`, or
#'                   `"sequential"`. `"auto"` selects a safe backend based
#'                   on the operating system.
#' @param max_workers Character. Which CPU cores to count when determining the
#'                   worker ceiling. `"physical"` (default) limits to physical
#'                   cores; `"logical"` allows hyperthreads. Using `"physical"`
#'                   is generally recommended.
#' @param silent     Logical. If `FALSE` (default), prints a message describing
#'                   the active parallel plan.
#' @param parallel   Logical. Has `parallel` been requested?
#'
#' @return All three functions invisibly return a logical scalar:
#'   - `SetupParallel()` and `DisableParallel()` return `TRUE`
#'   - `CheckParallel()` returns the value of `parallel` if a valid plan is
#'     active, or `FALSE` if no parallel plan is detected
#'     
#'     
#' @examples
#' \dontrun{
#' SetupParallel()                        # auto-select backend
#' SetupParallel(workers = 4)             # explicit worker count
#' SetupParallel(backend = "multicore")   # multicore on Linux/macOS
#' DisableParallel()                      # restore sequential execution
#' }
#' 
#' @seealso [future::plan()], [future::availableCores()], [furrr::future_map()]
#'
#' @export
#' 
SetupParallel <- function(workers = future::availableCores(),
                          backend = c("auto", "multisession", "multicore", "sequential"),
                          max_workers = c("physical", "logical"),
                          silent = FALSE) {
  
  CheckPackage('future')
  backend <- match.arg(backend)
  max_workers <- match.arg(max_workers)
  
  plan <- ResolveFuturePlan(
    backend = backend,
    workers = workers,
    max_workers = max_workers
  )
  
  do.call(future::plan, plan$args)
  
  if (!silent) {
    cli::cli_inform(
      "Parallel processing enabled using {.val {plan$name}} with {.val {plan$workers}} worker{?s} ({.val {plan$core_type}} cores)."
    )
  }
  
  invisible(TRUE)
}

#' @rdname SetupParallel
#' @export
DisableParallel <- function(silent=FALSE) {
  future::plan(future::sequential)
  cli::cli_inform("Parallel processing disabled. Running sequentially.")
  invisible(TRUE)
}

#' @rdname SetupParallel
#' @export
CheckParallel <- function(parallel) {
  if (!parallel)
    return(FALSE)
  
  if (inherits(future::plan(), "sequential")) {
    cli::cli_alert_warning(
      "{.val parallel = TRUE} requested but no parallel `future` plan is active."
    )
    cli::cli_inform(c(
      "i" = "Initialise a parallel plan first, e.g.:",
      " " = "{.code SetupParallel(workers = 4)}",
      "i" = "Running sequentially instead."
    ))
    return(FALSE)
  }
  
  parallel
}


ResolveFuturePlan <- function(backend, workers, max_workers) {
  
  if (backend == "sequential") {
    return(list(
      name = "sequential",
      workers = 1L,
      args = list(future::sequential)
    ))
  }
  
  if (backend == "auto") {
    backend <- if (IsWindows()) "multisession" else "multisession"
  }
  
  if (backend == "multicore" && IsWindows()) {
    cli::cli_abort(
      "`future::multicore` is not supported on Windows. Use `multisession` instead."
    )
  }
  
  logical <- max_workers == "logical"
  available <- future::availableCores(logical = logical)
  workers <- min(as.integer(workers), as.integer(available))
  
  list(
    name = backend,
    workers = workers,
    core_type = max_workers,
    args = list(
      switch(
        backend,
        multisession = future::multisession,
        multicore    = future::multicore
      ),
      workers = workers
    )
  )
}

IsWindows <- function() {
  .Platform$OS.type == "windows"
}


