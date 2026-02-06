#' Set up parallel processing for openMSE workflows
#'
#' Configure the global parallel processing strategy used throughout
#' the openMSE framework via the `future` framework.
#' 
#' These functions are intended to be called at the 
#' beginning (and optionally the end) of a workflow to control 
#' how parallel evaluation is performed.
#'
#' * `SetupParallel()` establishes a parallel execution plan, while
#' * `DisableParallel()` restores sequential execution
#'
#' By default, `SetupParallel()` automatically selects a safe and
#' platform-appropriate backend:
#'
#' * **Windows**: `future::multisession`
#' * **macOS / Linux**: `future::multisession` (default), with optional
#'   support for `future::multicore`
#'
#' Although `future::multicore` is often faster on Unix-like systems,
#' `multisession` is used by default because it is more robust.
#' 
#' ## Global side effects
#'
#' This function modifies the **global future plan** for the current R
#' session. All packages relying on `future` (including `furrr`) will
#' use this plan until it is changed again or reset with
#' `DisableParallel()`.
#'
#' ## Choosing a backend
#'
#' * `multisession` is the safest and most portable option and works on
#'   all platforms.
#' * `multicore` (Unix-like systems only) uses forked processes and can
#'   be faster for large workloads, but may be unsafe with certain
#'   compiled code or external pointers.
#' * `sequential` disables parallel processing entirely.
#' 
#' 
#' ## Recommended usage
#'
#' * Call `SetupParallel()` once at the start of an analysis
#' * Use `DisableParallel()` to explicitly restore sequential execution
#'
#' @param workers Integer; number of parallel workers to use.
#'   Defaults to `future::availableCores()`.
#'
#' @param backend Character; parallel backend to use. One of:
#'   * `"auto"` (default): choose a safe backend based on the operating system
#'   * `"multisession"`: background R sessions (all platforms)
#'   * `"multicore"`: forked processes (macOS / Linux only)
#'   * `"sequential"`: disable parallel processing
#'
#' @param max_workers Character; which type of CPU cores to use when
#'   determining the maximum number of workers.
#'   * `"physical"` (default): limit workers to physical CPU cores
#'   * `"logical"`: allow use of logical cores (hyperthreads)
#' 
#' Using `"physical"` is generally recommended.
#'   
#' @param silent Logical; if `FALSE`, prints a short message describing
#'   the selected parallel plan.
#'
#' @return Invisibly returns `TRUE`.
#'
#' @examples
#' \dontrun{
#' # Typical usage at the start of a workflow
#' SetupParallel()
#'
#' # Explicitly request 4 workers
#' SetupParallel(workers = 4)
#'
#' # Use multicore on Linux/macOS
#' SetupParallel(backend = "multicore")
#'
#' # Disable parallel processing
#' DisableParallel()
#' }
#'
#' @seealso
#' * [future::plan()]
#' * [future::availableCores()]
#' * [furrr::future_map()]
#'
#' @export
SetupParallel <- function(workers = future::availableCores(),
                          backend = c("auto", "multisession", "multicore", "sequential"),
                          max_workers = c("physical", "logical"),
                          silent = FALSE) {
  
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
DisableParallel <- function() {
  future::plan(future::sequential)
  invisible(TRUE)
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

