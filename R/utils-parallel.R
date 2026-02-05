#' Manage parallel processing plans with `future`
#'
#' These functions help configure the `future` framework for parallel
#' processing. `use_multisession()` sets up a multisession backend,
#' creating background R sessions for concurrent evaluation.  
#' `use_sequential()` restores sequential execution.
#'
#' **Important:** These functions modify the **global future plan** for the
#' R session. Users should be aware that this affects all future-based
#' operations (e.g., `furrr::future_map()`) until the plan is changed again.
#'
#' ## OS-specific notes
#'
#' - On **Windows**, `future::multisession` uses PSOCK clusters. Worker
#'   startup can be slower and may take several seconds per worker.
#' - On **Unix-like systems** (Linux, macOS), PSOCK or multicore backends
#'   may be used; worker creation is generally faster.
#'
#' @param workers Integer; the number of worker processes to launch. 
#'   Defaults to `future::availableCores()`.
#'
#' @return Invisibly returns `TRUE`.
#'
#' @examples
#' \dontrun{
#' # Enable parallel processing with 4 workers
#' use_multisession(workers = 4)
#' 
#' # Example parallel map
#' res <- furrr::future_map(x, f)
#'
#' # Restore sequential processing
#' use_sequential()
#' }
#'
#' @seealso [future::plan()], [furrr::future_map()]
#' @name future_plan_helpers
NULL

#' @rdname future_plan_helpers
#' @export
use_multisession <- function(workers = future::availableCores()) {
  future::plan(future::multisession, workers = workers)
  invisible(TRUE)
}

#' @rdname future_plan_helpers
#' @export
use_sequential <- function() {
  future::plan(future::sequential)
  invisible(TRUE)
}