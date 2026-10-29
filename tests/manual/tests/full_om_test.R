#' Test `Simulate()` Across All Available Operating Models
#'
#' Runs [MSEtool::Simulate()] for every OM object returned by `avail('OM')`
#' (or a supplied vector of names), catching errors so a single broken OM
#' doesn't halt the run, and comparing the resulting `Hist` object against a
#' saved snapshot from the last time the test was run. This makes it easy to
#' see either (a) that an OM's `Hist` output has changed since the last run,
#' or (b) exactly what error `Simulate()` is now throwing for a given OM.
#'
#' @param OM_names Character vector of OM object names to test. Defaults to
#'   `avail('OM')`.
#' @param snapshot_dir Directory in which `Hist` snapshots are stored as one
#'   `.rds` file per OM. Defaults to `tests/testthat/_snaps/Hist` (via
#'   `testthat::test_path()`), falling back to `_snaps/Hist` in the working
#'   directory if not run inside a package/testthat context.
#' @param update Logical. If `TRUE`, overwrite existing snapshots with the
#'   newly generated `Hist` objects, i.e. accept the current output as the
#'   new baseline. Default `FALSE`.
#' @param ... Additional arguments passed to `Simulate()`, e.g. `silent = TRUE`.
#'
#' @return An invisible `data.frame`, one row per OM, with columns `OM`,
#'   `status` (`"new"`, `"unchanged"`, `"changed"`, `"error"`, or
#'   `"updated"`), `message` (error message, or any warnings caught during
#'   `Simulate()`), and `n_diffs` (number of difference chunks reported by
#'   `waldo::compare()` when `status == "changed"`). The full `Hist` objects
#'   and `waldo` diffs are attached via `attr(., "results")` for
#'   programmatic inspection.
#'
#' @examples
#' \dontrun{
#' library(MSEtool)
#'
#' # first run: no snapshots exist yet, everything is recorded as "new"
#' res <- test_OM_Simulate()
#'
#' # after changing OM/Simulate code, re-run to check for regressions
#' res <- test_OM_Simulate()
#' res[res$status != "unchanged", ]
#'
#' # inspect exactly what changed for one OM
#' attr(res, "results")[["Albacore"]]$diff
#'
#' # once you've reviewed and accept the new output as correct:
#' test_OM_Simulate(update = TRUE)
#' }
#' @export
test_OM_Simulate <- function(OM_names = avail('om'),
                             snapshot_dir = NULL,
                             update = FALSE,
                             ...) {
  
  if (is.null(snapshot_dir)) {
    snapshot_dir <- tryCatch(
      testthat::test_path('_snaps', 'Hist'),
      error = function(e) file.path('_snaps', 'Hist')
    )
  }
  if (!dir.exists(snapshot_dir)) dir.create(snapshot_dir, recursive = TRUE)
  
  if (!requireNamespace('waldo', quietly = TRUE)) {
    stop("Package 'waldo' is required to detect changes in Hist objects. ",
         "Install it with install.packages('waldo').")
  }
  
  cli::cli_h1('Testing Simulate() for {length(OM_names)} OM object{?s}')
  
  results <- vector('list', length(OM_names))
  names(results) <- OM_names
  
  cli::cli_progress_bar('Simulating', total = length(OM_names))
  
  for (nm in OM_names) {
    
    OM <- tryCatch(get(nm), error = function(e) NULL)
    
    if (is.null(OM)) {
      results[[nm]] <- list(status = 'error',
                            message = paste('Could not find OM object:', nm),
                            Hist = NULL, diff = NULL)
      cli::cli_progress_update()
      next
    }
    
    # capture warnings without letting them halt the loop, and without
    # treating them as failures
    warnings_caught <- character()
    
    Hist <- withCallingHandlers(
      tryCatch(Simulate(OM,...), error = function(e) e),
      warning = function(w) {
        warnings_caught <<- c(warnings_caught, conditionMessage(w))
        invokeRestart('muffleWarning')
      }
    )
    
    if (inherits(Hist, 'error')) {
      results[[nm]] <- list(status = 'error',
                            message = conditionMessage(Hist),
                            Hist = NULL, diff = NULL)
      cli::cli_progress_update()
      next
    }
    
    snap_file <- file.path(snapshot_dir, paste0(nm, '.rds'))
    warn_msg  <- if (length(warnings_caught)) paste(warnings_caught, collapse = '; ') else NA_character_
    
    if (!file.exists(snap_file)) {
      saveRDS(Hist, snap_file)
      results[[nm]] <- list(status = 'new', message = warn_msg, Hist = Hist, diff = NULL)
      
    } else if (update) {
      saveRDS(Hist, snap_file)
      results[[nm]] <- list(status = 'updated', message = warn_msg, Hist = Hist, diff = NULL)
      
    } else {
      old_Hist <- readRDS(snap_file)
      diff <- waldo::compare(old_Hist, Hist, x_arg = 'previous', y_arg = 'current')
      
      if (length(diff) == 0) {
        results[[nm]] <- list(status = 'unchanged', message = warn_msg, Hist = Hist, diff = NULL)
      } else {
        results[[nm]] <- list(status = 'changed', message = warn_msg, Hist = Hist, diff = diff)
      }
    }
    
    cli::cli_progress_update()
  }
  
  cli::cli_progress_done()
  
  summary_df <- data.frame(
    OM      = names(results),
    status  = vapply(results, `[[`, character(1), 'status'),
    message = vapply(results, function(x) if (is.na(x$message)) '' else x$message, character(1)),
    n_diffs = vapply(results, function(x) if (is.null(x$diff)) NA_integer_ else length(x$diff), integer(1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  
  n_err <- sum(summary_df$status == 'error')
  n_chg <- sum(summary_df$status == 'changed')
  n_ok  <- sum(summary_df$status %in% c('unchanged', 'new', 'updated'))
  
  if (n_err > 0) cli::cli_alert_danger('{n_err} OM{?s} errored')
  if (n_chg > 0) cli::cli_alert_warning('{n_chg} OM{?s} changed since last snapshot')
  if (n_ok  > 0) cli::cli_alert_success('{n_ok} OM{?s} unchanged / newly recorded')
  
  attr(summary_df, 'results') <- results
  invisible(summary_df)
}