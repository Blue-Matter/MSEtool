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
#' `SetupParallel()` spawns background worker processes that persist until
#' they are explicitly shut down. Always call [DisableParallel()] once your
#' workflow is finished. If a session ends abnormally (crash, forced kill)
#' before that happens, its workers are orphaned and keep running; use
#' [ListParallelWorkers()] to check for these and [CleanupOrphanWorkers()] to
#' terminate them.
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
#' `SetupParallel()` modifies the global future plan for the current R
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
#'   - `CheckParallel()` returns `FALSE` if `parallel = FALSE`, `TRUE` if
#'     `parallel = TRUE` and a parallel plan is active, and throws an error
#'     if `parallel = TRUE` but no parallel plan is active
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
#' @seealso [future::plan()], [future::availableCores()], [furrr::future_map()],
#'   [ListParallelWorkers()], [CleanupOrphanWorkers()]
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
  
  plan <- .ResolveFuturePlan(
    backend = backend,
    workers = workers,
    max_workers = max_workers
  )
  
  do.call(future::plan, plan$args)
  
  if (!silent) {
    cli::cli_inform(c(
      "Parallel processing enabled using {.val {plan$name}} with {.val {plan$workers}} worker{?s} ({plan$core_type} cores).",
      "i" = "Call {.code DisableParallel()} when you are done to shut these workers down.",
      "i" = "If a session ever ends without calling it, use {.code ListParallelWorkers()} and {.code CleanupOrphanWorkers()} to find and remove any orphaned workers."
    ))
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
    cli::cli_abort(c(
      "{.val parallel = TRUE} requested but no parallel {.pkg future} plan is active.",
      "i" = "Initialise a parallel plan first, e.g. {.code SetupParallel(workers = 4)},",
      "i" = "or pass {.code parallel = FALSE} to run sequentially."
    ))
  }

  parallel
}


.ResolveFuturePlan <- function(backend, workers, max_workers) {
  
  if (backend == "sequential") {
    return(list(
      name = "sequential",
      workers = 1L,
      args = list(future::sequential)
    ))
  }
  
  if (backend == "auto") {
    backend <- if (.IsWindows()) "multisession" else "multisession"
  }
  
  if (backend == "multicore" && .IsWindows()) {
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

.IsWindows <- function() {
  .Platform$OS.type == "windows"
}

#' List and Clean Up Parallel Worker Processes
#' 
#' Clean up orphan worker processes
#'
#' `future::multisession` (the backend [SetupParallel()] uses) spawns
#' separate, persistent R processes for its workers. If the R session that
#' created them exits abnormally rather than calling [DisableParallel()]
#' those worker processes are not automatically cleaned up; each keeps
#' running (holding whatever memory it had at the time) until manually killed
#' or the machine restarts. 
#' 
#' - `ListParallelWorkers()` finds every such worker
#' process on this machine and reports whether its parent R session is still
#' alive. 
#' 
#' - `CleanupOrphanWorkers()` terminates only the ones whose parent is
#' confirmed gone.
#'
#'
#' @param dry_run Logical. If `TRUE` (default), `CleanupOrphanWorkers()`
#'   reports what would be terminated without actually terminating anything.
#'
#' @return `ListParallelWorkers()` returns a `data.frame` with columns `pid`,
#'   `parent_pid` (`NA` if it could not be determined), `parent_alive` (`NA`
#'   if undetermined), and `orphan` (`TRUE` only when `parent_pid` is known
#'   and confirmed dead). `CleanupOrphanWorkers()` invisibly returns the same
#'   data.frame restricted to the orphaned rows that were (or, under
#'   `dry_run = TRUE`, would be) terminated.
#'
#' @seealso [SetupParallel()], [DisableParallel()], [CheckParallel()]
#' @export
ListParallelWorkers <- function() {
  procs <- if (.IsWindows()) .ListProcessesWindows() else .ListProcessesUnix()

  empty <- data.frame(pid = integer(0), parent_pid = integer(0),
                      parent_alive = logical(0), orphan = logical(0))
  if (!nrow(procs)) return(empty)

  is_worker <- grepl("workRSOCK|\\.slaveRSOCK", procs$cmd, perl = TRUE)
  workers   <- procs[is_worker, , drop = FALSE]
  if (!nrow(workers)) return(empty)

  has_parent <- grepl("parallelly\\.parent=[0-9]+", workers$cmd, perl = TRUE)
  parent_pid <- rep(NA_integer_, nrow(workers))
  parent_pid[has_parent] <- as.integer(
    sub(".*parallelly\\.parent=([0-9]+).*", "\\1", workers$cmd[has_parent], perl = TRUE)
  )

  parent_alive <- rep(NA, length(parent_pid))
  parent_alive[has_parent] <- vapply(parent_pid[has_parent], .ProcessAlive, logical(1))

  data.frame(
    pid          = workers$pid,
    parent_pid   = parent_pid,
    parent_alive = parent_alive,
    orphan       = has_parent & !parent_alive,
    row.names    = NULL
  )
}

#' @rdname ListParallelWorkers
#' @export
CleanupOrphanWorkers <- function(dry_run = TRUE) {
  workers <- ListParallelWorkers()
  orphans <- workers[!is.na(workers$orphan) & workers$orphan, , drop = FALSE]

  if (!nrow(orphans)) {
    cli::cli_inform("No orphaned parallel worker processes found.")
    return(invisible(orphans))
  }

  if (dry_run) {
    cli::cli_inform(c(
      "!" = "Found {nrow(orphans)} orphaned worker process{?es} (PID{?s}: {.val {orphans$pid}}).",
      "i" = "Run {.code CleanupOrphanWorkers(dry_run = FALSE)} to terminate {cli::qty(nrow(orphans))}{?it/them}."
    ))
    return(invisible(orphans))
  }

  for (pid in orphans$pid) {
    ok <- .KillProcess(pid)
    if (ok) {
      cli::cli_alert_success("Terminated orphaned worker PID {.val {pid}}.")
    } else {
      cli::cli_alert_warning("Failed to terminate PID {.val {pid}} (it may already have exited).")
    }
  }

  invisible(orphans)
}

.RunPowerShell <- function(script_text) {
  script <- tempfile(fileext = ".ps1")
  on.exit(unlink(script))
  writeLines(script_text, script)
  tryCatch(
    system2("powershell", c("-NoProfile", "-ExecutionPolicy", "Bypass", "-File", script),
            stdout = TRUE, stderr = FALSE),
    error = function(e) character(0),
    warning = function(w) character(0)
  )
}


.ListProcessesWindows <- function() {
  lines <- .RunPowerShell(
    'Get-CimInstance Win32_Process -Filter "name=\'Rscript.exe\'" | ForEach-Object { "$($_.ProcessId)|$($_.CommandLine)" }'
  )
  if (!length(lines)) return(data.frame(pid = integer(0), cmd = character(0)))

  parts <- strsplit(lines, "|", fixed = TRUE)
  pid   <- suppressWarnings(as.integer(vapply(parts, `[`, character(1), 1)))
  cmd   <- vapply(parts, \(p) paste(p[-1], collapse = "|"), character(1))
  valid <- !is.na(pid)
  data.frame(pid = pid[valid], cmd = cmd[valid], row.names = NULL)
}


.ListProcessesUnix <- function() {
  lines <- tryCatch(
    system2("ps", c("-eo", "pid=,command="), stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)
  )
  if (!length(lines)) return(data.frame(pid = integer(0), cmd = character(0)))

  lines <- trimws(lines)
  pid   <- suppressWarnings(as.integer(sub("^([0-9]+)\\s.*", "\\1", lines)))
  cmd   <- sub("^[0-9]+\\s+", "", lines)
  valid <- !is.na(pid)
  data.frame(pid = pid[valid], cmd = cmd[valid], row.names = NULL)
}

# TRUE/FALSE for whether a process with this pid currently exists.
.ProcessAlive <- function(pid) {
  if (is.na(pid)) return(NA)
  if (.IsWindows()) {
    out <- tryCatch(
      system2("tasklist", c("/FI", shQuote(paste0("PID eq ", pid)), "/NH"),
              stdout = TRUE, stderr = FALSE),
      error = function(e) character(0)
    )
    return(any(grepl(paste0("\\b", pid, "\\b"), out)))
  }
  out <- tryCatch(
    system2("ps", c("-p", pid), stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)
  )
  length(out) > 1
}

# Terminates a process by pid; returns TRUE on success.
.KillProcess <- function(pid) {
  if (.IsWindows()) {
    res <- tryCatch(
      system2("taskkill", c("/PID", pid, "/F"), stdout = FALSE, stderr = FALSE),
      error = function(e) 1L
    )
  } else {
    res <- tryCatch(
      system2("kill", c("-9", pid), stdout = FALSE, stderr = FALSE),
      error = function(e) 1L
    )
  }
  identical(res, 0L)
}
