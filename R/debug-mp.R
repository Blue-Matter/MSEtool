#' Debug a Failed Management Procedure
#'
#' Returns the inputs of a management procedure (MP) call that failed during
#' [Project()]: the `Data` object the MP was given, the simulation, year and
#' stock/complex, the MP function and its argument values, the error message
#' and the call stack at the error. With `run = TRUE`, the MP is called again
#' on the same `Data`, with the same random seed, and steps into the MP
#' function in the debugger.
#'
#' An MP call fails when the MP raises an error or returns an invalid
#' `Advice` object. Failures are recorded in the `Log` slot of the returned
#' [mse-class] object (see [Log()]). The inputs of the first failure of each
#' MP are stored with the log entry. The `MSEtool.DebugMP` option controls
#' how many are stored:
#'
#' - `options(MSEtool.DebugMP = "first")` (the default) stores the first
#'   failed call of each MP.
#' - `options(MSEtool.DebugMP = "all")` stores every failed call.
#'   This can make the `MSE` object much larger.
#' - `options(MSEtool.DebugMP = FALSE)` stores none.
#'
#' @param MSE An [mse-class] object returned by [Project()].
#' @param MP Character. Name of the MP. Default `NULL` uses the first MP with
#'   a stored failure.
#' @param Sim Integer. Simulation number. Default `NULL` uses the first
#'   stored failure of `MP`.
#' @param Year Numeric. Projection year (time step). Default `NULL` uses the
#'   first stored failure of `MP`.
#' @param run Logical. Call the MP again on the stored inputs? In an
#'   interactive session, the MP function is opened in the debugger with
#'   [debugonce()]. Default `FALSE`.
#'
#' @return If `run = FALSE`, an object of class `mpdebug`: a list with
#'   elements
#'   \describe{
#'     \item{`MP`}{Name of the MP.}
#'     \item{`Sim`, `Year`, `Stock`}{Simulation, year and stock/complex of the failed call.}
#'     \item{`Message`}{The logged error message.}
#'     \item{`Function`}{The MP function.}
#'     \item{`Args`}{Named list of the argument values of the MP, other than `Data`.}
#'     \item{`Data`}{The [data-class] object the MP was given (a named list
#'       of them, `DataList`, for an `mmp`-class MP).}
#'     \item{`Returned`}{The value the MP returned when it ran without error
#'       but its `Advice` was invalid; otherwise `NULL`.}
#'     \item{`Calls`}{Character vector of the call stack at the error.}
#'     \item{`Seed`}{The seed of the MP call.}
#'   }
#'   If `run = TRUE`, the value returned by the MP.
#'
#' @examples
#' \dontrun{
#' MyMP <- function(Data, Target = 0.5) {
#'   stop("something went wrong")
#' }
#' class(MyMP) <- 'mp'
#'
#' Hist <- Simulate(SingleStockOM, nSim = 2)
#' MSE <- Project(Hist, MPs = 'MyMP')
#'
#' d <- DebugMP(MSE)
#' d
#' d$Data
#' DebugMP(MSE, run = TRUE)
#' }
#'
#' @seealso [Log()], [Project()], [SetMPArgs()]
#' @export
DebugMP <- function(MSE, MP = NULL, Sim = NULL, Year = NULL, run = FALSE) {
  if (!'Log' %in% methods::slotNames(MSE))
    cli::cli_abort("{.arg MSE} must be an object returned by {.fn Project}.")

  Errors <- Filter(.IsLogEntry, MSE@Log$error)
  if (!length(Errors))
    cli::cli_abort("No MP errors are recorded in {.arg MSE}.")

  Keep <- \(e) (is.null(MP) || identical(as.character(e$mp)[1], MP)) &&
    (is.null(Sim) || isTRUE(as.integer(e$sim[1]) == Sim)) &&
    (is.null(Year) || isTRUE(abs(as.numeric(e$year[1]) - Year) < 1e-6))
  Matched <- Filter(Keep, Errors)
  if (!length(Matched))
    cli::cli_abort("No MP errors are recorded in {.arg MSE} for the selected {.arg MP}, {.arg Sim} and {.arg Year}.")

  Stored <- Filter(\(e) !is.null(e$debug), Matched)
  if (!length(Stored))
    cli::cli_abort(c(
      "The inputs of the selected MP call are not stored in {.arg MSE}.",
      "i" = "Set {.code options(MSEtool.DebugMP = \"all\")} and re-run {.fn Project} to store the inputs of every failed MP call."
    ))

  e   <- Stored[[1]]
  rec <- e$debug
  out <- structure(list(
    MP       = as.character(e$mp)[1] %||% rec$MPName,
    Sim      = e$sim,
    Year     = e$year,
    Stock    = rec$Stock,
    Message  = e$message,
    Function = rec$MP,
    Args     = .MPArgs(rec$MP),
    Data     = rec$Data,
    Returned = rec$Returned,
    Calls    = rec$Calls,
    Seed     = rec$Seed
  ), class = 'mpdebug')

  if (isTRUE(run))
    return(.RerunMP(out, Browse = interactive()))
  out
}

#' @export
print.mpdebug <- function(x, ...) {
  .MsgTheme()
  cli::cli_rule(left = "MP debug: {.mp {x$MP}}")
  Tags <- c(paste0("Sim: ", x$Sim), paste0("Year: ", x$Year),
            if (length(x$Stock) && nzchar(x$Stock)) paste0("Stock: ", x$Stock))
  cli::cli_text(paste(Tags, collapse = paste0(' ', intToUtf8(183), ' ')))
  cli::cli_alert_danger(.MsgEscape(gsub("\\s*\n\\s*", " ", trimws(x$Message))))

  if (length(x$Args)) {
    cli::cli_text('')
    cli::cli_text("Arguments:")
    Args <- vapply(x$Args, \(a) paste(deparse(a, width.cutoff = 60L, nlines = 1L), collapse = ''),
                   character(1))
    cli::cli_verbatim(paste0("  ", names(Args), " = ", Args))
  }
  if (!is.null(x$Returned)) {
    cli::cli_text('')
    cli::cli_text("MP returned an object of class {.cls {class(x$Returned)}}.")
  }
  if (length(x$Calls)) {
    cli::cli_text('')
    cli::cli_text("Call stack:")
    Calls <- sub("^MPfunction\\(", paste0(x$MP, "("), x$Calls)
    cli::cli_verbatim(paste0("  ", Calls))
  }
  cli::cli_text('')
  DataArg <- if (inherits(x$Function, 'mmp')) "DataList" else "Data"
  cli::cli_text(cli::col_grey(paste0(
    "Inspect {.code x$Data}; call {.code x$Function(", DataArg, " = x$Data)}, ",
    "or {.code DebugMP(..., run = TRUE)} to step through the MP.")))
  invisible(x)
}

# try() that also records the call stack where the error was raised
.TryMP <- function(expr) {
  Calls <- NULL
  out <- try(withCallingHandlers(expr, error = \(e) Calls <<- sys.calls()), silent = TRUE)
  if (inherits(out, 'try-error'))
    attr(out, 'Calls') <- .MPCallStack(Calls)
  out
}

# calls from the MP down to the error, without the handler frames
.MPCallStack <- function(Calls) {
  if (!length(Calls)) return(character(0))
  Calls <- Calls[-length(Calls)]
  IsMP  <- vapply(Calls, \(cl) is.call(cl) && identical(cl[[1]], quote(MPfunction)), logical(1))
  if (any(IsMP))
    Calls <- Calls[which(IsMP)[1]:length(Calls)]
  Drop <- vapply(Calls, \(cl) is.call(cl) && identical(cl[[1]], quote(.handleSimpleError)), logical(1))
  vapply(Calls[!Drop], \(cl) paste(deparse(cl, width.cutoff = 100L, nlines = 1L), collapse = ''),
         character(1))
}

.MPDebugRecord <- function(MPName, MPfunction, Data, Stock, Proj, sim, Year, Key, Result) {
  list(MPName   = MPName,
       MP       = MPfunction,
       Data     = Data,
       Stock    = Stock,
       Seed     = list(Seed = Proj@OM@Seed, Sim = .GlobalSim(Proj@OM, sim), Year = Year, Key = Key),
       Calls    = attr(Result, 'Calls', exact = TRUE),
       Returned = if (!inherits(Result, 'try-error')) Result)
}

#' Split Debug Records off the Advice of One Simulation
#'
#' Removes the `Debug` attribute that `.CalcAdviceSimMP()` and
#' `.CalcAdviceSimMMP()` attach to failed MP calls, so the advice stored in
#' `Proj@@Misc$MPAdvice` does not keep copies of the `Data` objects.
#'
#' @param AdviceList Named list of `Advice` objects and failures for one
#'   simulation, or a `try-error` for the whole simulation.
#'
#' @return A list with `Value` (`AdviceList` without the attributes) and
#'   `Debug` (a named list of debug records keyed by stock/complex name, or
#'   by `.Sim` for a failure of the whole simulation).
#' @keywords internal
.SplitMPDebug <- function(AdviceList) {
  Take <- function(x) {
    d <- attr(x, 'Debug', exact = TRUE)
    attr(x, 'Debug') <- NULL
    list(x, d)
  }
  if (!is.list(AdviceList)) {
    if (is.null(attr(AdviceList, 'Debug', exact = TRUE)))
      return(list(Value = AdviceList, Debug = NULL))
    out <- Take(AdviceList)
    return(list(Value = out[[1]], Debug = list(.Sim = out[[2]])))
  }
  Debug <- list()
  for (j in seq_along(AdviceList)) {
    x <- AdviceList[[j]]
    if (inherits(x, 'advice') || is.null(attr(x, 'Debug', exact = TRUE))) next
    out <- Take(x)
    AdviceList[j] <- list(out[[1]])
    Debug[[names(AdviceList)[j]]] <- out[[2]]
  }
  list(Value = AdviceList, Debug = if (length(Debug)) Debug)
}

.MPDebugMode <- function() {
  Value <- getOption('MSEtool.DebugMP', 'first')
  if (isFALSE(Value) || is.null(Value)) return('none')
  if (isTRUE(Value)) return('first')
  if (is.character(Value) && length(Value) == 1 && Value %in% c('first', 'all'))
    return(Value)
  cli::cli_abort("{.code options(MSEtool.DebugMP)} must be {.val first}, {.val all}, or {.val {FALSE}}.")
}

# keeps the debug record on the first error entry only, unless every record is kept
.CapMPDebug <- function(Entries, Mode = .MPDebugMode()) {
  if (Mode == 'all') return(Entries)
  Has <- which(vapply(Entries, \(e) .IsLogEntry(e) && !is.null(e$debug), logical(1)))
  Drop <- if (Mode == 'none') Has else Has[-1]
  for (i in Drop) Entries[[i]]$debug <- NULL
  Entries
}

.HasMPDebug <- function(Entries)
  any(vapply(Entries, \(e) .IsLogEntry(e) && !is.null(e$debug), logical(1)))

.MsgDebugHint <- function(Entries) {
  if (.HasMPDebug(Entries))
    cli::cli_bullets(c(" " = cli::col_grey("Use {.code DebugMP(MSE)} to inspect the inputs of the failed MP call.")))
  invisible(NULL)
}

# argument values of an MP other than its data argument
.MPArgs <- function(MPfunction) {
  Legacy <- attr(MPfunction, 'LegacyMP', exact = TRUE)
  Fun    <- if (is.function(Legacy)) Legacy else MPfunction
  Skip   <- if (is.function(Legacy)) c('x', 'Data', 'reps', 'plot', '...') else c('Data', 'DataList', '...')
  Formals <- as.list(formals(Fun))
  Formals <- Formals[setdiff(names(Formals), Skip)]
  Env <- environment(Fun) %||% globalenv()
  lapply(Formals, \(f) {
    if (identical(f, quote(expr = ))) return(f)
    tryCatch(eval(f, Env), error = \(e) f)
  })
}

.RerunMP <- function(x, Browse = FALSE) {
  Fun    <- x$Function
  Legacy <- attr(Fun, 'LegacyMP', exact = TRUE)
  if (is.function(Legacy)) {
    Fun  <- Legacy
    Call <- \() Fun(x = 1, Data = Convert(x$Data, silent = TRUE), reps = 1, plot = FALSE)
  } else if (inherits(Fun, 'mmp')) {
    Call <- \() Fun(DataList = x$Data)
  } else {
    Call <- \() Fun(Data = x$Data)
  }
  if (Browse) debugonce(Fun)
  .WithMPSeed(Call(), x$Seed$Seed, x$Seed$Sim, x$Seed$Year, x$Seed$Key)
}
