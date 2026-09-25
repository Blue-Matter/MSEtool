#' Initialise an `mse` Object from a `hist` Object
#'
#' Constructs a skeleton [mse-class] object from a completed [hist-class]
#' object by copying the operating model, unfished reference state, reference
#' points, and historical time-series slots via `.CopyTimeseriesSlots()`. MP
#' functions are attached via `.AddMPFunctions()` and projection-period
#' arrays are pre-allocated via `.InitializeTimeSeries()`.
#'
#' @param Hist A populated [hist-class] object.
#' @param MPs Named list of MP functions (from `.ResolveMPs()`), or a
#'   character vector of MP function names.
#'
#' @return An [mse-class] object with historical slots populated and
#'   projection arrays initialised, ready for closed-loop simulation.
#' @keywords internal
.Hist2MSE <- function(Hist, MPs) {
  MPs <- .ResolveMPs(MPs)
  MSE           <- new('mse')
  MSE@OM        <- Hist@OM
  MSE@Unfished  <- Hist@Unfished
  MSE@Reference <- Hist@Reference
  MSE           <- .CopyTimeseriesSlots(MSE, Hist)
  MSE           <- .AddMPFunctions(MSE, MPs)
  MSE           <- .InitializeTimeSeries(MSE, 'Projection', MPs = names(MPs))
  MSE
}

#' Copy and Subset Historical Time-Series Slots into an `mse` Object
#'
#' Copies each slot from the [timeseries-class] inherited by `Hist` into the
#' corresponding slot of `MSE@Hist`, subsetting each array to the historical
#' years only via `.SubsetYear`.
#'
#' @param MSE An [mse-class] object whose `Hist` slot will be populated.
#' @param Hist A populated [hist-class] object.
#'
#' @return `MSE` with `MSE@Hist` slots populated and subsetted to the
#'   historical period.
#' @keywords internal
.CopyTimeseriesSlots <- function(MSE, Hist) {
  HistYears <- Years(Hist, 'H')
  for (sl in slotNames(MSE@Hist)) {
    slot(MSE@Hist, sl) <- slot(Hist, sl) |> .SubsetYear(HistYears)
  }
  MSE
}

#' Attach MP Functions to an `mse` Object
#'
#' Wraps any legacy `"MP"` functions via `.WrapLegacyMP()` so they can be
#' called by the new pipeline, then wraps each in a self-contained
#' environment via `.MakeSelfContained()` and stores them in `MSE@MPs`.
#'
#' @param MSE An [mse-class] object.
#' @param MPs Named list of MP functions (from `.ResolveMPs()`), or a
#'   character vector of MP function names.
#'
#' @return `MSE` with `MSE@MPs` populated by valid, self-contained MP
#'   functions, named as in `MPs`.
#' @keywords internal
.AddMPFunctions <- function(MSE, MPs) {
  MPs <- .ResolveMPs(MPs)

  Fns <- purrr::imap(MPs, \(fn, x) {
    if (inherits(fn, 'MMP'))
      cli::cli_abort("Legacy {.cls MMP} management procedures are not yet supported ({.val {x}}).")
    if (inherits(fn, 'MP'))
      fn <- .WrapLegacyMP(fn)
    .MakeSelfContained(fn)
  })
  MSE@MPs <- Fns
  MSE
}

#' Resolve Management Procedures to a Named List of Functions
#'
#' Accepts a character vector of MP function names, a named list of MP
#' functions, or a list mixing both. Character elements are looked up with
#' [get()] and named by themselves unless a name is given; function elements
#' must be named. Validates each with `.CheckMPClass()`.
#'
#' @param MPs Character vector, or list of character strings and/or
#'   functions.
#'
#' @return A named list of MP functions.
#' @keywords internal
.ResolveMPs <- function(MPs) {
  if (is.function(MPs))
    cli::cli_abort("Supply a single MP function as a named list, e.g. {.code MPs = list(MyMP = MyMP)}.")
  if (is.character(MPs))
    MPs <- as.list(MPs)
  if (!is.list(MPs) || !length(MPs))
    cli::cli_abort("{.arg MPs} must be a character vector of MP names, or a named list of MP functions.")

  Names <- names(MPs)
  if (is.null(Names)) Names <- rep('', length(MPs))
  Names[is.na(Names)] <- ''

  Fns <- vector('list', length(MPs))
  for (i in seq_along(MPs)) {
    x <- MPs[[i]]
    if (is.character(x)) {
      if (length(x) != 1)
        cli::cli_abort("Character elements of {.arg MPs} must each be a single MP name.")
      if (!nzchar(Names[i])) Names[i] <- x
      if (!exists(x, mode = 'function'))
        cli::cli_abort("MP {.val {x}} not found.")
      x <- get(x, mode = 'function')
    } else if (!is.function(x)) {
      cli::cli_abort("Element {i} of {.arg MPs} must be an MP name or an MP function.")
    } else if (!nzchar(Names[i])) {
      cli::cli_abort("MP functions in {.arg MPs} must be named, e.g. {.code MPs = list(MyMP = MyMP)}.")
    }
    Fns[[i]] <- x
  }
  names(Fns) <- Names

  Dup <- unique(Names[duplicated(Names)])
  if (length(Dup))
    cli::cli_abort("Duplicated MP name{?s}: {.val {Dup}}.")

  .CheckMPClass(Fns)
  Fns
}

#' Check That All MPs are of Class `"mp"`, `"mmp"`, `"MP"`, or `"MMP"`
#'
#' Aborts with an informative error listing all invalid MPs if any is not one
#' of the native (`"mp"`/`"mmp"`) or legacy DLMtool/SAMtool (`"MP"`/`"MMP"`)
#' management-procedure classes. Legacy `"MP"` functions are wrapped for the
#' new pipeline by `.WrapLegacyMP()` inside `.AddMPFunctions()`.
#'
#' @param MPs Named list of MP functions, or a character vector of MP
#'   function names.
#'
#' @return `NULL` invisibly if all MPs are valid. Otherwise throws an error.
#' @keywords internal
.CheckMPClass <- function(MPs) {
  if (is.character(MPs))
    MPs <- stats::setNames(purrr::map(MPs, get), MPs)

  is_mp <- vapply(MPs, \(f) inherits(f, c('mp', 'mmp', 'MP', 'MMP')), logical(1))

  invalid <- names(MPs)[!is_mp]
  if (length(invalid))
    cli::cli_abort(c(
      "All MPs must be of class {.cls mp}, {.cls mmp}, or legacy {.cls MP}/{.cls MMP}.",
      "x" = "The following are not: {.val {invalid}}."
    ))

  invisible(NULL)
}

#' Detect Helper Functions Called by an MP
#'
#' Recursively scans the body of `MP` and any detected helper functions,
#' returning the names of all user-defined functions reachable from `MP` that
#' are not part of the MSEtool namespace or base R. Detects both functions
#' called directly (`f(...)`) and functions referenced by name as a bare
#' argument value (e.g. passed as a callback to another function). Used by
#' `.MakeSelfContained()` to identify functions that must be copied into the
#' MP's self-contained environment.
#'
#' @param MP A function of class `"mp"`.
#' @param MSEtool_funs Character vector of function names in the MSEtool
#'   namespace, used to exclude them from results.
#' @param visited Character vector of already-visited function names, used
#'   internally to prevent infinite recursion.
#'
#' @return A character vector of unique helper function names.
#' @keywords internal
.DetectCalledFunctions <- function(MP, MSEtool_funs, visited=character()) {
  skip <- c("{", "<-", "=", "(", "[", "[[", "::", ":::",
            "if", "for", "while", "repeat", "return", "function")

  find_calls <- function(expr) {
    if (is.symbol(expr)) {
      fname <- as.character(expr)
      if (!nzchar(fname) || fname %in% skip || fname %in% MSEtool_funs)
        return(NULL)
      if (exists(fname, mode="function", inherits=TRUE))
        return(fname)
      return(NULL)
    }
    if (!is.call(expr)) return(NULL)

    fun <- expr[[1]]
    if (is.symbol(fun) && as.character(fun) %in% c("::", ":::"))
      return(NULL)

    unlist(lapply(as.list(expr), find_calls))
  }

  direct      <- unique(find_calls(body(MP)))
  new_helpers <- setdiff(direct, c(MSEtool_funs, visited))
  visited     <- union(visited, new_helpers)
  
  deeper <- unlist(lapply(new_helpers, function(fname) {
    fun <- tryCatch(
      get(fname, mode="function", inherits=TRUE),
      error = function(e) NULL
    )
    if (is.null(fun) || isNamespace(environment(fun)))
      return(NULL)
    .DetectCalledFunctions(fun, MSEtool_funs, visited)
  }))
  
  unique(c(direct, deeper))
}

#' Make an MP Self-Contained by Capturing Helper Functions
#'
#' Creates a new function environment for `MP`, parented to the MSEtool
#' namespace so that all MSEtool package functions are findable via normal
#' lexical scoping without being copied. Every other helper function detected
#' by `.DetectCalledFunctions()`.
#'
#' This ensures that an MP carries all of its dependencies when passed to a
#' parallel worker or saved to disk, without bundling the entire MSEtool
#' namespace.
#'
#' @param MP A function of class `"mp"`.
#'
#' @return `MP` with its environment replaced by a self-contained environment
#'   parented to the MSEtool namespace.
#' @keywords internal
.MakeSelfContained <- function(MP) {
  .CheckClass(MP, c('mp', 'mmp'), 'MP')

  env    <- new.env(parent=asNamespace("MSEtool"))
  mp_env <- environment(MP)

  MSEtool_funs <- names(Filter(
    isTRUE,
    vapply(
      ls(getNamespace("MSEtool"), all.names=TRUE),
      \(x) is.function(get(x, envir=asNamespace("MSEtool"))),
      logical(1)
    )
  ))

  helpers <- .DetectCalledFunctions(MP, MSEtool_funs=MSEtool_funs)
  helpers <- Filter(\(fname) exists(fname, envir=mp_env, inherits=TRUE), helpers)

  for (fname in helpers)
    env[[fname]] <- get(fname, envir=mp_env, inherits=TRUE)

  environment(MP) <- env
  MP
}


