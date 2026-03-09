#' Initialise an `mse` Object from a `hist` Object
#'
#' Constructs a skeleton [mse-class] object from a completed [hist-class]
#' object by copying the operating model, unfished reference state, reference
#' points, and historical time-series slots via [CopyTimeseriesSlots()]. MP
#' functions are attached via [Add_MP_Functions()] and projection-period
#' arrays are pre-allocated via [InitializeTimeSeries()].
#'
#' @param Hist A populated [hist-class] object.
#' @param MPNames Character vector of MP function names to attach to the MSE.
#'
#' @return An [mse-class] object with historical slots populated and
#'   projection arrays initialised, ready for closed-loop simulation.
#' @keywords internal
Hist2MSE <- function(Hist, MPNames) {
  MSE           <- new('mse')
  MSE@OM        <- Hist@OM
  MSE@Unfished  <- Hist@Unfished
  MSE@Reference <- Hist@Reference
  MSE           <- CopyTimeseriesSlots(MSE, Hist)
  MSE           <- Add_MP_Functions(MSE, MPNames)
  MSE           <- InitializeTimeSeries(MSE, 'Projection', MPs=MPNames)
  MSE
}

#' Copy and Subset Historical Time-Series Slots into an `mse` Object
#'
#' Copies each slot from the [timeseries-class] inherited by `Hist` into the
#' corresponding slot of `MSE@Hist`, subsetting each array to the historical
#' years only via `SubsetYear`.
#'
#' @param MSE An [mse-class] object whose `Hist` slot will be populated.
#' @param Hist A populated [hist-class] object.
#'
#' @return `MSE` with `MSE@Hist` slots populated and subsetted to the
#'   historical period.
#' @keywords internal
CopyTimeseriesSlots <- function(MSE, Hist) {
  HistYears <- Years(Hist, 'H')
  for (sl in slotNames(MSE@Hist)) {
    slot(MSE@Hist, sl) <- slot(Hist, sl) |> SubsetYear(HistYears)
  }
  MSE
}

#' Attach MP Functions to an `mse` Object
#'
#' Validates that all names in `MPNames` correspond to functions of class
#' `"mp"` via [CheckMPClass()], then wraps each in a self-contained
#' environment via [MakeSelfContained()] and stores them in `MSE@MPs`.
#'
#' @param MSE An [mse-class] object.
#' @param MPNames Character vector of MP function names.
#'
#' @return `MSE` with `MSE@MPs` populated by valid, self-contained MP
#'   functions named by their original names.
#' @keywords internal
Add_MP_Functions <- function(MSE, MPNames) {
  CheckMPClass(MPNames)
  
  MPs <- lapply(MPNames, \(x) MakeSelfContained(get(x)))
  MSE@MPs        <- MPs
  names(MSE@MPs) <- MPNames
  MSE
}



#' Check That All MPs are of Class `"mp"`
#'
#' Retrieves each function named in `MPs` and aborts with an informative error
#' listing all invalid MPs if any is not of class `"mp"`. Used as an upfront
#' validation step before running an MSE.
#'
#' @param MPs Character vector of MP function names.
#'
#' @return `NULL` invisibly if all MPs are valid. Otherwise throws an error.
#' @keywords internal
CheckMPClass <- function(MPs) {
  CheckClass(MPs, 'character', 'MPs')
  
  MP_funs    <- purrr::map(MPs, get)
  is_mp      <- vapply(MP_funs, \(f) inherits(f, 'mp'), logical(1))
  names(is_mp) <- MPs
  
  invalid <- names(is_mp)[!is_mp]
  if (length(invalid))
    cli::cli_abort(c(
      "All MPs must be of class {.cls mp}.",
      "x" = "The following are not: {.val {invalid}}."
    ))
  
  invisible(NULL)
}


#' Detect Helper Functions Called by an MP
#'
#' Recursively scans the body of `MP` and any detected helper functions,
#' returning the names of all user-defined functions reachable from `MP` that
#' are not part of the MSEtool namespace or base R. Used by
#' [MakeSelfContained()] to identify functions that must be copied into the
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
DetectCalledFunctions <- function(MP, MSEtool_funs, visited=character()) {
  skip <- c("{", "<-", "=", "(", "[", "[[",
            "if", "for", "while", "repeat", "return")
  
  find_calls <- function(expr) {
    if (!is.call(expr)) return(NULL)
    fun <- expr[[1]]
    out <- character()
    if (is.symbol(fun)) {
      fname <- as.character(fun)
      if (!fname %in% skip &&
          !fname %in% MSEtool_funs &&
          exists(fname, mode="function", inherits=TRUE))
        out <- fname
    }
    c(out, unlist(lapply(as.list(expr)[-1], find_calls)))
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
    DetectCalledFunctions(fun, MSEtool_funs, visited)
  }))
  
  unique(c(direct, deeper))
}

#' Make an MP Self-Contained by Capturing Helper Functions
#'
#' Creates a new function environment for `MP`, parented to the MSEtool
#' namespace so that all package functions are findable via normal lexical
#' scoping without being copied. Only user-defined helper functions detected
#' by [DetectCalledFunctions()] are copied explicitly into the new environment.
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
MakeSelfContained <- function(MP) {
  CheckClass(MP, 'mp', 'MP')
  
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
  
  helpers <- DetectCalledFunctions(MP, MSEtool_funs=MSEtool_funs)
  helpers <- Filter(\(fname) {
    exists(fname, envir=mp_env, inherits=TRUE) &&
      !isNamespace(environment(get(fname, envir=mp_env, inherits=TRUE)))
  }, helpers)
  
  for (fname in helpers)
    env[[fname]] <- get(fname, envir=mp_env, inherits=TRUE)
  
  environment(MP) <- env
  MP
}




