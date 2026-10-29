#' Initialise an `mse` Object from a `hist` Object
#'
#' Constructs a skeleton [mse-class] object from a completed [hist-class]
#' object by copying the operating model, unfished reference state, reference
#' points, and historical time-series slots via `.CopyTimeseriesSlots()`. MP
#' functions are attached via `.AddMPFunctions()`, the management interval
#' resolved for each MP is stored in `MSE@Misc$Interval`, and projection-period
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
  MSE@Misc$Interval <- .MPIntervals(MSE@OM, MSE@MPs)
  MSE           <- .InitializeTimeSeries(MSE, 'Projection', MPs = names(MPs))
  MSE
}

#' Copy and Subset Historical Time-Series Slots into an `mse` Object
#'
#' Copies each slot other than `Misc` from the [timeseries-class] inherited by `Hist` into the
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
  for (sl in setdiff(slotNames(MSE@Hist), 'Misc')) {
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
#' must be named. Warns via `.WarnIdenticalMPs()` if the same function is
#' supplied under more than one name, and validates each with `.CheckMPClass()`.
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

  .WarnIdenticalMPs(Fns)
  .CheckMPClass(Fns)
  Fns
}

#' Warn When Identical MP Functions are Supplied Under Different Names
#'
#' Compares each MP function with those before it using [identical()],
#' ignoring bytecode and source references, and warns once listing each MP
#' that duplicates an earlier one.
#'
#' @param MPs Named list of MP functions.
#'
#' @return `NULL` invisibly.
#' @keywords internal
.WarnIdenticalMPs <- function(MPs) {
  Nms   <- names(MPs)
  First <- rep(NA_character_, length(MPs))
  for (i in seq_along(MPs)[-1]) {
    for (j in seq_len(i - 1)) {
      if (is.na(First[j]) &&
          identical(MPs[[i]], MPs[[j]], ignore.bytecode = TRUE, ignore.srcref = TRUE)) {
        First[i] <- Nms[j]
        break
      }
    }
  }
  if (all(is.na(First))) return(invisible(NULL))

  Groups <- split(Nms[!is.na(First)], First[!is.na(First)])
  Msg <- purrr::imap_chr(Groups, \(dup, orig)
    cli::format_inline("{.val {dup}} {?is/are} identical to {.val {orig}}."))
  cli::cli_warn(c(
    "Identical MP functions supplied under different names; each will be projected separately with identical results.",
    stats::setNames(Msg, rep('!', length(Msg)))
  ))
  invisible(NULL)
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

#' Resolve a Free Name of an MP to a User Object
#'
#' Looks up `name` along the enclosing environments of an MP or helper
#' function, starting at `env`. Returns the object when it is defined in user
#' code (a closure frame, the global environment, or an attached package that
#' is not attached by default) and `NULL` when it is provided by MSEtool, its
#' imports, base R, a default package, or is not found.
#'
#' @param name Character. The free name.
#' @param env The environment the name is looked up from.
#'
#' @return A list with element `value`, or `NULL`.
#' @keywords internal
.ResolveUserObject <- function(name, env) {
  ns       <- asNamespace("MSEtool")
  Provided <- exists(name, envir = ns, inherits = FALSE) ||
    exists(name, envir = parent.env(ns), inherits = FALSE) ||
    exists(name, envir = baseenv(), inherits = FALSE)
  Default  <- paste0("package:", c("base", getOption("defaultPackages")))

  while (!identical(env, emptyenv())) {
    if (isNamespace(env) || identical(env, baseenv()))
      return(NULL)
    if (exists(name, envir = env, inherits = FALSE)) {
      EnvName <- environmentName(env)
      if (startsWith(EnvName, "package:"))
        return(if (Provided || EnvName %in% Default) NULL else list(value = get(name, envir = env)))
      if (identical(env, globalenv()) && Provided)
        return(NULL)
      return(list(value = get(name, envir = env)))
    }
    env <- parent.env(env)
  }
  NULL
}

# TRUE for closures defined in user code (not in a package namespace)
.IsUserFunction <- function(x) {
  is.function(x) && !is.primitive(x) && !isNamespace(topenv(environment(x)))
}

#' Make an MP Self-Contained by Capturing its Dependencies
#'
#' Creates a new function environment for `MP`, parented to the MSEtool
#' namespace so that all MSEtool package functions are findable via normal
#' lexical scoping without being copied. Every free name of `MP` that
#' resolves to a user object (see `.ResolveUserObject()`) - helper functions,
#' global variables, variables captured by a closure, and functions from
#' non-default attached packages - is copied into that environment. Copied
#' user-defined helper functions are scanned in turn from their original
#' environment and then re-homed into the same environment, so helpers
#' calling other helpers resolve. Names referenced only as strings (e.g. `do.call("f", ...)`) are
#' not detected.
#'
#' This ensures that an MP carries all of its dependencies when passed to a
#' parallel worker or saved to disk, without bundling the entire MSEtool
#' namespace.
#'
#' @param MP A function of class `"mp"`.
#' @param MaxSize Numeric. Total size (bytes) of copied non-function objects
#'   above which a warning is issued.
#'
#' @return `MP` with its environment replaced by a self-contained environment
#'   parented to the MSEtool namespace.
#' @keywords internal
.MakeSelfContained <- function(MP, MaxSize = 10 * 1024^2) {
  .CheckClass(MP, c('mp', 'mmp'), 'MP')

  env   <- new.env(parent = asNamespace("MSEtool"))
  Queue <- list(MP)
  while (length(Queue)) {
    fn    <- Queue[[1]]
    Queue <- Queue[-1]
    for (nm in codetools::findGlobals(fn, merge = TRUE)) {
      if (exists(nm, envir = env, inherits = FALSE)) next
      Obj <- .ResolveUserObject(nm, environment(fn))
      if (is.null(Obj)) next
      Value <- Obj$value
      if (.IsUserFunction(Value)) {
        Queue <- c(Queue, Value)
        environment(Value) <- env
      }
      env[[nm]] <- Value
    }
  }

  Vars <- Filter(\(x) !is.function(env[[x]]), ls(env, all.names = TRUE))
  Size <- vapply(Vars, \(x) as.numeric(utils::object.size(env[[x]])), numeric(1))
  if (sum(Size) > MaxSize)
    cli::cli_warn(c(
      "MP uses {.val {length(Vars)}} global object{?s} totalling {format(structure(sum(Size), class = 'object_size'), units = 'auto')}: {.val {Vars[order(-Size)]}}.",
      "i" = "These are stored with the MP and sent to every parallel task. Pass large inputs as MP arguments with {.fn SetMPArgs} or reduce them."
    ))

  environment(MP) <- env
  MP
}


