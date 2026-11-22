#' Reduce the number of simulations 
#' 
#' Subsets an [OM()] or [Hist()] object by reducing number
#' of simulations to `nSim`
#' 
#' @param object An [OM()] or [Hist()] object
#' @param nSim Integer. The maximum number of simulations
#' 
#' @return The [OM()] or [Hist()] with reduced `nSim`
#' 
#' @example man-examples/ReduceNSim.R
#' @export
#' @seealso [Reduce()]
ReduceNSim <- function(object, nSim = NULL) {
  if (is.null(nSim)) {
    return(object)
  }
  
  .CheckClass(object, c("om", "hist"), "object")
  .CheckClass(nSim, c("numeric", "integer"), "nSim")
  
  if (length(nSim) > 1) {
    cli::cli_abort("`nSim` ({.val {nSim}}) must be length 1")
  }
  
  if (nSim < 1) {
    cli::cli_abort("`nSim` ({.val {nSim}}) must be >= 1")
  }
  
  if (nSim > nSim(object)) {
    cli::cli_alert_warning("Argument `nSim` ({.val {nSim}}) is greater than {.run nSim(OM)} ({.val {nSim(object)}}). Ignoring argument `nSim`  ")
    nSim <- nSim(object)
  }
  
  .SubsetSim(object, Sims = 1:nSim)
}

# Trims arrays and per-sim vectors populated for more sims than `OM@nSim`;
# aborts if a populated OM holds fewer, since drawn values replace the sampling inputs
.DropStaleSims <- function(OM) {
  .CheckNSimNotIncreased(OM)
  .CheckYearsNotChanged(OM)
  nOld <- .GetNSim(OM)
  if (is.null(nOld) || nOld <= OM@nSim) return(OM)
  .SubsetSim(OM, Sims = seq_len(OM@nSim), nSim = nOld)
}

.CheckNSimNotIncreased <- function(OM) {
  if (is.null(attr(OM, "digest"))) return(invisible())
  # StockTargeting is rebuilt for the current nSim
  Slots <- setdiff(methods::slotNames(OM), "StockTargeting")
  nPop  <- max(0, vapply(Slots, \(s) .LongestSimDim(methods::slot(OM, s)), numeric(1)))
  if (nPop <= 1 || nPop >= OM@nSim) return(invisible())
  cli::cli_abort(c(
    "x" = "{.code nSim} cannot be increased on a populated OM: it was populated with {nPop} simulation{?s}, but {.code nSim} is now {OM@nSim}.",
    "i" = "Populating replaces sampling inputs (e.g. parameter bounds) with the drawn values.",
    "i" = "Set {.code nSim} on the OM before calling {.fn Populate} or {.fn Simulate}."
  ), call = NULL)
}

# years are fixed once populated: drawn and conditioned arrays are labelled by the years they were populated for
.CheckYearsNotChanged <- function(object, Years = NULL, what = 'OM') {
  if (is.null(attr(object, "digest"))) return(invisible())
  Years  <- Years %||% CalcYears(object@nYear, object@pYear, object@CurrentYear, Seasons = object@Seasons %||% 1)
  Stocks <- if (methods::is(object, 'om')) .ConvertToList(object@Stock) else list(object)
  Old    <- .StockYearsMismatch(Stocks, Years)
  if (is.null(Old)) return(invisible())
  cli::cli_abort(c(
    "x" = "The years of a populated {what} cannot be changed: it was populated for {min(Old)}-{max(Old)}, but {.code nYear}, {.code pYear}, {.code CurrentYear}, and {.code Seasons} now give {min(Years)}-{max(Years)}.",
    "i" = "Populating labels drawn and conditioned values by year.",
    "i" = "An {.cls om} subset with {.code Subset(Years = )} keeps its original years and cannot be simulated.",
    "i" = "Set the years on the unpopulated {what} before calling {.fn Populate} or {.fn Simulate}."
  ), call = NULL)
}

# the stored years of the first stock that differ from `Years`, or NULL
.StockYearsMismatch <- function(Stocks, Years) {
  for (st in Stocks)
    if (length(st@Years) && !isTRUE(all.equal(as.numeric(st@Years), as.numeric(Years))))
      return(st@Years)
  NULL
}

# a hist subset with Subset(Years = ) no longer matches the years of its OM
.CheckHistYears <- function(Hist, Label = 'Hist') {
  YearsAll  <- Years(Hist)
  YearsHist <- as.numeric(Years(Hist, 'Historical'))
  Held <- .StockYearsMismatch(.ConvertToList(Hist@OM@Stock), YearsAll)
  if (is.null(Held)) {
    SBYears <- suppressWarnings(as.numeric(dimnames(Hist@SBiomass)$Year))
    if (length(SBYears) > 1 && !isTRUE(all.equal(SBYears, YearsHist)))
      Held <- SBYears
  }
  if (is.null(Held)) return(invisible())
  Held <- as.numeric(Held)
  cli::cli_abort(c(
    "x" = "{.arg {Label}} holds years {min(Held)}-{max(Held)}, but its OM's years are {min(YearsAll)}-{max(YearsAll)} (historical {min(YearsHist)}-{max(YearsHist)}).",
    "i" = "A {.cls hist} subset with {.code Subset(Years = )} can be summarised and plotted, but not projected.",
    "i" = "Project the full {.cls hist} object, then subset the resulting {.cls mse} object."
  ), call = NULL)
}

# a populated array has 1 simulation (constant) or nSim; more than 1 but fewer than nSim means nSim was increased after populating
.CheckSimsNotIncreased <- function(object, nSim, label = NULL) {
  if (is.null(nSim)) return(invisible())
  Problems <- .SimIncreaseProblems(object, nSim)
  if (!length(Problems))
    return(invisible())
  Fix <- paste0('`nSim` cannot be increased after an object is populated: arrays must have 1 simulation ',
                '(constant across simulations) or `nSim` (', nSim, '). Set `nSim` before populating.')
  .AbortInvalidInputs(c(Problems, Fix), class(object)[1], label)
}

.SimIncreaseProblems <- function(x, nSim, name = NULL) {
  if (is.null(x) || is.function(x) || is.environment(x)) return(NULL)
  if (isS4(x))
    return(unlist(lapply(setdiff(methods::slotNames(x), c('Misc', 'Log')), \(s) {
      if (isS4(methods::slot(x, s))) NULL else .SimIncreaseProblems(methods::slot(x, s), nSim, s)
    })))
  if (is.list(x) && !is.data.frame(x)) {
    Nms <- names(x) %||% seq_along(x)
    return(unlist(lapply(seq_along(x), \(i) .SimIncreaseProblems(x[[i]], nSim, paste0(name, '$', Nms[i])))))
  }
  n <- if (is.array(x)) length(dimnames(x)$Sim) else 0
  if (n > 1 && n < nSim)
    paste0('`', name, '` has ', n, ' simulations but `nSim` is ', nSim, '.')
}

.LongestSimDim <- function(x) {
  if (is.null(x) || is.function(x) || is.environment(x)) return(0)
  if (isS4(x))
    return(max(0, vapply(methods::slotNames(x), \(s) .LongestSimDim(methods::slot(x, s)), numeric(1))))
  if (is.list(x) && !is.data.frame(x))
    return(max(0, vapply(x, .LongestSimDim, numeric(1))))
  if (is.array(x)) length(dimnames(x)$Sim) else 0
}
