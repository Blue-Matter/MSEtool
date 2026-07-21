#' Combine Multiple `mse` Objects Across Simulations
#'
#' Combines a list of [mse-class] objects -- e.g. results from independent
#' batches of simulations of the same OM, or from different OMs that should
#' be treated as one large ensemble -- into a single `mse` object, by
#' concatenating every simulation-indexed array along the `Sim` dimension.
#' Used internally by [PM_FFMSY()] and the other `PM_*` functions whenever
#' `object` is a `list` rather than a single `mse`.
#'
#' All elements of `MSE_List` must share the same MPs (same names, same
#' order), the same stock names, and the same historical/projection years.
#'
#' Every slot of `mse` (including inherited [timeseries-class] slots, `Hist`,
#' `Unfished`, and `Reference`) is combined by concatenating along `Sim`,
#' except `OM`, `MPs`, `PPD`, `Log`, and `Misc`, which are taken from the
#' first element of `MSE_List` (only `OM@nSim` is updated, to the combined
#' total). This makes `CombineMSE()` suitable for computing performance
#' metrics over the combined ensemble, but the result should not be used to
#' resume/extend a projection.
#'
#' @param MSE_List A list of [mse-class] objects.
#' @param silent Logical. Suppress the summary message. Default `FALSE`.
#'
#' @return A single [mse-class] object spanning all simulations in
#'   `MSE_List`.
#'
#' @seealso [CombineOMs()], [CombineFleets()]
#' @export
CombineMSE <- function(MSE_List, silent = FALSE) {
  .CheckClass(MSE_List, 'list', 'MSE_List')
  purrr::walk(MSE_List, .CheckClass, class = 'mse', name = 'element of `MSE_List`')

  if (length(MSE_List) == 1)
    return(MSE_List[[1]])

  ref      <- MSE_List[[1]]
  RefMPs   <- names(MPs(ref))
  RefStock <- StockNames(ref)
  RefYears <- Years(ref@OM)

  purrr::walk(MSE_List[-1], \(m) {
    if (!identical(names(MPs(m)), RefMPs))
      cli::cli_abort("All `mse` objects in `MSE_List` must evaluate the same MPs, in the same order.")
    if (!identical(StockNames(m), RefStock))
      cli::cli_abort("All `mse` objects in `MSE_List` must share the same stock names.")
    if (!identical(Years(m@OM), RefYears))
      cli::cli_abort("All `mse` objects in `MSE_List` must share the same historical and projection years.")
  })

  out   <- ref
  skip  <- c('OM', 'MPs', 'PPD', 'Log', 'Misc')
  slots <- setdiff(methods::slotNames(ref), skip)

  for (sl in slots) {
    values           <- purrr::map(MSE_List, \(m) methods::slot(m, sl))
    slot(out, sl)    <- purrr::reduce(values, .CombineSimwise)
  }

  totalSim       <- sum(purrr::map_dbl(MSE_List, nSim))
  out            <- .RelabelSim(out, totalSim)
  out@OM@nSim    <- totalSim

  if (!silent)
    cli::cli_alert_success("Combined {length(MSE_List)} `mse` objects into {totalSim} total simulations")

  out
}

# Recursively concatenate two S4/array/list structures along their `Sim` dimension.
.CombineSimwise <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.null(b)) return(a)

  if (isS4(a)) {
    for (sl in methods::slotNames(a))
      methods::slot(a, sl) <- .CombineSimwise(methods::slot(a, sl), methods::slot(b, sl))
    return(a)
  }

  if (is.array(a)) {
    dn <- names(dimnames(a))
    if (!is.null(dn) && 'Sim' %in% dn) {
      out <- abind::abind(a, b, along = match('Sim', dn))
      names(dimnames(out)) <- dn
      return(out)
    }
    return(a)
  }

  if (is.list(a)) {
    if (length(a) == length(b))
      return(purrr::map2(a, b, .CombineSimwise))
    return(a)
  }

  a
}

# Recursively relabel every `Sim` dimname to sequential integers after combining.
.RelabelSim <- function(x, totalSim) {
  if (isS4(x)) {
    for (sl in methods::slotNames(x))
      methods::slot(x, sl) <- .RelabelSim(methods::slot(x, sl), totalSim)
    return(x)
  }

  if (is.array(x)) {
    dn <- dimnames(x)
    nm <- names(dn)
    if (!is.null(nm) && 'Sim' %in% nm && dim(x)[match('Sim', nm)] == totalSim)
      dn[[match('Sim', nm)]] <- as.character(seq_len(totalSim))
    dimnames(x) <- dn
    return(x)
  }

  if (is.list(x))
    return(purrr::map(x, .RelabelSim, totalSim = totalSim))

  x
}
