#' Populate an Imp Object
#'
#' Populates the `TAC`, `Effort`, and `Size` [impslot-class] sub-objects of
#' an [imp-class] object across simulation replicates and years.
#'
#' @param Imp An [imp-class] object.
#' @param nSim Integer. Number of simulation replicates.
#' @param HistYears Integer vector. Calendar years of the historical period.
#' @param ProjYears Integer vector. Calendar years of the projection period.
#'
#' @return A populated [imp-class] object.
#'
#' @seealso [PopulateImpSlot()], [imp-class], [impslot-class]
#' @export
PopulateImp <- function(Imp, nSim, HistYears, ProjYears) {
  .CheckClass(Imp, 'imp', 'Imp')
  Years <- c(HistYears, ProjYears)

  Imp@TAC    <- PopulateImpSlot(Imp@TAC,    nSim, Years)
  Imp@Effort <- PopulateImpSlot(Imp@Effort, nSim, Years)
  Imp@Size   <- PopulateImpSlot(Imp@Size,   nSim, Years)
  Imp
}

#' Populate an ImpSlot Object
#'
#' Expands `Mean` and `SD` to named `[Sim]` arrays, `Compliance` to a named
#' `[Sim x Year]` array, and generates a stochastic lognormal `Error` array
#' from `Mean`/`SD` if not supplied directly. Mirrors [PopulateCatchObs()]'s
#' `CV`/`Bias`/`Error` handling, applied to `Mean`/`SD` instead. Left
#' untouched (all slots `NULL`) if the input object is empty -- see
#' [EmptyObject()] -- so that the (common) case of an unconfigured
#' implementation model incurs no extra computation.
#'
#' @param ImpSlot An [impslot-class] object.
#' @param nSim Integer. Number of simulation replicates.
#' @param Years Integer vector. Calendar years (historical and projection).
#'
#' @details
#' `Compliance` is expanded to a `[Sim x Year]` array via
#' `.StructurePars()`, using the same conventions as every other structured
#' OM parameter: a scalar is a fixed value for every simulation and year; a
#' length-2 vector is `c(lower, upper)` bounds sampled once per simulation
#' (held constant across years); a full array is respected as-is, with named
#' `Year` dimnames treated as breakpoints and forward-filled to cover all of
#' `Years` (see [Extend()]/[ExtendYears()]).
#'
#' @return A populated [impslot-class] object.
#' @seealso [PopulateImp()], [impslot-class], [ImpSlot()]
#' @export
PopulateImpSlot <- function(ImpSlot, nSim, Years) {
  .CheckClass(ImpSlot, 'impslot', 'ImpSlot')
  if (EmptyObject(ImpSlot)) return(ImpSlot)

  nTS <- length(Years)

  # Mean: multiplicative bias, 1 = perfect implementation. A scalar is a
  # literal, fixed value for every simulation (unlike CatchObs's Bias,
  # which treats a scalar as a CV of noise around 1); a length-2 vector is
  # `c(lower, upper)` bounds sampled once per simulation -- matching
  # .StructurePars()'s convention used throughout the package.
  if (!length(ImpSlot@Mean)) {
    ImpSlot@Mean <- array(1, dim = nSim, dimnames = list(Sim = seq_len(nSim)))
  } else if (is.null(dimnames(ImpSlot@Mean))) {
    ImpSlot@Mean <- .StructurePars(list(ImpSlot@Mean), nSim)[[1]] |>
      ExtendSims(nSim) |> DropDimension('Year', warn = FALSE)
  }

  # SD: expand to [Sim]
  if (length(ImpSlot@SD) && is.null(dimnames(ImpSlot@SD))) {
    ImpSlot@SD <- .StructurePars(list(ImpSlot@SD), nSim)[[1]] |>
      ExtendSims(nSim) |> DropDimension('Year', warn = FALSE)
  }

  # Compliance: expand to [Sim x Year], unlike Mean/SD the Year dimension is
  # kept (not dropped) since Compliance can vary by year -- e.g. a fleet's
  # reconciliation behaviour tightening over the projection.
  if (length(ImpSlot@Compliance)) {
    ImpSlot@Compliance <- .StructurePars(list(ImpSlot@Compliance), nSim, Years)[[1]] |>
      ExtendSims(nSim) |> ExtendYears(Years)
  }

  # Error: [Sim x Year] lognormal draw from Mean/SD, unless supplied directly
  if (!length(ImpSlot@Error)) {
    mean_vec <- rep(as.numeric(ImpSlot@Mean), nTS)
    sd_vec   <- rep(if (length(ImpSlot@SD)) as.numeric(ImpSlot@SD) else rep(0, nSim), nTS)
    ImpSlot@Error <- array(
      rlnorm(nSim * nTS, mconv(mean_vec, sd_vec), sdconv(mean_vec, sd_vec)),
      dim      = c(nSim, nTS),
      dimnames = list(Sim = seq_len(nSim), Year = Years)
    )
  }

  ImpSlot
}
