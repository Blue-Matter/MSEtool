#' Populate Composition Observation Error
#'
#' Populate a [compobs-class] object by expanding `SampleSize`, `ESS`,
#' `Theta`, and `Shift` across simulation replicates, years, and composition
#' bins, and applying default values where slots are unspecified.
#'
#' @param Comp A [compobs-class] object. Used for landed catch-at-age
#'   (`Obs@LandingsAtAge`), discarded catch-at-age (`Obs@DiscardsAtAge`),
#'   landed catch-at-size (`Obs@LandingsAtSize`), and discarded catch-at-size
#'   (`Obs@DiscardsAtSize`).
#' @param nSim Integer. Number of simulation replicates.
#' @param HistYears Integer vector. Calendar years of the historical period.
#' @param ProjYears Integer vector. Calendar years of the projection period.
#' @param Bins Numeric vector. Age classes for age composition 
#' slots, or length class midpoints for size
#'   composition slots. Used to set the length of the `Bin` dimension and to
#'   name it in the populated `Shift` array. `NULL` is accepted only when
#'   [EmptyObject()] returns `TRUE` for `Comp`.
#' @param BinName Character scalar. Name used for the bin dimension in
#'   populated arrays, typically `"Age"` or `"Size"`.
#'
#' @details
#' If [EmptyObject()] returns `TRUE` for `Comp` (i.e. `SampleSize` is
#' `NULL`), the object is returned unchanged and no population is performed.
#'
#' Otherwise the following slots are populated:
#'
#' **`SampleSize`**: expanded to a named `[nSim x nYear]` array via
#' [PopulateObsScalar()], covering all historical and projection years. A
#' length-2 input `c(lower, upper)` is interpreted as bounds of a Uniform
#' distribution from which `nSim` values are drawn.
#'
#' **`ESS`**: expanded in the same way as `SampleSize`. If `NULL`, defaults
#' silently to the populated `SampleSize` array. Typically `ESS <= SampleSize`;
#' values of `ESS < SampleSize` produce overdispersion relative to a pure
#' multinomial with `SampleSize` draws.
#'
#' **`Theta`**: expanded to a named `[nSim x nYear]` array of values in
#' `(0, 1]`. If `NULL`, defaults silently to 1 (standard multinomial, no
#' additional overdispersion). A length-2 input is treated as Uniform bounds.
#'
#' **`Shift`**: expanded to a named `[nSim x nYear x nBin]` array via
#' [Extend()], where `nBin = length(Bins)`. Accepted input forms:
#' * `NULL` (default): no shift applied; slot remains `NULL`.
#' * Scalar: constant offset across all simulations, years, and bins.
#' * Vector of length `nBin`: bin-specific offset, constant across simulations
#'   and years.
#' * Named matrix with any subset of `Sim`, `Year`, and `Bin` dimensions with
#'   change-point years.
#' * Full `[nSim x nYear x nBin]` array.
#'
#' In conditioning mode, `ESS`, `Theta`, and `Shift` are populated internally
#' by [ConditionObs_Comp()] and should not be set by the user.
#'
#' ## Composition generation model
#'
#' After population, the fully-expanded slots are used by the simulation
#' engine to generate observed compositions as follows. Let **q** be the
#' OM-predicted composition vector for a given simulation, year, and fleet:
#'
#' 1. Base concentration: \eqn{\alpha = \mathrm{ESS} \times \mathbf{q}}
#' 2. Apply shift (if non-`NULL`):
#'    \eqn{\alpha_b' = \alpha_b \times \exp(\mathrm{Shift}[\mathrm{sim}, \mathrm{year}, b])}
#' 3. Dirichlet draw:
#'    \eqn{\mathbf{p}^* \sim \mathrm{Dirichlet}(\alpha' / \mathrm{Theta})}
#' 4. Multinomial draw:
#'    \eqn{\mathrm{obs} \sim \mathrm{Multinomial}(\mathrm{SampleSize}, \mathbf{p}^*)}
#'
#' @return A populated [compobs-class] object.
#'
#' @seealso
#' [CompObs()], [compobs-class], [PopulateObs()], [ConditionObs_Comp()]
#'
#' @export
PopulateCompObs <- function(Comp,
                            nSim,
                            HistYears,
                            ProjYears,
                            Bins    = NULL,
                            BinName = "Bin") {
  CheckClass(Comp, "compobs", "Comp")
  
  if (EmptyObject(Comp))
    return(Comp)
  
  if (is.null(Bins))
    cli::cli_abort(
      c("x" = "`Bins` must be provided when `Comp` is non-empty.",
        "i" = "Supply the age class vector (`AgeBins`) or length class vector (`SizeBins`) to `PopulateObs()`.")
    )
  
  Years <- c(HistYears, ProjYears)
  nBin  <- length(Bins)
  
  # SampleSize 
  Comp@SampleSize <- PopulateObsScalar(
    x     = Comp@SampleSize,
    nSim  = nSim,
    Years = Years,
    label = "SampleSize"
  )
  
  # ESS 
  # Default silently to SampleSize when unspecified
  if (is.null(Comp@ESS)) {
    Comp@ESS <- Comp@SampleSize
  } else {
    Comp@ESS <- PopulateObsScalar(
      x     = Comp@ESS,
      nSim  = nSim,
      Years = Years,
      label = "ESS"
    )
  }
  
  # Theta 
  # Default silently to 1 (standard multinomial) when unspecified
  if (is.null(Comp@Theta)) {
    Comp@Theta <- array(
      1,
      dim      = c(nSim, length(Years)),
      dimnames = list(Sim = seq_len(nSim), Year = Years)
    )
  } else {
    Comp@Theta <- PopulateObsScalar(
      x     = Comp@Theta,
      nSim  = nSim,
      Years = Years,
      label = "Theta"
    )
    if (any(Comp@Theta <= 0) || any(Comp@Theta > 1))
      cli::cli_abort(
        c("x" = "`Theta` values must be in (0, 1].",
          "i" = "`Theta = 1` recovers the standard multinomial; smaller values increase overdispersion.")
      )
  }
  
  # Shift
  if (!is.null(Comp@Shift)) {
    Comp@Shift <- PopulateObsShift(
      Shift   = Comp@Shift,
      nSim    = nSim,
      Years   = Years,
      Bins    = Bins,
      BinName = BinName
    )
  }
  Comp
}




