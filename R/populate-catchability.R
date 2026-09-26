#' Populate a Catchability Object
#'
#' Populate a `Catchability` object by setting fleet/gear efficiency values for
#' historical and projected years, and applying optional stochastic or
#' increasing catchability parameters.
#'
#' @param Catchability A [Catchability()] object to populate.
#' @param nSim Integer. Number of simulation replicates.
#' @param HistYears Numeric vector of historical years.
#' @param ProjYears Numeric vector of projected years.
#' @param seed Integer. Random number seed used for stochastic generation.
#' @param silent Logical; if `TRUE`, suppresses messages.
#'
#' @details
#' `PopulateCatchability()` performs the following steps:
#'
#' * Initializes `Efficiency` to 1 if not provided.
#' * Checks that `Efficiency` has correct dimensions (rows = 1 or `nSim`).
#' * Adds dimension names if missing.
#' * Applies `qInc` (annual increase in catchability) if provided.
#' * Applies `qCV` (coefficient of variation) to introduce stochasticity.
#'
#' After population, `qInc` and `qCV` hold the per-simulation values used, and
#' the multipliers applied to the projection years of `Efficiency` are stored
#' in `Misc$qAdjust`. Re-populating leaves `Efficiency` unchanged while `qInc`
#' and `qCV` are unchanged. If either is modified, its previous multiplier is
#' divided out of `Efficiency` before the new one is applied.
#'
#' @return
#' A populated [Catchability()] object.
#'
#' @examples
#' \dontrun{
#' Catch <- Catchability()
#' Catch_pop <- PopulateCatchability(
#'   Catchability = Catch,
#'   nSim = 50,
#'   HistYears = 2000:2020,
#'   ProjYears = 2021:2025,
#'   seed = 123
#' )
#' }
#'
#' @export
PopulateCatchability <- function(Catchability,
                                 nSim = 5,
                                 HistYears = NULL,
                                 ProjYears = NULL,
                                 seed = NULL,
                                 silent = FALSE) {
  
  Years  <- c(HistYears, ProjYears)
  nSim   <- .GetNSim(Catchability, nSim)
  
  if (is.null(Catchability@Efficiency)) {
    Catchability@Efficiency <- array(
      1,
      dim = c(nSim, length(Years)),
      dimnames = list(
        Sim = 1:nSim,
        Year = Years
      )
    )
  }
  
  dd <- dim(Catchability@Efficiency)
  if (is.null(dd)) {
    Catchability@Efficiency <- array(Catchability@Efficiency, dim=c(1, 1),
                                     dimnames=list(
                                       Sim=1,
                                       Year=HistYears[1]
                                       )
                                     )
    dd <- dim(Catchability@Efficiency)
  }
  
  if (dd[1] != nSim && dd[1] != 1) {
    cli::cli_abort(c(
      "x" = "Incorrect number of rows in matrix: `Fleet |> Catchability() |> Efficiency()`",
      "i" = "Must have either 1 row or `nSim` ({.val {nSim}}) rows."
    ))
  }
  
  if (is.null(dimnames(Catchability@Efficiency))) {
    dimnames(Catchability@Efficiency) <- list(
      Sim = 1:nrow(Catchability@Efficiency),
      Year = Years[1:ncol(Catchability@Efficiency)]
    )
  }
  
  Catchability@Efficiency <- ExtendYears(
    Catchability@Efficiency,
    Years = HistYears
  )
  
  for (nm in c("qInc", "qCV")) {
    if (!is.null(slot(Catchability, nm)) && all(slot(Catchability, nm) == 0))
      slot(Catchability, nm) <- NULL
    Catchability <- .ApplyqAdjust(Catchability, nm, nSim, HistYears, ProjYears)
  }

  Catchability
}

# Misc$qAdjust records the multiplier already in projection Efficiency so re-populating never compounds it
.ApplyqAdjust <- function(Catchability, nm, nSim, HistYears, ProjYears) {
  Par       <- slot(Catchability, nm)
  Prev      <- Catchability@Misc$qAdjust[[nm]]
  PrevYears <- dimnames(Prev$Multiplier)$Year
  Applied   <- !is.null(Prev) &&
    all(PrevYears %in% dimnames(Catchability@Efficiency)$Year) &&
    !any(PrevYears %in% as.character(HistYears))

  if (Applied && identical(Prev$Par, Par) && identical(PrevYears, as.character(ProjYears)))
    return(Catchability)

  if (Applied)
    ArrayFill(Catchability@Efficiency) <- ArrayDivide(
      .SubsetYear(Catchability@Efficiency, PrevYears), Prev$Multiplier)
  if (!is.null(Prev))
    Catchability@Misc$qAdjust[[nm]] <- NULL

  pYears <- length(ProjYears)
  if (is.null(Par) || !pYears)
    return(Catchability)

  Par <- .StructurePar(Par, nSim = nSim, Years = c(HistYears, ProjYears))[, 1] |> unname()
  Multiplier <- if (nm == "qInc") {
    outer(Par, seq_len(pYears), \(x, t) (1 + x / 100)^t)
  } else {
    matrix(exp(rnorm(pYears * nSim, rep(-0.5 * Par^2, pYears), rep(Par, pYears))), nrow = nSim)
  }
  dimnames(Multiplier) <- list(Sim = seq_len(nrow(Multiplier)), Year = ProjYears)

  if (nrow(Multiplier) > 1)
    Catchability@Efficiency <- Extend(Catchability@Efficiency, nSim = nSim)
  ArrayFill(Catchability@Efficiency) <- ArrayMultiply(
    .SubsetYear(Catchability@Efficiency, ProjYears), Multiplier)

  slot(Catchability, nm) <- Par
  Catchability@Misc$qAdjust[[nm]] <- list(Par = Par, Multiplier = Multiplier)
  Catchability
}
