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
  
  pYears <- length(ProjYears)
  Years <- c(HistYears, ProjYears)
  
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
  
  if (!is.null(Catchability@qInc)) {
    if (all(Catchability@qInc==0)) {
      Catchability@qInc <- NULL
      
    } else {
      qIncs <- StructurePars_(Catchability@qInc, nSim = nSim, Years = Years)[, 1]
      qIncs <- sapply(qIncs, function(x) (1 + x / 100)^(1:pYears)) |> t()
      dimnames(qIncs) <- list(Sim = 1:nSim, Year = ProjYears)
      qIncs <- ReduceDims(qIncs)
      q_sims <- dimnames(Catchability@Efficiency)[['Sim']] |> as.numeric()
      qinc_sims <- dimnames(qIncs)[['Sim']] |> as.numeric()
      
      if (length(q_sims)>1 || length(qinc_sims)>1) {
        allSims <- c(q_sims, qinc_sims) |> unique() 
        maxSims <- 1
        if (length(allSims)>1)
          maxSims <- max(allSims)
        
        Catchability@Efficiency <- ExtendSims(Catchability@Efficiency, maxSims)
        qIncs <- ExtendSims(qIncs, maxSims)
      }
      
      qfuture <- ArrayMultiply(SubsetYear(Catchability@Efficiency, ProjYears), qIncs)
      ArrayFill(Catchability@Efficiency) <- qfuture
      Catchability@qInc <- qIncs
    }
    

  }
  
  if (!is.null(Catchability@qCV)) {
    if (all(Catchability@qCV==0)) {
      Catchability@qCV <- NULL
    } else {
      qCVs <- StructurePars_(Catchability@qCV, nSim = nSim, Years = Years)[, 1]
      Catchability@qCV <- qCVs
      qmu <- -0.5 * qCVs^2
      qvar <- array(
        exp(rnorm(pYears * nSim, rep(qmu, pYears), rep(qCVs, pYears))),
        dim = c(nSim, pYears),
        dimnames = list(Sim = 1:nSim, Year = ProjYears)
      )
      qfuture <- ArrayMultiply(SubsetYear(Catchability@Efficiency, ProjYears), qvar)
      if (!all(qfuture == 1)) {
        ArrayFill(Catchability@Efficiency) <- qfuture
      }
    }

  }
  
  Catchability
}
