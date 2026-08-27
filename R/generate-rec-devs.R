#' Generate Stock-Recruitment Deviations
#'
#' Generate recruitment deviations for initial ages, historical years, and 
#' projected years, with optional auto-correlation and truncation.
#'
#' @param SD Numeric vector of standard deviations for the log recruitment
#'   deviations. This is the marginal standard deviation of the generated
#'   series, after autocorrelation and truncation.
#' @param AC Numeric vector of autocorrelation coefficients (0 = no autocorr).
#'   Must be within -1 and 1, exclusive.
#' @param TruncSD Numeric. Number of standard deviations at which the log
#'   deviations are bounded. Because the latent spread is widened to preserve
#'   the marginal standard deviation, the realised bound is somewhat wider than
#'   `TruncSD * SD`. Default `3`.
#' @param Ages An [Ages()] object defining age classes.
#' @param HistYears Numeric vector of historical years.
#' @param ProjYears Numeric vector of projected years.
#' @param nSim Integer. Number of simulation replicates.
#' @param RecDevInit Optional array of initial age recruitment deviations.
#' @param RecDevHist Optional array of historical recruitment deviations.
#' @param RecDevProj Optional array of projected recruitment deviations.
#' 
#'
#' `GenRecDevs()` generates recruitment deviations across initial ages,
#' historical years, and projected years. 
#'
#' If `RecDevInit`, `RecDevHist`, or `RecDevProj` are provided as non-NULL
#' arrays, they will not be overwritten and will be returned as-is.
#'
#' @return A list with three elements:
#' * `RecDevInit`: Array of recruitment deviations for initial ages.
#' * `RecDevHist`: Array of recruitment deviations for historical years.
#' * `RecDevProj`: Array of recruitment deviations for projection years.
#'
#' @examples
#' \dontrun{
#' # Assuming `Ages` object exists
#' recdevs <- GenRecDevs(
#'   SD = 0.2,
#'   AC = 0.3,
#'   TruncSD = 3,
#'   Ages = Ages,
#'   HistYears = 2000:2020,
#'   ProjYears = 2021:2030,
#'   nSim = 5
#' )
#' }
#'
#' @export
GenRecDevs <- function(SD = 0.2, 
                       AC = 0,
                       TruncSD = 3,
                       Ages = NULL,
                       HistYears = NULL, 
                       ProjYears = NULL,
                       nSim = 48,
                       RecDevInit = NULL,
                       RecDevHist = NULL,
                       RecDevProj = NULL) {
  
  if (is.null(HistYears)) cli::cli_abort('`HistYears` cannot be NULL')
  if (is.null(ProjYears)) cli::cli_abort('`ProjYears` cannot be NULL')
  
  nInitRecDev <- length(Ages@Classes) - 1
  nHistTS <- length(HistYears)
  nProjTS <- length(ProjYears)
  
  genInit <- genHist <- genProj <- TRUE
  
  if (!is.null(RecDevInit) && all(!is.na(RecDevInit))) {
    if (!is.array(RecDevInit)) {
      RecDevInit <- array(RecDevInit, dim = c(1, nInitRecDev))  
    }
    logRecDevInit <- log(RecDevInit)
    genInit <- FALSE
  }
  if (!is.null(RecDevHist) && all(!is.na(RecDevHist))) {
    if (!is.array(RecDevHist)) {
      RecDevHist <- array(RecDevHist, dim = c(1, nHistTS))  
    }
    
    logRecDevHist <- log(RecDevHist)
    genHist <- FALSE
  }
  if (!is.null(RecDevProj) && all(!is.na(RecDevProj))) {
    if (!is.array(RecDevProj)) {
      RecDevProj <- array(RecDevProj, dim = c(1, nProjTS))
    }
    logRecDevProj <- log(RecDevProj)
    genProj <- FALSE
  }
  
  
  if (!genInit && !genHist && !genProj) {
    dimnames(RecDevInit) <- list(Sim = 1:nrow(RecDevInit),
                                 Age = Ages@Classes[-1])
    dimnames(RecDevHist) <- list(Sim = 1:nrow(RecDevHist),
                                 Year = HistYears)
    dimnames(RecDevProj) <- list(Sim = 1:nrow(RecDevProj),
                                 Year = ProjYears)
    return(list(RecDevInit = RecDevInit,
                RecDevHist = RecDevHist,
                RecDevProj = RecDevProj))
  }
  
  SD <- rep(SD, nSim)[1:nSim]
  AC <- rep(AC, nSim)[1:nSim]
  AC[!is.finite(AC)] <- 0

  if (any(abs(AC) >= 1))
    cli::cli_abort("{.arg AC} must be within {.val {c(-1, 1)}} exclusive: an AR(1) process with {.code abs(AC) >= 1} has no stationary distribution.")

  if (!is.numeric(TruncSD) || length(TruncSD) != 1 || TruncSD <= 0)
    cli::cli_abort("{.arg TruncSD} must be a positive scalar")

  if (genInit)
    logRecDevInit <- array(stats::rnorm(nSim*nInitRecDev),
                           dim = c(nSim, nInitRecDev))
  if (genHist)
    logRecDevHist <- array(stats::rnorm(nSim*nHistTS),
                           dim = c(nSim, nHistTS))
  if (genProj)
    logRecDevProj <- array(stats::rnorm(nSim*nProjTS),
                           dim = c(nSim, nProjTS))

  period <- c(rep('Init', nInitRecDev), rep('Hist', nHistTS),
              rep('Proj', nProjTS))
  required <- c(rep(genInit, nInitRecDev), rep(genHist, nHistTS),
                rep(genProj, nProjTS))
  YearsSeq <- which(required)
  firstHistPos   <- nInitRecDev + 1
  firstProjPos   <- nInitRecDev + nHistTS + 1
  nearestInitPos <- if (nInitRecDev > 0) 1L else NA_integer_

  for (i in seq_len(nSim)) {
    init_sim <- min(nrow(logRecDevInit), i)
    hist_sim <- min(nrow(logRecDevHist), i)
    proj_sim <- min(nrow(logRecDevProj), i)

    logRecDevs <- c(logRecDevInit[init_sim, ],
                    logRecDevHist[hist_sim, ],
                    logRecDevProj[proj_sim, ])

    for (t in seq_along(YearsSeq)) {
      pos <- YearsSeq[t]

      if (pos == firstHistPos && !genInit && !is.na(nearestInitPos)) {
        prevVal <- .DevToLatent(logRecDevs[nearestInitPos], SD[i], TruncSD)
        logRecDevs[pos] <- AC[i] * prevVal + logRecDevs[pos] * sqrt(1 - AC[i]^2)
        next
      }

      if (pos == firstProjPos && !genHist) {
        prevVal <- .DevToLatent(logRecDevs[pos - 1], SD[i], TruncSD)
        logRecDevs[pos] <- AC[i] * prevVal + logRecDevs[pos] * sqrt(1 - AC[i]^2)
        next
      }

      if (t == 1) next
      logRecDevs[pos] <- AC[i] * logRecDevs[YearsSeq[t - 1]] +
        logRecDevs[pos] * sqrt(1 - AC[i]^2)
    }

    logRecDevs[YearsSeq] <- .LatentToDev(logRecDevs[YearsSeq], SD[i], TruncSD)

    if (genInit) logRecDevInit[init_sim, ] <- logRecDevs[period == 'Init']
    if (genHist) logRecDevHist[hist_sim, ] <- logRecDevs[period == 'Hist']
    if (genProj) logRecDevProj[proj_sim, ] <- logRecDevs[period == 'Proj']
  }
  
  RecDevInit <- exp(logRecDevInit)
  RecDevHist <- exp(logRecDevHist)
  RecDevProj <- exp(logRecDevProj)
  
  dimnames(RecDevInit) <- list(Sim = 1:nrow(RecDevInit),
                               Age = Ages@Classes[-1])
  dimnames(RecDevHist) <- list(Sim = 1:nrow(RecDevHist),
                               Year = HistYears)
  dimnames(RecDevProj) <- list(Sim = 1:nrow(RecDevProj),
                               Year = ProjYears)
  
  list(RecDevInit = RecDevInit,
       RecDevHist = RecDevHist,
       RecDevProj = RecDevProj)
}
