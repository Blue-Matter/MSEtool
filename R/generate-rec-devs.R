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
#' arrays, their non-`NA` entries will not be overwritten and are returned
#' as-is. Individual `NA` entries (e.g. recent historical years not yet
#' supported by data in an imported assessment) are generated in their place,
#' chained via the same AR(1)/truncation process off the nearest known
#' deviation. 
#'
#' @return A list with three elements:
#' * `RecDevInit`: Array of recruitment deviations for initial ages.
#' * `RecDevHist`: Array of recruitment deviations for historical years.
#' * `RecDevProj`: Array of recruitment deviations for projection years.
#'
#' @examples
#' recdevs <- GenRecDevs(
#'   SD = 0.2,
#'   AC = 0.3,
#'   TruncSD = 3,
#'   Ages = Ages(10),
#'   HistYears = 2000:2020,
#'   ProjYears = 2021:2030,
#'   nSim = 5
#' )
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

  .Normalize <- function(x, n, label) {
    if (is.null(x)) return(NULL)
    if (!is.array(x)) {
      if (length(x) != n)
        cli::cli_abort(c(
          "x" = "{.arg {label}} has length {.val {length(x)}} but {.val {n}} is expected.",
          "i" = "{.arg {label}} must have one value per age/year class."
        ))
      x <- array(x, dim = c(1, n))
    }
    x
  }
  RecDevInit <- .Normalize(RecDevInit, nInitRecDev, "RecDevInit")
  RecDevHist <- .Normalize(RecDevHist, nHistTS, "RecDevHist")
  RecDevProj <- .Normalize(RecDevProj, nProjTS, "RecDevProj")

  # A column needs generating if it wasn't supplied at all, or is NA in the
  # supplied array (any Sim row, since a single supplied row recycled across
  # `nSim` simulations means every simulation sees that NA).
  .ColRequired <- function(x, n) {
    if (is.null(x)) return(rep(TRUE, n))
    apply(is.na(x), 2, any)
  }
  initReq <- .ColRequired(RecDevInit, nInitRecDev)
  histReq <- .ColRequired(RecDevHist, nHistTS)
  projReq <- .ColRequired(RecDevProj, nProjTS)

  genInit <- is.null(RecDevInit) || any(initReq)
  genHist <- is.null(RecDevHist) || any(histReq)
  genProj <- is.null(RecDevProj) || any(projReq)

  if (!is.null(RecDevInit)) logRecDevInit <- log(RecDevInit)
  if (!is.null(RecDevHist)) logRecDevHist <- log(RecDevHist)
  if (!is.null(RecDevProj)) logRecDevProj <- log(RecDevProj)

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

  .SeedBlock <- function(x, req, n) {
    mat <- array(stats::rnorm(nSim * n), dim = c(nSim, n))
    known <- which(!req)
    if (length(known)) {
      nrowX <- nrow(x)
      for (i in seq_len(nSim)) mat[i, known] <- log(x[min(nrowX, i), known])
    }
    mat
  }
  if (genInit) logRecDevInit <- .SeedBlock(RecDevInit, initReq, nInitRecDev)
  if (genHist) logRecDevHist <- .SeedBlock(RecDevHist, histReq, nHistTS)
  if (genProj) logRecDevProj <- .SeedBlock(RecDevProj, projReq, nProjTS)

  period <- c(rep('Init', nInitRecDev), rep('Hist', nHistTS),
              rep('Proj', nProjTS))
  required <- c(initReq, histReq, projReq)
  YearsSeq <- which(required)
  firstHistPos   <- nInitRecDev + 1
  
  # RecDevInit is stored in ascending-age order (see calc-initial-timestep.R):
  # column 1 is `Ages@Classes[2]`, the age one year before HistYears[1] and so
  # the chronologically nearest Init deviation to Hist. The last column is
  # the oldest age, i.e. the furthest deviation in the past.
  nearestInitPos <- if (nInitRecDev > 0) 1L else NA_integer_
  histSeedsFromLastInit <- nInitRecDev > 0 && required[firstHistPos - 1]

  init_sim <- if (genInit) seq_len(nSim) else pmin(nrow(logRecDevInit), seq_len(nSim))
  hist_sim <- if (genHist) seq_len(nSim) else pmin(nrow(logRecDevHist), seq_len(nSim))
  proj_sim <- if (genProj) seq_len(nSim) else pmin(nrow(logRecDevProj), seq_len(nSim))

  for (i in seq_len(nSim)) {
    logRecDevs <- c(logRecDevInit[init_sim[i], ],
                    logRecDevHist[hist_sim[i], ],
                    logRecDevProj[proj_sim[i], ])

    for (t in seq_along(YearsSeq)) {
      pos <- YearsSeq[t]

      if (pos == firstHistPos && nInitRecDev > 0 && !histSeedsFromLastInit) {
        prevVal <- .DevToLatent(logRecDevs[nearestInitPos], SD[i], TruncSD)
        logRecDevs[pos] <- AC[i] * prevVal + logRecDevs[pos] * sqrt(1 - AC[i]^2)
        next
      }

      if (pos == 1) next

      prevPos <- pos - 1
      prevVal <- logRecDevs[prevPos]
      if (!required[prevPos]) prevVal <- .DevToLatent(prevVal, SD[i], TruncSD)
      logRecDevs[pos] <- AC[i] * prevVal + logRecDevs[pos] * sqrt(1 - AC[i]^2)
    }

    logRecDevs[required] <- .LatentToDev(logRecDevs[required], SD[i], TruncSD)

    if (genInit) logRecDevInit[i, ] <- logRecDevs[period == 'Init']
    if (genHist) logRecDevHist[i, ] <- logRecDevs[period == 'Hist']
    if (genProj) logRecDevProj[i, ] <- logRecDevs[period == 'Proj']
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
