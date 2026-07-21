#' Generate Stock-Recruitment Deviations
#'
#' Generate recruitment deviations for initial ages, historical years, and 
#' projected years, with optional auto-correlation and truncation.
#'
#' @param SD Numeric vector of standard deviations for recruitment deviations.
#' @param AC Numeric vector of autocorrelation coefficients (0 = no autocorr).
#' @param TruncSD Numeric. Number of SDs for truncation of deviations.
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
#' historical years, and projected years. Steps include:
#'
#' * Generating deviations from a truncated normal distribution
#'   with standard deviation `SD` and truncation at `TruncSD` standard 
#'   deviations (default 2).
#' * Applying autocorrelation using `AC` for each simulation replicate.
#' * Returning arrays of recruitment deviations for initial ages,
#'   historical years, and projected years.
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
#'   TruncSD = 2,
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
                       TruncSD = 2,
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
  
  mu <- -0.5 * SD^2 * (1 - AC)/sqrt(1 - AC^2)
  lower <- mu - TruncSD * SD
  upper <- mu + TruncSD * SD
  
  if (genInit)
    logRecDevInit <- array(.Rtnorm(nSim*nInitRecDev, mu, SD, lower, upper),
                           dim = c(nSim, nInitRecDev))
  if (genHist)
    logRecDevHist <- array(.Rtnorm(nSim*nHistTS, mu, SD, lower, upper),
                           dim = c(nSim, nHistTS))
  if (genProj)
    logRecDevProj <- array(.Rtnorm(nSim*nProjTS, mu, SD, lower, upper),
                           dim = c(nSim, nProjTS))
  
  period <- c(rep('Init', nInitRecDev), rep('Hist', nHistTS),
              rep('Proj', nProjTS))
  required <- c(rep(genInit, nInitRecDev), rep(genHist, nHistTS),
                rep(genProj, nProjTS))
  YearsSeq <- which(required)
  
  for (i in seq_len(nSim)) {
    init_sim <- min(nrow(logRecDevInit), i)
    hist_sim <- min(nrow(logRecDevHist), i)
    proj_sim <- min(nrow(logRecDevProj), i)
    
    logRecDevs <- c(logRecDevInit[init_sim, ], 
                    logRecDevHist[hist_sim, ], 
                    logRecDevProj[proj_sim, ])
    for (t in seq_along(YearsSeq)[-1]) {
      logRecDevs[YearsSeq[t]] <- AC[i] * logRecDevs[YearsSeq[t-1]] +
        logRecDevs[YearsSeq[t]] * sqrt(1 - AC[i]^2)
    }
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
