
#' Condition Observed Index for a Stock or Stock Complex
#'
#' Internal function to condition observation error from provided indices (CPUE or Survey)
#' including residuals and autocorrelated errors.
#'
#' @param Hist A [Hist()] object populated with historical fishery dynamics.
#' @param FisheryData A [Data()] object with real fishery data
#' @param HistYears Numeric vector of historical years
#' @param ProjYears Numeric vector of projection years
#' @param stocks Integer vector of stock indices in the complex
#' @param i Integer index of observed data set
#' @param type Character, either `CPUE` or `Survey`
#' @param EstimateBeta Logical. Estimate the hyperstability/hyperdepletion
#'   parameter `Beta` by regression? If `FALSE`, `Beta` is fixed at `1` unless
#'   the user has already supplied a value on the `Obs` object. Default `TRUE`
#'   for this internal function; `.ConditionObs()` normally passes through
#'   `SimControl()`'s own default of `FALSE`.
#'
#' @keywords internal
.ConditionObsIndex <- function(Hist,
                               FisheryData,
                               HistYears,
                               ProjYears,
                               stocks,    # stocks in this complex
                               i,         # observe data set number
                               type=c('CPUE', 'Survey'),
                               EstimateBeta=TRUE) {
  
  type <- match.arg(type, c('CPUE', 'Survey'))
  
  nHistTS <- length(HistYears)
  nProjTS <- length(ProjYears)
  nArea <- nArea(Hist)
  Areas <- 1:nArea
  nSeasons <- Seasons(Hist)
   
  nSim <- nSim(Hist)
  
  IndicesData <- slot(FisheryData, type)
  
  Indices_Name <- IndicesData@Name
  Indices_Value <- IndicesData@Value
  
  if (is.null(Indices_Value)) return(Hist)
  
  dd <- dim(Indices_Value)
  nFleet <- ncol(Indices_Value)
  
  # checks 
  if (dd[2] != length(Indices_Name)) 
    cli::cli_abort(c("x"= "{.val ncol(OM@Data[[{i}]]@{type}@Value)} is not the same as {.val length(OM@Data[[{i}]]@{type}@Name)}"))
  
  if (dd[1] < nHistTS) 
    cli::cli_abort(c("x"= "{.val nrow(OM@Data[[{i}]]@{type}@Value)} must be at least length {.val {nHistTS}}"))
  
  dimnames(Indices_Value) <- list(Year=c(HistYears, ProjYears)[1:dd[1]],
                                  Fleet=Indices_Name)

  slot(Hist@OM@Data[[i]], type)@Value <- Indices_Value

  Indices_Hist <- Indices_Value |> .ArraySubsetYear(Years=HistYears)
  
  Sim_Number_List <- Hist@Number[stocks]
  
  for (fl in 1:nFleet) {
    
    ObsObject <- Hist@OM@Obs[[i]][[Indices_Name[fl]]]
    if (is.null(ObsObject)) {
      ObsObject <- Obs(Name = Indices_Name[fl])
      Hist@OM@Obs[[i]][[Indices_Name[fl]]] <- ObsObject
    }

    Index_Obs <- slot(ObsObject,type)
    ObservedIndex <- Indices_Value[,fl]
    if (all(is.na(ObservedIndex))) next
    
    BadInd <- which(!is.na(ObservedIndex) & ObservedIndex <= 0)
    if (length(BadInd)) {
      Hist <- .CaptureLog(Hist,
                          string = cli::format_inline(
                            "{.val {type}} index {.val {Indices_Name[fl]}} has non-positive value{?s} in year{?s} {.val {HistYears[BadInd]}}; treating as missing (NA)."
                          ),
                          name = '.ConditionObsIndex',
                          type = 'assumption')
      ObservedIndex[BadInd] <- NA
    }
    
    
    SelectivityAtAge_Data <- slot(FisheryData, type)@Selectivity[[fl]]
    SelectivityAtAge <- if (is.character(SelectivityAtAge_Data)) SelectivityAtAge_Data else NULL

    Units <- slot(FisheryData, type)@Units[fl]
    if (is.null(Units)) Units <- 'Biomass'
    Index_Obs@Units <- Units

    timing <- slot(FisheryData, type)@Timing
    timing <- if (length(timing) >= fl) timing[fl] else NA_real_

    # Areas
    if (is.null(Index_Obs@Areas)) {
      Index_Obs@Areas <- Areas
    } else {
      OutOfBounds <- setdiff(Index_Obs@Areas, Areas)
      if (length(OutOfBounds))
        cli::cli_abort("`Index_Obs@Areas` outside bounds: {.val {OutOfBounds}}",
                       .internal = TRUE)

      if (anyDuplicated(Index_Obs@Areas))
        cli::cli_abort("`Index_Obs@Areas` contains duplicate areas: {.val {unique(Index_Obs@Areas[duplicated(Index_Obs@Areas)])}}",
                       .internal = TRUE)
    }

    recruitment_years <- if (Units == 'Recruitment' && nSeasons > 1) {
      names(ObservedIndex[!is.na(ObservedIndex)])
    } else {
      NULL
    }

    Nom_Index <- .CalcNomIndex(
      Number_List         = Sim_Number_List,
      object              = Hist,
      stocks              = stocks,
      fleet               = Indices_Name[fl],
      IndexObs            = Index_Obs,
      Years               = HistYears,
      SelectivityAtAge    = SelectivityAtAge,
      timing              = timing,
      Units               = Units,
      recruitment_years   = recruitment_years,
      on_missing_obs_sel  = function() {
        Hist <<- .CaptureLog(
          Hist,
          string = cli::format_inline(
            "No `Obs` object found for {.val {type}} Data: {.val {Indices_Name[fl]}} \n Assuming selectivity = 1 for all age classes"
          ),
          name = '.ConditionObs',
          type = 'assumption'
        )
      }
    )
    
    NonNAInd <- which(!is.na(ObservedIndex) & ObservedIndex > 0)

    if (is.null(Index_Obs@Years))
      Index_Obs@Years <- HistYears[NonNAInd]

    TSInd <- match(Index_Obs@Years, HistYears)

    # Observed_t = Efficiency * Nom_Index_t^Beta * exp(residual_t) (Harley et al. 2001).
    # Fit jointly unless Beta is fixed by the user or EstimateBeta = FALSE.
    FixedBeta <- if (EstimateBeta) Index_Obs@Beta else (Index_Obs@Beta %||% 1)
    
    ResidualsBeta <- .CalcIndexResiduals(ObservedIndex, Nom_Index, beta=FixedBeta, FitInd=TSInd)

    Index_Obs@Beta       <- ResidualsBeta$Beta
    Index_Obs@Efficiency <- ResidualsBeta$Efficiency
    LogResiduals <- ResidualsBeta$LogResiduals

    Index_Obs@Misc$BetaFit <- list(
      SE_Beta  = ResidualsBeta$SE_Beta,
      CI_Lower = ResidualsBeta$CI_Lower,
      CI_Upper = ResidualsBeta$CI_Upper,
      R2       = ResidualsBeta$R2,
      PValue   = ResidualsBeta$PValue,
      nPoints  = ResidualsBeta$nPoints,
      Status   = ResidualsBeta$Status
    )

    if (is.null(FixedBeta)) {
      FellBack <- ResidualsBeta$Status != "estimated"
      if (any(FellBack)) {
        StatusTable <- table(ResidualsBeta$Status[FellBack])
        StatusMsg <- paste(names(StatusTable), StatusTable, sep = ": ", collapse = ", ")
        Hist <- .CaptureLog(Hist,
                            string = cli::format_inline(
                              "{.val {type}} index {.val {Indices_Name[fl]}}: {.val Beta} could not be freely estimated for {sum(FellBack)}/{length(FellBack)} simulation{?s} ({StatusMsg}); fixed at 1 (or bounds-clamped) for those."
                            ),
                            name = '.ConditionObsIndex',
                            type = 'assumption')
      }
    }

    Stats <- CalcResidualStats(LogResiduals=LogResiduals[, TSInd, drop=FALSE],
                               nSeasons=nSeasons)
    
    Index_Obs@Stats <- Stats
    
    if (is.null(Index_Obs@TruncSD)) 
      Index_Obs@TruncSD <- 2
    
    
    logProjResids <- GenResiduals(SD = Stats$SD, 
                                  AC = Stats$AC, 
                                  Years = ProjYears, 
                                  TruncSD = Index_Obs@TruncSD,
                                  nSeasons = nSeasons, 
                                  NA_Season = Stats$NA_Season)
 
    logProjResids <- ApplyAC(LogResid = logProjResids, 
                             AC = Stats$AC,
                             LastError = .LastResidual(LogResiduals))
    
    
    ResidualsHistorical <- exp(LogResiduals)
    ResidualsProjection <- exp(logProjResids)
    
    Index_Obs@Error <- abind::abind(ResidualsHistorical, ResidualsProjection,
                                    along=2, use.dnns=TRUE) 
    dimnames(Index_Obs@Error) <- list(Sim = seq_len(nSim),
                                     Year=c(HistYears, ProjYears))
    
    slot(Hist@OM@Obs[[i]][[Indices_Name[fl]]],type) <- Index_Obs
    
  
 } # end fleet loop
  
  Hist
}




#' Calculate log residuals for standardized index, fitting hyperstability (beta)
#'
#' Computes log-scale residuals under the hyperstability model
#' `Observed_t = Efficiency * Simulated_t^Beta * exp(residual_t)` (Harley et
#' al. 2001). `Efficiency` is fit per simulation by log-linear regression;
#' `Beta` estimation (or fixing) is delegated to `.EstimateBeta()`.
#'
#' @param ObservedIndex Numeric vector of length nYear with observed index values.
#' @param SimulatedIndex Numeric matrix or array of dimensions nSim x nYear with simulated index values.
#' @param beta `NULL`, numeric scalar, or length-nSim vector. Passed through
#'   to `.EstimateBeta()`.
#' @param MinPoints Integer. Passed through to `.EstimateBeta()`. Default `8`.
#' @param FitInd Integer vector or `NULL`. Year positions to restrict fitting
#'   to (e.g. `Index_Obs@Years`, matching `CalcResidualStats()`'s year
#'   restriction). `NULL` (default) uses every non-NA, positive year.
#' @param alpha Numeric. Passed through to `.EstimateBeta()`. Default `0.05`.
#'
#' @details
#' `NA` or non-positive years are dropped before fitting. See
#' `.EstimateBeta()` for when `Beta` is fixed at `1` rather than freely
#' estimated.
#'
#' @return A list with elements:
#' * `LogResiduals`: matrix nSim x nYear of log residuals (`log(Observed) - fitted log(Observed)`)
#' * `Beta`: numeric vector length nSim, the fitted or supplied beta values.
#' * `Efficiency`: numeric vector length nSim, the fitted catchability `q`.
#' * `SE_Beta`, `CI_Lower`, `CI_Upper`, `R2`, `PValue`, `nPoints`, `Status`: fit
#'   diagnostics from `.EstimateBeta()`, passed through unchanged.
#'
#' @keywords internal
.CalcIndexResiduals <- function(ObservedIndex, SimulatedIndex, beta = NULL,
                                MinPoints = 8, FitInd = NULL, alpha = 0.05) {

  if (any(ObservedIndex < 0, na.rm = TRUE)) {
    cli::cli_abort(
      "`ObservedIndex` cannot have negative values. Standardize to positive values with mean 1."
    )
  }

  nSim <- nrow(SimulatedIndex)
  n_ts <- ncol(SimulatedIndex)
  ObservedIndex <- ObservedIndex[seq_len(n_ts)]

  ValidInd <- which(!is.na(ObservedIndex) & ObservedIndex > 0)
  if (!is.null(FitInd)) ValidInd <- intersect(ValidInd, FitInd)

  logObs <- log(ObservedIndex[ValidInd])
  logSim <- log(SimulatedIndex[, ValidInd, drop = FALSE])
  logSim[!is.finite(logSim)] <- NA

  BetaFit <- .EstimateBeta(logObs, logSim, beta = beta, MinPoints = MinPoints, alpha = alpha)
  Beta <- BetaFit$Beta

  if (length(ValidInd) == 0) {
    LogResiduals <- matrix(NA_real_, nrow = nSim, ncol = n_ts)
    return(list(LogResiduals = LogResiduals,
                Beta = Beta,
                Efficiency = rep(0, nSim),
                SE_Beta  = BetaFit$SE_Beta,
                CI_Lower = BetaFit$CI_Lower,
                CI_Upper = BetaFit$CI_Upper,
                R2       = BetaFit$R2,
                PValue   = BetaFit$PValue,
                nPoints  = BetaFit$nPoints,
                Status   = BetaFit$Status))
  }


  ybar <- mean(logObs, na.rm = TRUE)
  xbar <- rowMeans(logSim, na.rm = TRUE)

  logQ <- ybar - Beta * xbar
  Efficiency <- exp(logQ)
  Efficiency[!is.finite(Efficiency)] <- 0

  logSimAll <- log(SimulatedIndex)
  fitted <- logQ + Beta * logSimAll
  LogResiduals <- sweep(-fitted, 2, log(ObservedIndex), FUN = "+")

  list(
    LogResiduals = LogResiduals,
    Beta         = Beta,
    Efficiency   = Efficiency,
    SE_Beta      = BetaFit$SE_Beta,
    CI_Lower     = BetaFit$CI_Lower,
    CI_Upper     = BetaFit$CI_Upper,
    R2           = BetaFit$R2,
    PValue       = BetaFit$PValue,
    nPoints      = BetaFit$nPoints,
    Status       = BetaFit$Status
  )
}


#' Estimate (or Fix) the Hyperstability Parameter Beta
#'
#' Fits `Beta` per simulation by OLS regression of `log(Observed)` on
#' `log(Simulated)`, or returns a supplied fixed value.
#'
#' When estimating, `Beta` is fixed at `1` when there isn't enough evidence to
#' distinguish it from `1`, or clamped to `[0.1, 3]` when the fit is
#' significant but extreme. The `Status` element records which of these
#' happened for each simulation, so callers can surface it rather than have
#' `Beta` silently fall back to `1`.
#'
#' @param logObs Numeric vector, length n. `log(Observed)` for the years
#'   being fit (already subset to valid/fitted years).
#' @param logSim Numeric matrix, nSim x n. `log(Simulated)` for the same
#'   years, one row per simulation.
#' @param beta `NULL`, numeric scalar, or length-nSim vector. When `NULL`
#'   (default), `Beta` is estimated as described above. Otherwise the
#'   supplied value(s) are used directly (recycled to nSim) and no fitting
#'   or testing is performed (`Status = "fixed_user"`).
#' @param MinPoints Integer. Minimum usable years required before attempting
#'   a free fit. Default `8`.
#' @param MinLogSD Numeric. Minimum standard deviation of `log(Simulated)`
#'   (per simulation) required before attempting a free fit -- guards against
#'   an unstable slope estimate from a near-flat nominal index. Default `0.1`.
#' @param alpha Numeric. Two-sided significance level for testing whether
#'   the fitted slope differs from `1`. Default `0.05`.
#' @param Bounds Numeric length 2. Lower/upper clamp applied to a significant,
#'   freely estimated `Beta`. Default `c(0.1, 3)`.
#'
#' @return A list with elements, each numeric length nSim unless noted:
#' * `Beta`: the fitted or supplied `Beta`.
#' * `SE_Beta`: standard error of the OLS slope. `NA` unless `Status == "estimated"`.
#' * `CI_Lower`, `CI_Upper`: two-sided `1 - alpha` confidence interval on the
#'   OLS slope (pre-clamping). `NA` unless `Status == "estimated"` or `"fixed_bounds"`.
#' * `R2`: fit R-squared. `NA` unless `Status == "estimated"` or `"fixed_bounds"`.
#' * `PValue`: two-sided p-value for `Beta == 1`. `NA` unless `Status ==
#'   "estimated"` or `"fixed_bounds"`.
#' * `nPoints`: scalar integer, the number of years used in the fit (`n`).
#' * `Status`: character. One of `"estimated"`, `"fixed_user"` (`Beta` was
#'   supplied), `"fixed_insufficient_data"` (`n < MinPoints`),
#'   `"fixed_low_variance"` (nominal index too flat to fit reliably),
#'   `"fixed_not_significant"` (slope not distinguishable from `1` at
#'   `alpha`), or `"fixed_bounds"` (significant slope clamped to `Bounds`).
#'
#' @keywords internal
.EstimateBeta <- function(logObs, logSim, beta = NULL, MinPoints = 8,
                          MinLogSD = 0.1, alpha = 0.05, Bounds = c(0.1, 3)) {

  nSim <- nrow(logSim)
  n    <- length(logObs)

  NAVec <- rep(NA_real_, nSim)

  if (!is.null(beta)) {
    Beta <- rep(beta, length.out = nSim)
    return(list(Beta = Beta, SE_Beta = NAVec, CI_Lower = NAVec, CI_Upper = NAVec,
                R2 = NAVec, PValue = NAVec, nPoints = n,
                Status = rep("fixed_user", nSim)))
  }

  if (n < MinPoints) {
    return(list(Beta = rep(1, nSim), SE_Beta = NAVec, CI_Lower = NAVec, CI_Upper = NAVec,
                R2 = NAVec, PValue = NAVec, nPoints = n,
                Status = rep("fixed_insufficient_data", nSim)))
  }

  ybar <- mean(logObs, na.rm = TRUE)
  xbar <- rowMeans(logSim, na.rm = TRUE)

  xc <- logSim - xbar
  yc <- matrix(logObs - ybar, nrow = nSim, ncol = n, byrow = TRUE)

  Sxy <- rowSums(xc * yc, na.rm = TRUE)
  Sxx <- rowSums(xc^2, na.rm = TRUE)
  Syy <- rowSums(yc^2, na.rm = TRUE)

  Degenerate <- !(Sxx > 1e-8)
  SDx        <- sqrt(Sxx / pmax(n - 1, 1))
  LowVar     <- Degenerate | (SDx < MinLogSD)

  BetaOLS <- ifelse(Degenerate, 1, Sxy / pmax(Sxx, 1e-12))
  R2      <- ifelse(Degenerate | !(Syy > 1e-8), 0, (Sxy^2) / (Sxx * Syy))

  # SE of the slope: SE(beta) = sqrt(s2 / Sxx), s2 = SSE / (n-2)
  resid_c <- yc - BetaOLS * xc
  SSE     <- rowSums(resid_c^2, na.rm = TRUE)
  s2      <- SSE / pmax(n - 2, 1)
  SE_beta <- sqrt(s2 / pmax(Sxx, 1e-12))

  DF    <- max(n - 2, 1)
  CritT <- stats::qt(1 - alpha / 2, df = DF)

  tstat  <- ifelse(SE_beta > 0, (BetaOLS - 1) / SE_beta,
                   ifelse(BetaOLS == 1, 0, Inf))
  PValue <- ifelse(is.finite(tstat), 2 * stats::pt(abs(tstat), df = DF, lower.tail = FALSE), 0)

  CI_Lower <- BetaOLS - CritT * SE_beta
  CI_Upper <- BetaOLS + CritT * SE_beta

  NotSignificant <- !LowVar & is.finite(tstat) & (abs(tstat) < CritT)
  OutOfBounds    <- !LowVar & !NotSignificant & (BetaOLS < Bounds[1] | BetaOLS > Bounds[2])

  Status <- ifelse(LowVar, "fixed_low_variance",
             ifelse(NotSignificant, "fixed_not_significant",
              ifelse(OutOfBounds, "fixed_bounds", "estimated")))

  Beta <- ifelse(Status %in% c("fixed_low_variance", "fixed_not_significant"), 1,
           ifelse(Status == "fixed_bounds", pmin(pmax(BetaOLS, Bounds[1]), Bounds[2]),
            BetaOLS))

  # Diagnostics are only meaningful where a slope was actually fit and used.
  Blank <- LowVar
  SE_beta[Blank]  <- NA_real_
  CI_Lower[Blank] <- NA_real_
  CI_Upper[Blank] <- NA_real_
  PValue[Blank]   <- NA_real_
  R2[Blank]       <- NA_real_

  list(
    Beta     = Beta,
    SE_Beta  = SE_beta,
    CI_Lower = CI_Lower,
    CI_Upper = CI_Upper,
    R2       = R2,
    PValue   = PValue,
    nPoints  = n,
    Status   = Status
  )
}
