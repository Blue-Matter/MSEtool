#' Condition Composition Observation Error from Observed Data
#'
#' Internal function to condition [compobs-class] observation error parameters
#' from observed age or size composition data. Populates `Shift` (per-bin
#' log-concentration residual) and `ESS` (effective sample size) for each
#' fleet where observed composition data are available.
#'
#' @param Hist A [Hist()] object populated with historical fishery dynamics.
#' @param FisheryData A [Data()] object containing real fishery composition
#'   data.
#' @param HistYears Numeric vector of historical years.
#' @param ProjYears Numeric vector of projection years.
#' @param stocks Integer vector of stock indices in the complex.
#' @param i Integer index of the observed data set.
#' @param type Character; one of `"LandingsAtAge"`, `"DiscardsAtAge"`,
#'   `"LandingsAtSize"`, or `"DiscardsAtSize"`.
#'
#' @details
#' ## Early Exit Conditions
#'
#' The function returns `Hist` unchanged if:
#'
#' - The relevant [compdata-class] slot of `FisheryData` is empty or has no
#'   `Value` array.
#' - A fleet has no matching column in the observed data, or its [compobs-class]
#'   object is a default unconditioned object (as determined by `isNewObject()`),
#'   or `SampleSize` is `NULL`.
#'
#' ## OM-Predicted Compositions
#'
#' True catch-at-age or catch-at-size is aggregated over stocks within the
#' complex and over areas:
#'
#' - **`*AtAge`**: extracted from `Hist@LandingsAtAge` or `Hist@DiscardsAtAge`
#'   (list over stocks, each `[nSim x nAge x nYear x nFleet x nArea]`), summed
#'   over areas and stocks.
#' - **`*AtSize`**: extracted from `Hist@LandingsAtSize` or
#'   `Hist@DiscardsAtSize` (nested list `[stock][fleet]`, each
#'   `[nSim x nClass x nYear x nArea]`), summed over areas and stocks,
#'   assembled over fleets.
#'
#' Predicted proportions \eqn{\hat{p}_{s,t,b}} are computed by normalising
#' within each simulation and year.
#'
#' ## Conditioned Parameters
#'
#' ### `Shift` per-bin log-concentration residual
#'
#' For each simulation `s`, fleet, and conditioning year `t`, the
#' log-concentration residual for bin \eqn{b} is:
#'
#' \deqn{r_{s,t,b} = \log(p^{\mathrm{obs}}_{t,b}) - \log(\hat{p}_{s,t,b})}
#'
#' where \eqn{p^{\mathrm{obs}}_{t,b}} is the observed proportion (identical
#' across simulations) and \eqn{\hat{p}_{s,t,b}} is the OM-predicted
#' proportion for simulation `s`. `Shift` is set to \eqn{r_{s,t,b}} directly
#' for each conditioning year, giving a `[nSim x nYear x nBin]` array.
#'
#' For projection years, the shift from the last conditioning year is held
#' constant. **This assumes that the compositional bias structure observed in
#' the final conditioning year persists unchanged into projection years.** If
#' the bias is driven by a time-varying process (e.g. gear change, spatial
#' shift), this assumption may not hold. The `Shift` slot can be overwritten
#' directly after conditioning if a different projection assumption is
#' preferred.
#'
#' ### `ESS` effective sample size
#'
#' `ESS` is estimated per simulation and year using the chi-square effective N:
#'
#' \deqn{\mathrm{ESS}_{s,t} = \frac{1}{\displaystyle\sum_b
#'   \frac{(p^{\mathrm{obs}}_{t,b} - \hat{p}_{s,t,b})^2}{\hat{p}_{s,t,b}}}}
#'
#' The result is stored as a `[nSim x nYear]` array. Years with no valid
#' observed or predicted compositions are set to `NA`. Non-finite estimates
#' (e.g. where all predicted catch is zero) fall back to `1`.
#'
#' For projection years, the ESS from the last conditioning year is held
#' constant. **This carries the same assumption as `Shift`: that the
#' observation error structure of the final conditioning year is representative
#' of future years.** The `ESS` slot can be overwritten directly after
#' conditioning if a different assumption is preferred.
#'
#' ### `Theta` not conditioned
#'
#' `Theta` is not estimated because it is not separately identifiable from
#' `ESS` given only observed composition proportions — both parameters control
#' overdispersion relative to the multinomial. **Conditioning implicitly
#' assumes `Theta = 1`, i.e. that all overdispersion is absorbed into `ESS`.**
#' If a different value is preferred, it can be set directly after conditioning:
#'
#' ```r
#' LandingsAtAge(Hist@OM@Obs[[i]][[fl]])@Theta <- 0.5
#' ```
#'
#' @return `Hist` with `Shift` and `ESS` populated in the relevant
#'   [compobs-class] slots of `Hist@OM@Obs[[i]]` for each fleet where
#'   observed composition data are available.
#'
#' @seealso [CompObs()], [compobs-class], [compdata-class],
#'   [ConditionObs_Catch()], [GenHistData_AgeComp()], [GenHistData_SizeComp()]
#' @keywords internal
ConditionObs_Comp <- function(Hist,
                              FisheryData,
                              HistYears,
                              ProjYears,
                              stocks,
                              i,
                              type = c('LandingsAtAge', 'DiscardsAtAge',
                                       'LandingsAtSize', 'DiscardsAtSize')) {
  
  type       <- match.arg(type)
  isAtAge    <- type %in% c('LandingsAtAge', 'DiscardsAtAge')
  nSim       <- nSim(Hist)
  nHistTS    <- length(HistYears)
  nProjTS    <- length(ProjYears)
  YearsAll   <- c(HistYears, ProjYears)
  nYearsAll  <- length(YearsAll)
  fleetnames <- FleetNames(Hist)
  nFleet     <- length(fleetnames)
  
  # Observed composition data: [nYear x nFleet x nBin]
  ObsCompData <- slot(FisheryData, type)
  
  if (EmptyObject(ObsCompData))
    return(Hist)
  
  ObsValue <- ObsCompData@Value
  
  # Aggregate OM-predicted catch over stocks and areas
  if (isAtAge) {
    PredCatch <- purrr::map(slot(Hist, type)[stocks], \(catch_n) {
      catch_n |>
        SubsetYear(HistYears) |>
        SumOverArea()
      
    }) |> List2Array('Stock', pos = 2) |>
      SumOverStock()      
    
    BinNames  <- dimnames(PredCatch)[['Age']]
    
  } else {
    # Nested [stock][fleet]: [nSim x nClass x nYear x nArea]
    PredCatch <- purrr::map(slot(Hist, type)[stocks], \(stock_list) {
      purrr::map(stock_list, \(catch_n) {
        catch_n |>
          SubsetYear(HistYears) |>
          SumOverArea()                        
      }) |> List2Array('Fleet', pos = 4)               
    }) |> List2Array('Stock', pos = 2) |>
      SumOverStock()                                    

    BinNames  <- dimnames(PredCatch)[[2]]
  }
  
  nBin <- length(BinNames)
  

  # Fleet loop
  for (fl in seq_len(nFleet)) {
    fleet_name <- fleetnames[fl]
    
    fl_obs_ind <- match(fleet_name, dimnames(ObsValue)[[2]])
    if (is.na(fl_obs_ind)) next
    
    CompObs <- slot(Hist@OM@Obs[[i]][[fleet_name]], type)
    if (isNewObject(CompObs) || is.null(CompObs@SampleSize)) next
    
    # Years to condition on
    CondYears  <- if (!is.null(CompObs@Years)) CompObs@Years else HistYears
    CondYears  <- intersect(CondYears, HistYears)
    if (length(CondYears) == 0) next
    nCondYears <- length(CondYears)
    
    # Observed counts for this fleet
    yr_ind_obs <- match(as.character(CondYears), dimnames(ObsValue)[[1]])
    ObsCounts  <- drop(ObsValue[yr_ind_obs, fl_obs_ind, , drop = FALSE])
    if (is.null(dim(ObsCounts)))
      ObsCounts <- matrix(ObsCounts, nrow = 1,
                          dimnames = list(Year = CondYears, Bin = BinNames))
    
    ObsTotals <- rowSums(ObsCounts, na.rm = TRUE)
    valid_yrs <- which(ObsTotals > 0 & !is.na(ObsTotals))
    if (length(valid_yrs) == 0) next
    
    # Observed proportions 
    ObsProp     <- ObsCounts / ObsTotals
    ObsProp[ObsTotals == 0 | is.na(ObsTotals), ] <- NA
    ObsPropSafe <- pmax(ObsProp, 1e-8)
    
    ObsPropSafe <- AddDimension(ObsPropSafe, 'Sim', pos = 1) |>
      ExtendSims(nSim = nSim) |> aperm(c(1,3,2))
      
    # OM-predicted catch for this fleet over conditioning years
    yr_ind_pred  <- match(as.character(CondYears), dimnames(PredCatch)[[3]])
    PredCatch_fl <- drop(PredCatch[, , yr_ind_pred, fl, drop = FALSE])
    if (length(dim(PredCatch_fl)) == 2)
      PredCatch_fl <- array(PredCatch_fl, dim = c(dim(PredCatch_fl), 1),
                            dimnames = c(dimnames(PredCatch_fl), list(Year = CondYears)))
    
    # Predicted proportions [nSim x nBin x nCondYear]
    PredTotals   <- apply(PredCatch_fl, c(1, 3), sum)
    PredTotals[PredTotals <= 0 | !is.finite(PredTotals)] <- NA
    PredProp     <- sweep(PredCatch_fl, c(1, 3), PredTotals, '/')
    PredPropSafe <- pmax(PredProp, 1e-8)
    

    # Shift: log(obs_prop[t,b]) - log(pred_prop[s,t,b])
    # Conditioning years: computed per year.
    # Projection years: last conditioning year held constant.
    logObsExp <- log(ObsPropSafe)
    
    # Residuals 
    Resid <- logObsExp - log(PredPropSafe)
    
    # NA years with no valid obs or pred
    for (yr in seq_len(nCondYears)) {
      if (!yr %in% valid_yrs) Resid[, , yr] <- NA
      Resid[is.na(PredTotals[, yr]), , yr]  <- NA
    }
    
    # Initialise Shift array over all years [nSim x nYearsAll x nBin]
    ShiftArray <- array(
      0,
      dim      = c(nSim, nYearsAll, nBin),
      dimnames = list(Sim  = seq_len(nSim),
                      Year = YearsAll,
                      Age  = BinNames)
    )
    
    if (!isAtAge)
      names(dimnames(ShiftArray))[3] <- 'Class'
      
    # Fill conditioning years:
    yr_ind_all <- match(as.character(CondYears), as.character(YearsAll))
    ShiftArray[, yr_ind_all, ] <- aperm(Resid, c(1, 3, 2))
    
    # Projection years and any hist years beyond conditioning: hold last cond year
    last_shift <- ShiftArray[, yr_ind_all[nCondYears], ]   
    fill_ind   <- setdiff(seq_len(nYearsAll), yr_ind_all)
    if (length(fill_ind) > 0)
      ShiftArray[, fill_ind, ] <- array(
        rep(last_shift, length(fill_ind)),
        dim = c(nSim, length(fill_ind), nBin)
      )
    
    CompObs@Shift <- ShiftArray

    # ESS: harmonic mean over conditioning years of chi-square effective N

    ObsPropExp <- AddDimension(ObsProp, 'Sim', pos = 1) |>
      ExtendSims(nSim = nSim) |> aperm(c(1,3,2))
    
    # Chi-square term per bin [nSim x nBin x nCondYear], sum over bins
    ChiSqSum <- apply((ObsPropExp - PredProp)^2 / PredPropSafe, c(1, 3), 
                      sum, na.rm = TRUE)
    
    NHat <- 1 / ChiSqSum
    NHat[!is.finite(NHat) | NHat <= 0] <- NA
    
    ESSArray <- array(
      NA,
      dim      = c(nSim, nYearsAll),
      dimnames = list(Sim  = seq_len(nSim),
                      Year = YearsAll)
    )
    
    ESSArray[, yr_ind_all] <- NHat
    
    # Hold last conditioning year constant for remaining years
    last_ess <- ESSArray[, yr_ind_all[nCondYears]]
    if (length(fill_ind) > 0)
      ESSArray[, fill_ind] <- matrix(
        rep(last_ess, length(fill_ind)),
        nrow = nSim,
        ncol = length(fill_ind)
      )
    
    CompObs@ESS <- ESSArray
    
    slot(Hist@OM@Obs[[i]][[fleet_name]], type) <- CompObs
  }
  
  Hist
}