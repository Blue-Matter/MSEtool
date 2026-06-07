#' Condition Composition Observation Error from Observed Data
#'
#' Internal function to condition [compobs-class] observation error parameters
#' from observed age or size composition data. Populates `Shift` (per-bin
#' log-concentration residual), `ESS` (effective sample size), and
#' `SampleSize` for each fleet where observed composition data are available.
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
#' @return `Hist` with `SampleSize`, `Shift`, and `ESS` populated in the
#'   relevant [compobs-class] slots of `Hist@OM@Obs[[i]]` for each fleet
#'   where observed composition data are available.
#'
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
    
    CompObs@SampleSize <- ObsTotals
    
    # Observed proportions 
    ObsProp     <- ObsCounts / ObsTotals
    ObsProp[ObsTotals == 0 | is.na(ObsTotals), ] <- NA
    ObsPropSafe <- pmax(ObsProp, 1e-8)
    
    ObsPropSafe <- AddDimension(ObsPropSafe, 'Sim', pos = 1) |>
      ExtendSims(nSim = nSim) |> aperm(c(1,3,2))
      
    # OM-predicted catch for this fleet over conditioning years
    yr_ind_pred  <- match(as.character(CondYears), dimnames(PredCatch)[[3]])
    PredCatch_fl <- abind::adrop(PredCatch[, , yr_ind_pred, fl, drop = FALSE], 4)
    
    # Predicted proportions [nSim x nBin x nCondYear]
    PredTotals   <- apply(PredCatch_fl, c(1,3), sum)
    PredTotals[PredTotals <= 0 | !is.finite(PredTotals)] <- NA
    PredProp     <- sweep(PredCatch_fl, c(1,3), PredTotals, '/')
    PredPropSafe <- pmax(PredProp, 1e-8)
  
    # Shift: log(obs_prop[t,b]) - log(pred_prop[s,t,b])
    # Conditioning years: computed per year.
    # Projection years: last conditioning year held constant.
    logObsExp <- log(ObsPropSafe)
    logObsExp <- ExtendSims(logObsExp, nSim = nSim)
    
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

    # ESS
    ObsPropExp <- AddDimension(ObsProp, 'Sim', pos = 1) |>
      ExtendSims(nSim = nSim) |> aperm(c(1, 3, 2))
    
    HN_num <- apply(PredProp * (1 - PredProp), c(1, 3), sum, na.rm = TRUE)
    HN_den <- apply((ObsPropExp - PredProp)^2, c(1, 3), sum, na.rm = TRUE)
    
    NHat <- HN_num / HN_den
    NHat[!is.finite(NHat) | NHat <= 0] <- NA
    
    # Cap ESS at observed sample size 
    SampleSizeMat <- matrix(
      rep(ObsTotals, each = nSim),
      nrow = nSim,
      ncol = nCondYears
    )
    NHat <- pmin(NHat, SampleSizeMat, na.rm = FALSE)
    
    ESSArray <- array(
      NA_real_,
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