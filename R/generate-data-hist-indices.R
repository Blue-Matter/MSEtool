#' Generate Historical Index Data
#'
#' Internal function to generate simulated historical index data
#' (`CPUE` or `Survey`) when real observations are not available.
#'
#' Uses conditioned observation objects to apply selectivity,
#' efficiency, and observation error to simulated population
#' numbers or biomass, aggregated over age, area, and stocks.
#'
#' Supports indices in number, biomass, or recruitment units,
#' applies fleet-specific selectivity-at-age, and standardizes
#' generated indices to mean 1.
#'
#' If no observation structure is defined at all, the existing data are
#' returned unchanged. Otherwise, generation happens per fleet.
#'
#' @param sim Integer index of the simulation replicate to extract
#' @param Data Fishery data object to populate
#' @param Hist Operating model history object
#' @param HistYears Numeric vector of historical years
#' @param i Integer index of observed data set
#' @param stocks Integer vector of stock indices in the complex
#' @param StockNames Character vector of stock names
#' @param nArea Integer number of spatial areas
#' @param defaultCV Numeric default coefficient of variation applied
#'   when no fleet-specific CV is provided
#' @param type Character, either `CPUE` or `Survey`
#'
#' @return An object of class `indicesdata` containing simulated
#'   historical index values and CVs
#'
#' @keywords internal
.GenHistDataIndices <- function(sim, Data, Hist, HistYears, i, stocks, StockNames, 
                                nArea,
                                defaultCV = 0.2,
                                type = c('CPUE', 'Survey')) {
  
  type <- match.arg(type, c('CPUE', 'Survey'))

  RealData <- slot(Data, type)

  AllObs <- lapply(Hist@OM@Obs[[i]], slot, type)
  no_obs <- all(lapply(AllObs, isNewObject) |> unlist()  == TRUE)

  # no Obs specified, don't simulate
  if (no_obs)
    return(RealData)

  # check which Obs objects have Obs@Error for `type`
  TypeFleets <- purrr::map(AllObs, \(obs) !is.null(obs@Error)) |> unlist() |> which()
  FleetNames <- names(AllObs)[TypeFleets]
  nFleet <- length(FleetNames)

  if (!nFleet)
    return(RealData)

  RealFleetIdx  <- if (!is.null(RealData@Value)) match(FleetNames, RealData@Name) else rep(NA_integer_, nFleet)
  HasRealColumn <- !is.na(RealFleetIdx)

  if (all(HasRealColumn))
    return(RealData)

  nTS <- length(HistYears)

  IndexData <- new('indicesdata')
  IndexData@Name <- FleetNames
  Value <- CV <- array(NA, dim=c(nTS, nFleet),
                           dimnames=list(Year=HistYears,
                                         Fleet=FleetNames))

  CV[] <- defaultCV
  IndexData@Units <- rep('Biomass', nFleet)
  
  IndexData@Ref <- rep(NA_real_, nFleet) 
  
  Real_Pop_Number <- Hist@Number[stocks]
  
  IndexData@Misc$IndexObs <- list()
  
  for (fl in 1:nFleet) {

    if (HasRealColumn[fl]) {
      ri <- RealFleetIdx[fl]
      yearIdx <- match(as.character(HistYears), rownames(RealData@Value))
      Value[, fl] <- RealData@Value[yearIdx, ri]
      if (!is.null(RealData@CV)) CV[, fl] <- RealData@CV[yearIdx, ri]
      if (length(RealData@Units) >= ri) IndexData@Units[fl] <- RealData@Units[ri]
      if (length(RealData@Ref) >= ri) IndexData@Ref[fl] <- RealData@Ref[ri]
      IndexData@Misc$IndexObs[[fl]] <- slot(Hist@OM@Obs[[i]][[FleetNames[fl]]], type)
      next()
    }

    IndexObs <- slot(Hist@OM@Obs[[i]][[FleetNames[fl]]],type)

    if (EmptyObject(IndexObs)) 
      next()
    
    if (!is.null(IndexObs@Units))
      IndexData@Units[fl] <- IndexObs@Units
    
    Units <- IndexData@Units[fl]
    
    if (is.null(IndexObs@Areas))
      IndexObs@Areas <- 1:nArea

    timing <- if (length(IndexData@Timing) >= fl) IndexData@Timing[fl] else NA_real_

    real_nom_index <- .CalcNomIndex(
      Number_List      = Real_Pop_Number,
      object           = Hist,
      stocks           = stocks,
      fleet            = FleetNames[fl],
      IndexObs         = IndexObs,
      Years            = HistYears,
      SelectivityAtAge = IndexObs@Selectivity,
      sim              = sim,
      timing           = timing,
      Units            = Units
    )
    
    Beta <- if (is.null(IndexObs@Beta)) 1 else IndexObs@Beta[min(sim, length(IndexObs@Beta))]
    BetaIndex <- real_nom_index^Beta

    # add error
    SimulatedIndexError <- BetaIndex *  .ArraySubsetYear(IndexObs@Error, HistYears)[sim,]
    # mean 1
    StIndex <- SimulatedIndexError/mean(SimulatedIndexError, na.rm=TRUE)
    Value[,fl] <- StIndex
    NonNAInd <- which(!is.na(StIndex))
    IndexObs@Efficiency <- mean(StIndex, na.rm=TRUE)/mean(BetaIndex[NonNAInd], na.rm=TRUE)
    IndexData@Misc$IndexObs[[fl]] <- IndexObs
    
    # Reference Value 
    if (length(IndexObs@Ref)) {
      # TODO - index ref value if units != Biomass - only does BMSY at the moment
      if (length(Hist@Reference@MSY@BMSY)) {
        adjust <- mean(real_nom_index/apply(Hist@Biomass[sim,i,,drop=FALSE], 'Year', mean, na.rm=TRUE), na.rm=TRUE)
        IndexData@Ref[fl] <- array(mean(Hist@RefPointsMSY@BMSY[sim,i,], na.rm=TRUE) *  adjust *IndexObs@Efficiency) * IndexObs@Ref[sim]
      } else {
        # do B0
        Ref_Dep <- 0.5 # hard coded for now 
        B0 <- Hist@Unfished@Equilibrium@Biomass[sim,i,,drop=FALSE]
        Bhist <- Hist@Biomass[sim,i,,drop=FALSE]
        B_B0 <- ArrayDivide(Bhist, B0) |> apply('Year', mean, na.rm=TRUE)
        ind <- which.min(abs(B_B0-Ref_Dep))
        if (!is.null(ind) && length(ind)) {
          adjust <- mean(real_nom_index/apply(Hist@Biomass[sim,i,,drop=FALSE], 'Year', mean, na.rm=TRUE), na.rm=TRUE)
          IndexData@Ref[fl] <- array(mean(Hist@Biomass[sim,i, ind], na.rm=TRUE) *  adjust *IndexObs@Efficiency) * IndexObs@Ref[sim]   
        }
        
        
      }
      
    }
  }
  
  names(IndexData@Misc$IndexObs) <- FleetNames
  
  IndexData@Value <- Value
  IndexData@CV <- CV
  
  IndexData
} 
