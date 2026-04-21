
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
#' 
#' @keywords internal
ConditionObs_Index <- function(Hist, 
                               FisheryData, 
                               HistYears, 
                               ProjYears, 
                               stocks,    # stocks in this complex
                               i,         # observe data set number
                               type=c('CPUE', 'Survey')) {
  
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
                                  Name=Indices_Name)
  
  Indices_Hist <- Indices_Value |> ArraySubsetYear(Years=HistYears)
  
  Sim_Number_List <- Hist@Number[stocks]
  
  for (fl in 1:nFleet) {
    ObsObject <- Hist@OM@Obs[[i]][[Indices_Name[fl]]]
    if (is.null(ObsObject))
      cli::cli_abort("No `Obs` object found for {.val {type}} Data: {.val {Indices_Name[fl]}}")
    
    Index_Obs <- slot(ObsObject,type)
    
    SelectivityAtAge_Data <- slot(FisheryData, type)@Selectivity[[fl]]
    
    # List of selectivty-at-age by stock for this fleet
    SelectivityAtAgeList <- MakeNamedList(StockNames(Hist@OM)[stocks])
    
    if (is.character(SelectivityAtAge_Data)) {
      for (st in seq_along(stocks)) {
        AgeClasses <- Hist@OM@Stock[[st]]@Ages@Classes
        
        if (SelectivityAtAge_Data == 'Biomass') {
          # all age classes selected 
          SelectivityAtAgeList[[st]] <- array(1, dim=c(1,length(AgeClasses), 1, nArea),
                                              dimnames = list(Sim=1,
                                                              Age=AgeClasses,
                                                              Year=HistYears[1],
                                                              Area=Areas)) 
        } else if (SelectivityAtAge_Data == 'SBiomass') {
          MaturityAtAge <- Hist@OM@Stock[[st]]@Maturity@MeanAtAge |>
            AddDimension("Area") |> 
            ExtendAreas(Areas) |>
            ReduceDims()
          
          SelectivityAtAgeList[[st]] <- MaturityAtAge
          
        } else if (SelectivityAtAge_Data == 'Obs') {
          # grab selectivity from `Obs`
          if (inherits(Index_Obs@Selectivity, 'array')) {
            SelectivityAtAgeList[[st]] <- Index_Obs@Selectivity |>  
              AddDimension("Area") |> 
              ExtendAreas(Areas) |>
              ReduceDims() 
          } else if  (inherits(Index_Obs@Selectivity, 'list')) {
            SelectivityAtAgeList[[st]] <- Index_Obs@Selectivity[[st]] |>  
              AddDimension("Area") |> 
              ExtendAreas(Areas) |>
              ReduceDims() 
          } 
          
        } 
      }
    } else {
      # Use fleet selectivity directly (CPUE)
      SelectivityAtAgeList <- purrr::map(Hist@OM@Fleet[stocks], \(stock) {
        stock[[Indices_Name[fl]]]@Selectivity@MeanAtAge |>
          ArraySubsetYear(HistYears) |> 
          ReduceDims()
      })
    }
      
    ObservedIndex <- Indices_Value[,fl]
    
    Units <- slot(FisheryData,type)@Units[fl]
    if (is.null(Units)) Units <- 'Biomass'
    Index_Obs@Units <- Units
    
    # multiply N-at-Age by Selectivity-at-Age for each fleet
    # list [stock] - array sim, age, year, area
    SimNumberSelectedList <- purrr::map2(Sim_Number_List, SelectivityAtAgeList, ArrayMultiply)
    
    if (Units=='Number') {
      SimulatedIndex <- SimNumberSelectedList 
      
    } else if (Units=='Biomass') {
      # conver to biomass
      WeightAtAgeList <- purrr::map(Hist@OM@Stock[stocks], \(stock) 
                                    stock@Weight@MeanAtAge |> 
                                      ArraySubsetYear(HistYears) |>
                                      AddDimension('Area') |>
                                      ExtendAreas(Areas)
                                      ) 
      SimulatedIndex <- purrr::map2(SimNumberSelectedList, WeightAtAgeList, ArrayMultiply) 
      
    } else if (Units=='Recruitment') {
      
      if (nSeasons > 1) {
        # match the age & season for observed recruitment
        # first non-zero age class across all years
        #
        # Recruitment should be the first age class, but in some 
        # applications recruitment in the OM is only in a single season
        # but the observed 'recruitment' index comes from a 
        # different season which has 0 the first age class
        
        Year_Names <- names(ObservedIndex[!is.na(ObservedIndex)])
        year_ind <- match(Year_Names, HistYears)
        
        SimulatedIndex <- SimNumberSelectedList |>
          purrr::map(\(stock) {
            ages <- as.numeric(dimnames(stock)[['Age']])    
            index <- array(NA, dim=c(nSim, 1, nHistTS, nArea),
                           dimnames = list(Sim = 1:nSim,
                                           Age = min(ages),
                                           Year = HistYears,
                                           Area = Areas
                                           )
                           )
            
            n <- stock |> SubsetYear(Years=Year_Names)
            
            first_non_zero_age <- apply(n, 1, function(x) {
              dim(x) <- c(dim(x)[1], prod(dim(x)[-1]))
              which(rowSums(x > 0) > 0)[1]
            })
            for (i in 1:nSim) {
              index[i,,year_ind,] <- n[i, first_non_zero_age[i],,, drop=FALSE]
            }
            index
          })  
        
        
      } else {
        # use first age class as recruitment
        SimulatedIndex <- SimNumberSelectedList |>
          purrr::map(\(stock) {
            ages <- as.numeric(dimnames(stock)[['Age']])
            stock |> ArraySubsetAge(min(ages))
          })  
        
      }
    } else {
      cli::cli_abort('Only {.val Biomass}, {.val Number} and {.val Recruitment} currently supported for {.val Units} in  {.val Data@CPUE} and {.val Data@Survey}', .internal=TRUE)
    }
    
    # Areas 
    if (is.null(Index_Obs@Areas)) {
      Index_Obs@Areas <- Areas
    } else {
      if (length(Index_Obs@Areas)>nArea) 
        cli::cli_abort("Index_Obs@Areas outside bounds", .internal=TRUE)
      
      if (any(!Index_Obs@Areas) %in% Areas)
        cli::cli_abort("Index_Obs@Areas outside bounds", .internal=TRUE)
    }
    
    Areas <- Index_Obs@Areas
    
    Nom_Index <- purrr::map(SimulatedIndex, \(stock) {
      index <- stock[,,,Areas, drop=FALSE] |>
        SumOverArea() |>
        SumOverAge()
    }) |>
      List2Array('Stock') |>
      apply(c('Sim', 'Year'), sum)
    
    NonNAInd <- which(!is.na(ObservedIndex))
    
    meanIndex <- Nom_Index[,NonNAInd, drop=FALSE] |> rowMeans(na.rm=TRUE)
    
    q <- mean(ObservedIndex[NonNAInd], na.rm=TRUE) / meanIndex
    
    q[!is.finite(q)] <- 0
    
    Index_Obs@Efficiency <- q

    SimulatedIndex <- Nom_Index * q
    
    if (is.null(Index_Obs@Years))
      Index_Obs@Years <- HistYears[NonNAInd]
   
    TSInd <- match(Index_Obs@Years, HistYears)
    
    # TODO  doesn't fit beta parameter for now; always assumes beta = 1
    # also need to account for TSInd when calculating beta
    ResidualsBeta <- CalcIndexResiduals(ObservedIndex, SimulatedIndex, beta=1)  
    
    Index_Obs@Beta <- ResidualsBeta$beta
    LogResiduals <- ResidualsBeta$LogResiduals
    
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
                             LastError = LastResidual(LogResiduals))
    
    
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




#' Calculate log residuals for standardized index
#'
#' Internal function to compute log-scale residuals between observed
#' and simulated indices. Standardizes both series to mean 1 before
#' calculating residuals.
#'
#' @param ObservedIndex Numeric vector of length nYear with observed index values.
#' @param SimulatedIndex Numeric matrix or array of dimensions nSim x nYear with simulated index values.
#' @param beta Numeric scaling parameter (default 1) returned for completeness.
#'
#' @return A list with elements:
#' * `LogResiduals`: matrix nSim x nYear of log residuals (log(Observed) - log(Simulated))
#' * `beta`: numeric, the input beta value.
#' 
#' @keywords internal
CalcIndexResiduals <- function(ObservedIndex, SimulatedIndex, beta = 1) {
  
  if (any(ObservedIndex < 0, na.rm = TRUE)) {
    cli::cli_abort(
      "`ObservedIndex` cannot have negative values. Standardize to positive values with mean 1."
    )
  }
  
  # Standardize observed index
  StObserved <- ObservedIndex / mean(ObservedIndex, na.rm = TRUE)
  not_na <- !is.na(StObserved)
  
  # Standardize simulated index using same year mask
  n_ts <- ncol(SimulatedIndex)
  StObserved <- StObserved[seq_len(n_ts)]
  
  StSimulated <- SimulatedIndex / mean(SimulatedIndex[, not_na[seq_len(n_ts)], drop = FALSE], na.rm = TRUE)
  
  # Compute log residuals
  LogResiduals <- -sweep(log(StSimulated), 2, log(StObserved), FUN = "-")
  
  list(
    LogResiduals = LogResiduals,
    beta = beta
  )
}



















