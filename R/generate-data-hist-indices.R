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
#' If real index data already exist, or no observation structure is
#' defined, the existing data are returned unchanged.
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
GenHistData_Indices <- function(sim, Data, Hist, HistYears, i, stocks, StockNames, 
                                nArea,
                                defaultCV=0.2,
                                type=c('CPUE', 'Survey')) {
  
  type <- match.arg(type, c('CPUE', 'Survey'))
  
  # don't simulate data if real data exists
  if (!EmptyObject(slot(Data, type))) 
    return(slot(Data, type))
  
  AllObs <- lapply(Hist@OM@Obs[[i]], slot, type)
  no_obs <- all(lapply(AllObs, isNewObject) |> unlist()  == TRUE)
  
  # no Obs specified, don't simulate
  if (no_obs)
    return(slot(Data, type))
  
  # check which Obs objects have Obs@Error for `type`
  TypeFleets <- purrr::map(AllObs, \(obs) !is.null(obs@Error)) |> unlist() |> which()
  FleetNames <- names(AllObs)[TypeFleets]
  
  nTS <- length(HistYears)
  
  nFleet <- length(FleetNames)
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
    IndexObs <- slot(Hist@OM@Obs[[i]][[FleetNames[fl]]],type)
    
    if (EmptyObject(IndexObs)) 
      next()
    
    if (!is.null(IndexObs@Units))
      IndexData@Units[fl] <- IndexObs@Units
    
    Units <- IndexData@Units[fl]
    
    # Get selectivity-at-age for this index 
    SelectivityAtAge <- IndexObs@Selectivity
    SelectivityAtAgeList <- MakeNamedList(StockNames[stocks])
    
    if (is.character(SelectivityAtAge)) {
      if (SelectivityAtAge == 'Biomass') {
        for (st in seq_along(stocks)) {
          AgeClasses <- Hist@OM@Stock[[st]]@Ages@Classes
          
          SelectivityAtAgeList[[st]] <- array(1, c(length(AgeClasses), 1,1),
                                              dimnames = list(
                                                Age = AgeClasses,
                                                Year = HistYears[1],
                                                Area = 1:nArea)
          )
          
        }
      } else if (SelectivityAtAge == 'SBiomass') {
        for (st in seq_along(stocks)) {
          maturity_at_age <-  Hist@OM@Stock[[stocks[st]]]@Maturity@MeanAtAge[sim,,, drop=FALSE] |>
            ArraySubsetYear(HistYears) |>
            abind::adrop(1) |>
            AddDimension('Area') |>
            ExtendAreas(Areas=1:nArea)
          SelectivityAtAgeList[[st]] <- maturity_at_age
        }
        
      } else if (SelectivityAtAge == 'Obs') {
        # SelectivityAtAgeList <- IndexObs@Selectivity
        SelectivityAtAgeList <- purrr::map(IndexObs@Selectivity, \(stock) {
          stock <- ArraySubsetYear(stock, HistYears) 
          stock[sim,,, drop=FALSE] |>
            AddDimension('Area') |>
            DropDimension(c('Sim', 'Year')) |>
            ExtendAreas(Areas=1:nArea)
        })
      }
    } else {
      SelectivityAtAgeList <- purrr::map(Hist@OM@Fleet[stocks], \(fleet_list) {
        dd <- dim(  fleet_list[[FleetNames[fl]]]@Selectivity@MeanAtAge)
        sel_x <- min(dd[1], sim)
        fleet_list[[FleetNames[fl]]]@Selectivity@MeanAtAge[sel_x,,,,drop=FALSE] |>
          ArraySubsetYear(HistYears) |>
          abind::adrop(1)
      }) 
    }
    
    if (!is.null(IndexObs@Units))
      IndexData@Units[fl] <- IndexObs@Units
    
    if (is.null(IndexObs@Areas))
      IndexObs@Areas <- 1:nArea
    
    Real_Pop_Number_Selected <- purrr::map2(Real_Pop_Number, SelectivityAtAgeList, \(num, sel) {
      n <- num[sim,,, IndexObs@Areas,drop=FALSE] |> ArraySubsetYear(HistYears) |> abind::adrop(1)
      s <- sel[,,IndexObs@Areas,drop=FALSE]
      ArrayMultiply(n, sel) |> SumOverArea()
    })
    
    if (Units=='Number') {
      real_nom_index <- purrr::map(Real_Pop_Number_Selected,SumOverAge) |>
        List2Array('Stock') |>
        apply('Year', sum)
      
    } else if (Units == 'Biomass') {
      WeightAtAgeList <- purrr::map(Hist@OM@Stock[stocks], \(stock) {
        wght <- stock@Weight@MeanAtAge
        dd <- dim(wght)
        wght_x <- min(dd[1], sim)
        wght[wght_x,,, drop=FALSE] |>
          ArraySubsetYear(HistYears) |>
        abind::adrop(1)
      })
      real_nom_index <- purrr::map2(Real_Pop_Number_Selected, WeightAtAgeList, ArrayMultiply) |>
        List2Array('Stock') |>
        apply('Year', sum)
      
    } else if (Units == "Recruitment") {
      
      real_nom_index <- purrr::map(Real_Pop_Number_Selected,\(pop_n) {
        ArraySubsetYear(pop_n, HistYears)[1, ,drop=FALSE] |>
          abind::adrop(1)
      }) |>
        List2Array('Stock', 'Year') 
      dimnames(real_nom_index)[['Year']] <- HistYears
      real_nom_index <- apply(real_nom_index, 'Year', sum)
  
    } else {
      cli::cli_abort('Only {.val Biomass}, {.val Number} and {.val Recruitment} currently supported for {.val Units} in  {.val Obs@CPUE} and {.val Obs@Survey}', .internal=TRUE)
    }
    
    
    # add error 
    SimulatedIndexError <- real_nom_index *  ArraySubsetYear(IndexObs@Error, HistYears)[sim,]
    # mean 1
    StIndex <- SimulatedIndexError/mean(SimulatedIndexError, na.rm=TRUE)
    Value[,fl] <- StIndex
    NonNAInd <- which(!is.na(StIndex))
    IndexObs@Efficiency <- mean(StIndex, na.rm=TRUE)/mean(real_nom_index[NonNAInd], na.rm=TRUE)  
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