




GenProjData_Index <- function(x, 
                              Proj, 
                              DataYear,
                              YearsAll,
                              i,
                              stocks, 
                              type=c('CPUE', 'Survey')) {
  
  # TODO hyperstability Beta not functional yet - ignored
  type <- match.arg(type, c('CPUE', 'Survey'))
  
  IndexData <- slot(Proj@Data[[x]][[i]], type)
  
  if (EmptyObject(IndexData))
    return(IndexData)
  
  nArea <- nArea(Proj)
  
  Value <- IndexData@Value
  CV <- IndexData@CV
  
  nFleet <- ncol(Value)
  NewValue <- array(NA, dim=c(1, nFleet),
                    dimnames = list(Year=DataYear,
                                    Fleet=IndexData@Name))
  NewCV <- NewValue
  
  TSIndex <- match(DataYear, YearsAll)
  FleetIndex <- match(IndexData@Name, names(Proj@OM@Obs[[i]]))
  
  if (length(FleetIndex)!= nFleet)
    cli::cli_abort('Mismatch in number of fleets in `Obs` and  `Data[[x]]@{type}`', .internal=TRUE)
  
  Real_Pop_Number <- purrr::map(Proj@Number[stocks], \(stock_n) { # [stock] sim, age, year, area
    stock_n[x,,TSIndex, 1:nArea, drop=FALSE] |> abind::adrop(c(1,3))
  }) 
  
  for (fl in 1:nFleet) {
    IndexObs <- slot(Proj@OM@Obs[[i]][[FleetIndex[fl]]], type)
    if (length(IndexObs)<1)
      next()
    
    # TODO - make this an option
    # currently doesn't simulate index if last five data points were NAs
    if (all(!is.finite(tail(Value[,fl],5)))) 
      next()
    
    Units <- IndexData@Units[fl]
    
    # Index
    if (!is.null(Proj@OM@Data[[i]]) && nrow(slot(Proj@OM@Data[[i]],type)@Value)>=TSIndex) {
      # real data exists
      NewValue[,fl] <- slot(Proj@OM@Data[[i]],type)@Value[TSIndex,fl]
    } else {
      # simulate data
      if (!is.na(IndexData@Timing[fl]) && IndexData@Timing[fl]!=0)
        cli::cli_alert_warning('`Index@Timing` currently not supported. Calculating from beginning of time step')
      
      
      # Get selectivity-at-age for this index 
      SelectivityAtAge <- IndexObs@Selectivity
      SelectivityAtAgeList <- MakeNamedList(StockNames[stocks])
      
      if (is.character(SelectivityAtAge)) {
        if (SelectivityAtAge == 'Biomass') {
          for (st in seq_along(stocks)) {
            AgeClasses <- Proj@OM@Stock[[st]]@Ages@Classes
            
            SelectivityAtAgeList[[st]] <- array(1, c(length(AgeClasses), 1),
                                                dimnames = list(
                                                  Age = AgeClasses,
                                                  Area = 1:nArea)
            )
            
          }
        } else if (SelectivityAtAge == 'SBiomass') {
          for (st in seq_along(stocks)) {
            maturity_at_age <-  Proj@OM@Stock[[stocks[st]]]@Maturity@MeanAtAge[x,,TSIndex, drop=FALSE] |>
              AddDimension('Area') |>
              DropDimension(c('Sim', 'Year')) |>
              ExtendAreas(Areas=1:nArea)
            SelectivityAtAgeList[[st]] <- maturity_at_age
          }
          
        } else if (SelectivityAtAge == 'Obs') {
          SelectivityAtAgeList <- purrr::map(IndexObs@Selectivity, \(stock) {
            stock[x,,TSIndex, drop=FALSE] |>
              AddDimension('Area') |>
              DropDimension(c('Sim', 'Year')) |>
              ExtendAreas(Areas=1:nArea)
          })
        }
      } else {
        # fleet selectivity 
        SelectivityAtAgeList <- purrr::map(Proj@OM@Fleet[stocks], \(fleet_list) {
          fleet_list[[FleetNames[fl]]]@Selectivity@MeanAtAge[x,,TSIndex,,drop=FALSE] |>
            DropDimension(c('Sim', 'Year'))
          
        }) 
      }
      
      # selected umber-at-age summed over specified areas
      Real_Pop_Number_Selected <- purrr::map2(Real_Pop_Number, SelectivityAtAgeList, \(num, sel) {
        ArrayMultiply(num[, IndexObs@Areas, drop=FALSE], sel[, IndexObs@Areas, drop=FALSE]) |> SumOverArea()
      })
      
      if (Units=='Number') {
        real_nom_index <- purrr::map_dbl(Real_Pop_Number_Selected, sum) |> sum()
        
      } else if (Units == 'Biomass') {
        WeightAtAgeList <- purrr::map(Proj@OM@Stock[stocks], \(stock) {
          stock@Weight@MeanAtAge[x,,TSIndex, drop=FALSE] |>
            DropDimension(c('Sim', 'Year'))
        })
        real_nom_index <- purrr::map2(Real_Pop_Number_Selected, WeightAtAgeList, ArrayMultiply) |>
          List2Array('Stock') |>
          sum()
        
      } else if (Units == "Recruitment") {
        real_nom_index <- purrr::map_dbl(Real_Pop_Number_Selected,\(pop_n) {
          pop_n[1]
        }) |> sum()
      } else {
        cli::cli_abort('Only {.val Biomass}, {.val Number} and {.val Recruitment} currently supported for {.val Units} in  {.val Obs@CPUE} and {.val Obs@Survey}', .internal=TRUE)
      }
      
      Value[,fl] <- real_nom_index *  ArraySubsetYear(IndexObs@Error, DataYear)[x] *
        IndexObs@Efficiency[x]
      
    } # end simulate Value 
    
    # CV 
    if (!is.null(Proj@OM@Data[[i]]) &&  
        !is.null(slot(Proj@OM@Data[[i]],type)@CV) &&
        nrow(slot(Proj@OM@Data[[i]],type)@CV)>=TSIndex) {
      NewCV[,fl] <- slot(Proj@OM@Data[[i]],type)@CV[TSIndex,fl]
    } else {
      if (!is.null(IndexData@CV)) {
        # use the CV from last time step
        previouscv <- IndexData@CV[,fl]
        previouscv <- previouscv[!is.na(previouscv)] |> tail(1) |> as.numeric()
        NewCV[,fl] <- previouscv  
      }
    }
    
    
  } # end fleet loop
  
  IndexData@Value <- abind::abind(Value, NewValue, along=1, use.dnns=TRUE)
  if (!is.null(IndexData@CV)) {
    IndexData@CV <- abind::abind(IndexData@CV, NewCV, along=1, use.dnns=TRUE)  
  }
  
  IndexData
  
}