### These functions aren't currently used 


CalcVulnBiomassByStockFleetArea <- function(Hist, TimeStep) {
  # Currently doesn't account for spatial closures
  
  if (!is.null(Hist@Fleet[[1]][[1]]@Distribution@Closure))
    cli::cli_abort('Spatial closures not done yet', .internal=TRUE)
  
  # TODO
  # account for Fleet@Distribution@Closure 
  
  NatAgeArea <- purrr::map(Hist@Number, ArraySubsetTimeStep, TimeSteps=TimeStep)
  FleetWeightAgeArea <- purrr::map2(Hist@Stock, Hist@Fleet, GetFleetWeightAtAge,
                                    TimeSteps=TimeStep) |>
    purrr::map(AddDimension, name='Area') |>
    purrr::map(aperm, perm=c(1,2,3,5,4))
  
  NatAgeAreaFleet <- purrr::map(NatAgeArea, AddDimension, name='Fleet')
  BatAgeAreaFleet <- purrr::map2(NatAgeAreaFleet, FleetWeightAgeArea, ArrayMultiply)
  
  # selectivity at age by fleet (list by stock)
  selectivity <- purrr::map(Hist@Fleet, GetSelectivityAtAge, TimeSteps=TimeStep) |>
    purrr::map(function(.x) {
      array <- array(unlist(.x), dim=c(dim(.x[[1]]), length(.x)))
      dimnames(array) <- c(dimnames(.x[[1]]), Fleet=list(names(.x)))
      AddDimension(array, 'Area') |>
        aperm(c(1,2,3,5,4))
    })
  
  retention <- purrr::map(Hist@Fleet, GetRetentionAtAge, TimeSteps=TimeStep) 
  if (any(is.null(unlist(retention)))) {
    SelectivityRetention <- selectivity
  } else {
    retention <- purrr::map(retention, function(.x) {
      array <- array(unlist(.x), dim=c(dim(.x[[1]]), length(.x)))
      dimnames(array) <- c(dimnames(.x[[1]]), Fleet=list(names(.x)))
      AddDimension(array, 'Area') |>
        aperm(c(1,2,3,5,4))
    })
    SelectivityRetention <- purrr::map2(selectivity, retention, ArrayMultiply)
  }
  
  purrr::map2(BatAgeAreaFleet, 
              SelectivityRetention, 
              ArrayMultiply)
  
  
}

CalcUtilityByStockFleetArea <- function(Hist, TimeStep) {
  # Calculate the relative utility of each area for each fleet
  # Utility is current defined as total vulnerable biomass by area by fleet
  # later can include value by age/length and cost by area
  # 

  NatAgeArea <- purrr::map(Hist@Number, ArraySubsetTimeStep, TimeSteps=TimeStep)
  FleetWeightAgeArea <- purrr::map2(Hist@Stock, Hist@Fleet, GetFleetWeightAtAge,
                                    TimeSteps=TimeStep) |>
    purrr::map(AddDimension, name='Area') |>
    purrr::map(aperm, perm=c(1,2,3,5,4))
  
  NatAgeAreaFleet <- purrr::map(NatAgeArea, AddDimension, name='Fleet')
  BatAgeAreaFleet <- purrr::map2(NatAgeAreaFleet, FleetWeightAgeArea, ArrayMultiply)
  
  # selectivity at age by fleet (list by stock)
  selectivity <- purrr::map(Hist@Fleet, GetSelectivityAtAge, TimeSteps=TimeStep) |>
    purrr::map(function(.x) {
      array <- array(unlist(.x), dim=c(dim(.x[[1]]), length(.x)))
      dimnames(array) <- c(dimnames(.x[[1]]), Fleet=list(names(.x)))
      AddDimension(array, 'Area') |>
        aperm(c(1,2,3,5,4))
    })
  
  retention <- purrr::map(Hist@Fleet, GetRetentionAtAge, TimeSteps=TimeStep) 
  if (any(is.null(unlist(retention)))) {
    SelectivityRetention <- selectivity
  } else {
    retention <- purrr::map(retention, function(.x) {
      array <- array(unlist(.x), dim=c(dim(.x[[1]]), length(.x)))
      dimnames(array) <- c(dimnames(.x[[1]]), Fleet=list(names(.x)))
      AddDimension(array, 'Area') |>
        aperm(c(1,2,3,5,4))
    })
    SelectivityRetention <- purrr::map2(selectivity, retention, ArrayMultiply)
  }
  
  VBatAgeStockAreaFleetList <- purrr::map2(BatAgeAreaFleet, 
                                           SelectivityRetention, 
                                           ArrayMultiply)
  
  # TODO
  # - account for value by stock/age/length
  # - account for cost by area/fleet
  
  UtilityAreaFleet <- purrr::map(VBatAgeStockAreaFleetList,
                                 function(.x)
                                   apply(.x, c(1,3,4,5), sum))
  
  TotalUtilityAreaFleet <- purrr::map(UtilityAreaFleet,
                                      function(.x)
                                        apply(.x, c(1,2,4), sum)) |>
    purrr::map(\(x) AddDimension(x, name='Area')) |>
    purrr::map(\(x) aperm(x, c(1,2,4,3)) )
  
  UtilityAreaFleet <- purrr::map2(UtilityAreaFleet,TotalUtilityAreaFleet, ArrayDivide)
  
  UtilityAreaFleet <- purrr::map(UtilityAreaFleet, function(x) {
      x[!is.finite(x)] <- 0
      x
    })
  
  UtilityAreaFleet
}
