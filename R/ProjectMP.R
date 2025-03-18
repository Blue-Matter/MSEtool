#' Projects a single MP from the output of `Simulate`
ProjectMP <- function(OMList, MP) {
  
  OMList <- rlang::duplicate(OMList)
  
  TimeStepsProj <- OMList[[1]]$TimeStepsProj[[1]]
  
  # Run MP for all Simulations
  start <- Sys.time()
  MPList <- tryCatch(
    purrr::map(OMList, CalcPopDynamics, Period="Projection", MP=MP,
               .progress = list(
                 type = "iterator",
                 format = "Running MP {cli::pb_bar} {cli::pb_percent}",
                 clear = TRUE
               )
               )
  )
  
  end <- Sys.time() 
  elapsed <- round(as.numeric(difftime(time1 = end, time2 = start, units = "secs")),2)
  cli::cli_alert_success("{.val {MP}} ({elapsed} seconds)")
  
  NumberAtAgeArea <- purrr::map(MPList, \(x)
                                purrr::map(x$NumberAtAgeArea, \(y) 
                                           ArraySubsetTimeStep(y, TimeStepsProj))) |> 
    ReverseList() |>
    purrr::map(List2Array, 'Sim') |>
    purrr::map(\(x) aperm(x, c("Sim", "Age", "TimeStep", "Area")))
  
  
  Biomass <- purrr::map(MPList, \(x)
                            purrr::map(x$Biomass, \(y) 
                                       ArraySubsetTimeStep(y, TimeStepsProj))) |> 
    ReverseList() |>
    purrr::map(List2Array, 'Sim', 'TimeStep') |>
    List2Array("Stock") |> 
    aperm(c("Sim", "Stock", "TimeStep"))
    
  SBiomass <- purrr::map(MPList, \(x)
                        purrr::map(x$SBiomass, \(y) 
                                   ArraySubsetTimeStep(y, TimeStepsProj))) |> 
    ReverseList() |>
    purrr::map(List2Array, 'Sim', 'TimeStep') |>
    List2Array("Stock") |> 
    aperm(c("Sim", "Stock", "TimeStep"))
  
  SProduction <- purrr::map(MPList, \(x)
                         purrr::map(x$SProduction, \(y) 
                                    ArraySubsetTimeStep(y, TimeStepsProj))) |> 
    ReverseList() |>
    purrr::map(List2Array, 'Sim', 'TimeStep') |>
    List2Array("Stock") |> 
    aperm(c("Sim", "Stock", "TimeStep"))
  
  # Removal Number-at-Age-Area
  RemovalAtAgeArea <- purrr::map(MPList, \(x)
                        purrr::map(x$RemovalAtAgeArea, \(y) {
                          y <- List2Array(y, "TimeStep")                
                          ArraySubsetTimeStep(y, TimeStepsProj)
                        })) |> 
    ReverseList() |>
    purrr::map(List2Array, 'Sim', 'TimeStep') |>
    purrr::map(\(x) aperm(x, c("Sim", "Age", "TimeStep", "Fleet", "Area")))
  
  # Removed Biomass (summed over areas)
  Removal <- purrr::map(MPList, \(x)
                        purrr::map(x$RemovalBiomassAtAge, \(y) 
                                   ArraySubsetTimeStep(y, TimeStepsProj))) |> 
    ReverseList() |>
    purrr::map(List2Array, 'Sim') |>
    purrr::map(\(x) aperm(x, c("Sim", "Age", "TimeStep", "Fleet")))

  # Retained Biomass (summed over areas)
  Retain <- purrr::map(MPList, \(x)
                        purrr::map(x$RetainBiomassAtAge, \(y) 
                                   ArraySubsetTimeStep(y, TimeStepsProj))) |> 
    ReverseList() |>
    purrr::map(List2Array, 'Sim') |>
    purrr::map(\(x) aperm(x, c("Sim", "Age", "TimeStep", "Fleet")))
    
  
  
  
  EffortArea <- purrr::map(MPList, \(x)
                        purrr::map(x$EffortArea, \(y) 
                                   ArraySubsetTimeStep(y, TimeStepsProj))) |> 
    ReverseList() |>
    purrr::map(List2Array, 'Sim', 'TimeStep') |>
    List2Array('Stock') |>
    aperm(c("Sim", "Stock", "TimeStep", "Fleet", "Area"))
  
  # EffortArea[1,1,,1,]
  
  FDeadAtAgeArea <- purrr::map(MPList, \(x)
                           purrr::map(x$FDeadAtAgeArea, \(y) {
                             y <- List2Array(y, 'TimeStep') 
                             ArraySubsetTimeStep(y, TimeStepsProj)
                           }))|> 
    ReverseList() |>
    purrr::map(List2Array, 'Sim') |>
    List2Array('Stock') |>
    aperm(c("Sim", "Stock", "Age" , "TimeStep", "Fleet", "Area"))
  
  # FDeadAtAgeArea[1,1,12,,1,]
  
  
  list(NumberAtAgeArea=NumberAtAgeArea,
       Biomass=Biomass,
       SBiomass=SBiomass,
       SProduction=SProduction,
       Removal=Removal,
       Retain=Retain,
       RemovalAtAgeArea=RemovalAtAgeArea)
  
}

