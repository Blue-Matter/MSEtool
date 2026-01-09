

FillFromMisc <- function(Hist) {
  
  if (!length(Misc)) {
    return(Hist@OM@Misc)
  }
 
    
  Hist |>
    Misc2Hist('Number') |>
    Misc2Hist('Biomass') |> 
    Misc2Hist('SBiomass') |> 
    Misc2Hist('SProduction') |> 
    Misc2Hist('Landings') |> 
    Misc2Hist('Discards') |> 
    Misc2Hist('Effort') |> 
    Misc2Hist('Distribution') |> 
    Misc2Hist('Catchability') |> 
    Misc2Hist('qArea') |> 
    Misc2Hist('FDead') |> 
    Misc2Hist('FRetain') |> 
    Misc2Hist('FDeadArea') |> 
    Misc2Hist('FRetainArea')
  
}

Misc2Hist <- function(Hist, sl='Biomass') {
  
  value <- Hist@OM@Misc[[sl]]
  if (is.null(value)) {
    return(Hist)
  }
    
  ReqDimNames <- slot(Hist, sl) |> dimnames() |> names()
  if (inherits(value, 'data.frame')) {
    InDimNames <- colnames(value)
    ReqDimNames <- c(ReqDimNames, 'Value')
    
    if (!prod(ReqDimNames %in% InDimNames)) {
      cli::cli_abort(c("Missing columns in data.frame: {.val OM@Misc${sl}}",
                       "i"='Expected: {.val {ReqDimNames}}',
                       "x"='Provided: {.val {InDimNames}}')
      )
    }
    value <- DF2Array(value) 
  } 
  ReqDimNames <- slot(Hist, sl) |> dimnames() |> names()
  InDimNames <- value |> dimnames() |> names()  
  
  if (!prod(ReqDimNames %in% InDimNames)) {
    cli::cli_abort(c("Missing named dimensions in array: {.val OM@Misc${sl}}",
                     "i"='Expected: {.val {ReqDimNames}}',
                     "x"='Provided: {.val {InDimNames}}')
    )
  }
  
  value <- ExtendSims(value, nSim(Hist))
  
  ArrayFill(slot(Hist, sl)) <- value
  Hist
}