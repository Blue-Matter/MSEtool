
ProcessMiscOM <- function(Hist) {
  CheckClass(Hist,'hist')
  
  Misc <- Hist@OM@Misc
  
  if (is.null(Misc))
    return(Hist)

  Hist |>
    FillFromMisc('Number', FALSE) |>
    FillFromMisc('Biomass', FALSE) |> 
    FillFromMisc('SBiomass', FALSE) |> 
    FillFromMisc('SProduction') |> 
    FillFromMisc('Landings', FALSE) |> 
    FillFromMisc('Discards', FALSE) |> 
    FillFromMisc('Effort', FALSE) |> 
    FillFromMisc('FDead', FALSE) |> 
    FillFromMisc('FRetain', FALSE) |> 
    FillFromMisc('FDeadArea', FALSE) |> 
    FillFromMisc('FRetainArea', FALSE)
  
}

FillFromMisc <- function(Hist, sl='Biomass', done=TRUE) {
  
  value <- Hist@OM@Misc[[sl]]
  if (is.null(value)) 
    return(Hist)
  
  if (!done) {
    cli::cli_alert_warning('`OM@Misc@{sl}` not done yet. Ignoring')
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
  
  value <- ExpandSims(value, nSim(Hist))
  
  ArrayFill(slot(Hist, sl)) <- value
  Hist
}
