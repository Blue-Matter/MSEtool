Hist2MSE <- function(Hist, MPNames) {
  MSE <- new('mse')
  MSE@OM <- Hist@OM
  MSE@Unfished <- Hist@Unfished
  MSE@Reference <- Hist@Reference
  HistYears <- Years(Hist,'H')
  
  slots <- slotNames(MSE@Hist)
  for (sl in slots)  {
    slot(MSE@Hist, sl) <- slot(Hist, sl) |> SubsetYear(HistYears)
  }
  
  MSE <- Add_MP_Functions(MSE, MPNames)
 
  
  MSE <- InitializeTimeSeries(MSE, 'Projection', MPs=MPNames)
  
  MSE
}

Add_MP_Functions <- function(MSE, MPNames) {
  
  MSE@MPs <- lapply(MPNames, function(x) {
    
    mp <- try(get(x), silent=TRUE)
    
    if (inherits(mp, 'try-error')) {
      cli::cli_alert_warning('Cannot find MP: {.val {x}} Skipping ... ')
      return(NULL)
    }
    
    if (!inherits(mp, 'mp')) {
      cli::cli_alert_warning('MP: {.val {x}} is not class {.val `mp`}. Skipping ... ')
      return(NULL) 
    }
    
    MakeSelfContained(mp)
  })
  
  valid <- !vapply(MSE@MPs, is.null, logical(1))
  
  MSE@MPs <- MSE@MPs[valid]
  names(MSE@MPs) <- MPNames[valid]
  MSE
}