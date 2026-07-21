.CalcMICE <- function(Hist, Years=NULL) {
  
  HistRel <- .SetHistRel(Hist) 
  
  if (length(Hist@Relations)>0) {
    cli::cli_abort('MICE not done', .interal=TRUE)
    
    # TODO
    # update all relevant at-age/length Stock and Fleet dynamics
    # based on MICE relations
    # only for `Years` 
  }
  
  
  Hist
}

.SetHistRel <- function(OM) {
  # Ignore MICE in historical period
  if (isFALSE(OM@Control$HistRel))
    return(list())
  Relations(OM) 
}
