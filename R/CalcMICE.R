CalcMICE <- function(Hist, TimeSteps=NULL) {
  
  HistRel <- SetHistRel(OM) 
  
  if (length(Hist@Relations)>0) {
    cli::cli_abort('MICE not done', .interal=TRUE)
    
    # TODO
    # update all relevant at-age/length Stock and Fleet dynamics
    # based on MICE relations
    # only for `TimeSteps` 
  }
  
  
  Hist
}