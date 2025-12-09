#' MSE Class
#' 
#' @include 00_Class_unions.R
#' @include 00_Class_om.R
#' @include 00_Class_hist.R
#' @include 00_Class_unfished.R
#' @include 00_Class_reference.R
#' @include 00_Class_refpointsMSY.R
#' @include 00_Class_refpointsPR.R
#' @include 00_Class_timeseries.R
#' @name MSEClass
setClass("mse",
         slots=c(OM='om',
                 MPs='list',
                 Unfished='unfished',
                 Reference='reference',
                 
                 RefPointsMSY='refpointsMSY', # keep for now TODO - remove
                 RefPointsPR='refpointsPR', # keep for now TODO - remove
                 RefLandings='array.null', # keep for now TODO - remove
                 RefRemovals='array.null', # keep for now TODO - remove
                 
                 Hist='timeseries',
                 PPD='list',
                 Log='list'
         ), 
         contains=c('timeseries',
                    'MiscClass')
)

MSE <- function(Hist=NULL, MPs=NULL,...) {
  if (is.null(Hist))
    return(new('mse'))
  
  if (!inherits(Hist,'hist'))
    cli::cli_abort('`Hist` must be class `hist`')
  
  Hist2MSE(Hist, MPs, ...)
}
