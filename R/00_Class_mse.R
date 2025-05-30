#' MSE Class
#' 
#' @include 00_Class_unions.R
#' @include 00_Class_om.R
#' @include 00_Class_hist.R
#' @include 00_Class_unfished.R
#' @include 00_Class_refpoints.R
#' @include 00_Class_timeseries.R
#' @name MSEClass
setClass("mse",
         slots=c(OM='om',
                 Unfished='unfished',
                 RefPoints='refpoints',
                 Hist='timeseries',
                 PPD='list' 
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
