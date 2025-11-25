
methods::setClassUnion(name="list.data", members=c('list', "data"))


#' Class Hist
#' 
#' More 
#' 
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#' @include 00_Class_data.R
#' @include 00_Class_om.R
#' @include 00_Class_unfished.R
#' @include 00_Class_reference.R
#' @include 00_Class_refpointsMSY.R
#' @include 00_Class_refpointsPR.R
#' @include 00_Class_timeseries.R
#' @name HistClass
#' 
#' @export
setClass("hist",
         slots=c(OM='om',
                 Unfished='unfished',
                 Reference='reference',
                 
                 RefPointsMSY='refpointsMSY', # keep for now TODO - remove
                 RefPointsPR='refpointsPR', # keep for now TODO - remove
                 RefLandings='array.null', # keep for now TODO - remove
                 RefRemovals='array.null', # keep for now TODO - remove
                 Data='list.data',
                 Log='list'
         ), 
         contains=c('timeseries',
                    'MiscClass')
)



Hist <- function(OM=NULL, ...) {
  if (is.null(OM))
    return(new('hist'))
  
  if (!inherits(OM,'om'))
    cli::cli_abort('`OM` must be class `om`')
  
  OM2Hist(OM, ...)
}

