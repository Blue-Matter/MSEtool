#' @include 00_Class_selectivity.R
#' @include 00_Class_retention.R
#' @include 00_Class_discardmortality.R
#' @include 00_Class_fishingmortality.R
setClass('advice',
         slots=c(TAC='numeric',
                 Effort='numeric',
                 Spatial='numeric',
                 Selectivity='selectivity',
                 Retention='retention',
                 DiscardMortality='discardmortality',
                 apicalF='numeric',
                 Misc='list',
                 Log='list'
         )
)

#' @export
Advice <- function(DataList=NULL) {
  # TODO - populate selectivity model parameters etc
  new('advice')
}
