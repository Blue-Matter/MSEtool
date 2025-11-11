methods::setClassUnion(name="selectivity.list", members=c("selectivity", 'list'))
methods::setClassUnion(name="retention.list", members=c("retention", 'list'))
methods::setClassUnion(name="discardmortality.list", members=c("discardmortality", 'list'))

#' @include 00_Class_selectivity.R
#' @include 00_Class_retention.R
#' @include 00_Class_discardmortality.R
#' @include 00_Class_fishingmortality.R
setClass('advice',
         slots=c(TAC='array.numeric.null',
                 Effort='array.numeric.null',
                 Closure='array.numeric.null',
                 Selectivity='selectivity.list',
                 Retention='retention.list',
                 DiscardMortality='discardmortality.list',
                 apicalF='array.numeric.null',
                 Misc='list',
                 Log='list'
         )
)

#' @export
Advice <- function(DataList=NULL) {
  # TODO - populate selectivity model parameters etc
  new('advice')
}

# MP - class `mp`
# - returns an Advice() object

#  TAC:
#   1. numeric length 1 - applies to all Fleets 
#   2. numeric length nFleet - TAC for each fleet
#   3. array - nFleet by nArea - TAC for each fleet and area
#   Distributed across Stocks (and Fleets in case of 1) according to Allocation
#
#  Effort: 
#   1. numeric length 1 - applies to all Fleets
#   2. numeric length nFleet - TAC for each fleet
#   3. array - nFleet by nArea - Effort for each fleet and area
#
#  Spatial
#  1. numeric length nArea - applies to all Fleets
#  2. array - nFleet by nArea - Closure for each fleet and area


# MMP - class `mmp`
# - returns a list of Advice() objects. One for each stock or stock complex




# Advice <- Advice()
# 
# Advice@TAC

