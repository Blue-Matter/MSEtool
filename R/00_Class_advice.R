## Advice ----

#' Advice Class and Constructor
#'
#' The `Advice` class defines management advice returned by a Management Procedure
#'
#'
#' @param x
#' * For `Advice()`: missing or a [Stock()] object.
#' * For `Advice<-`: a [Stock()] object.
#'
#' @param TAC Numeric array specifying total allowable catch.
#'
#' @param Effort Numeric array specifying relative or absolute fishing effort.
#'
#' @param Closure Numeric or logical array specifying spatial or temporal
#'   fishery closures.
#'
#' @param Selectivity A `selectivity` object or list of such objects.
#'
#' @param Retention A `retention` object or list of such objects.
#'
#' @param DiscardMortality A `discardmortality` object or list of such objects.
#'
#' @param apicalF Numeric array specifying apical fishing mortality.
#'
#' @param value An `Advice` object to assign.
#'
#' @details
#'
#' The `Advice` generic is used to construct a new `Advice` object
#'
#' Advice may be specified in terms of:
#'
#' * output controls: `TAC`;
#' * input controls: `Effort`;
#' * spatial controls: `Closure`;
#' * gear effects: `Selectivity`, `Retention`, `DiscardMortality`;
#' * direct fishing mortality (`apicalF`).
#'
#'
#' @return an empty `Advice` object
#'
#' @seealso [Stock()], [Selectivity()], [Retention()], [DiscardMortality()]
#'
#' @name Advice
#' @rdname Advice
#'
#' @include 00_Class_unions.R
#' @include 00_Class_selectivity.R
#' @include 00_Class_retention.R
#' @include 00_Class_discardmortality.R
NULL


methods::setClassUnion(
  name    = "selectivity.list",
  members = c("selectivity", "list")
)

methods::setClassUnion(
  name    = "retention.list",
  members = c("retention", "list")
)

methods::setClassUnion(
  name    = "discardmortality.list",
  members = c("discardmortality", "list")
)


setClass("advice",
         slots = c(TAC = "array.numeric.null",
                   Effort = "array.numeric.null",
                   Closure = "array.numeric.null",
                   Selectivity = "selectivity.list",
                   Retention = "retention.list",
                   DiscardMortality = "discardmortality.list",
                   apicalF = "array.numeric.null",
                   Misc = "list",
                   Log = "list")
)


setValidity("advice", function(object) {
  TRUE
})


setMethod(
  "initialize",
  "advice",
  function(.Object,
           TAC = numeric(),
           Effort = numeric(),
           Closure = numeric(),
           Selectivity = new('selectivity'),
           Retention = new('retention'),
           DiscardMortality = new('discardmortality'),
           apicalF = numeric()) {
    
    .Object@TAC              <- TAC
    .Object@Effort           <- Effort
    .Object@Closure          <- Closure
    .Object@Selectivity      <- Selectivity
    .Object@Retention        <- Retention
    .Object@DiscardMortality <- DiscardMortality
    .Object@apicalF          <- apicalF
    
    .Object
  }
)



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

