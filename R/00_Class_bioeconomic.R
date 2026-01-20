#' Bioeconomic Object
#'
#' Bioeconomic Information
#'
#' Revenue and Cost information for a [Fleet()] and a [Stock()]
#'
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#'
#'
#' @name Bioeconomic
#' @export
setClass("bioeconomic",
  slots = c(
    Revenue = "num.array", # Sim, Age, Year OR Sim, Length, Year
    Cost = "num.array", # Sim, Year, Area - operating cost of a unit of effort
    Investment = "num.array", # Sim, Year - investment cost of adding a new unit of effort to the fishery
    Disinvestment = "num.array", # Sim, Year - cost of removing a unit of effort from the fishery
    Depreciation = "num.array", # depreciation rate - Effort units dropping out each year due to eg degradation, expired license, etc
    Discount = "num.array", # discount factor
    Misc = 'list'
  )
)

setValidity('bioeconomic', function(object) {
  TRUE
})

setMethod("initialize", "bioeconomic", function(.Object,
                                                Revenue = NULL,
                                                Cost = NULL,
                                                Investment = NULL,
                                                Disinvestment = NULL,
                                                Depreciation = NULL,
                                                Discount = NULL,
                                                Misc = list()) {
  .Object@Revenue <- Revenue
  .Object@Cost <- Cost
  .Object@Investment <- Investment
  .Object@Disinvestment <- Disinvestment
  .Object@Depreciation <- Depreciation
  .Object@Discount <- Discount
  .Object@Misc <- Misc
  .Object
})

#' @rdname Bioeconomic
#' @export
Bioeconomic <- function(Revenue = NULL,
                        Cost = NULL,
                        Investment = NULL,
                        Disinvestment = NULL,
                        Depreciation = NULL,
                        Discount = NULL,
                        Misc = list()) {
  methods::new("bioeconomic",
    Revenue = Revenue,
    Cost = Cost,
    Investment = Investment,
    Disinvestment = Disinvestment,
    Depreciation = Depreciation,
    Discount = Discount,
    Misc = Misc
  )
}

#' @rdname Bioeconomic
#' @export
#'
Bioeconomic <- function(object) {
  object@Bioeconomic
}
