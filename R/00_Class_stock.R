
#' Stock Class and Constructor
#' 
#' @include 00_Class_unions.R
#' @include 00_Class_ages.R
#' @include 00_Class_length.R
#' @include 00_Class_weight.R
#' @include 00_Class_naturalmortality.R
#' @include 00_Class_maturity.R
#' @include 00_Class_fecundity.R 
#' @include 00_Class_srr.R
#' @include 00_Class_spatial.R
#' @include 00_Class_depletion.R
#' 
#' The `Stock()` function is used to create, access, or assign `Stock` objects.
#' A `Stock` object defines the biological and population-dynamics properties
#' of a tock used in an operating model (see [OM()]).
#'
#' @details
#' Objects of class `Stock` aggregate age structure, growth, mortality,
#' reproduction, recruitment, and optional spatial dynamics into a single object.
#' 
#'
#' A `Stock` object can be accessed from an [OM()] object using `Stock(om)` or
#' `Stock(om, i)` when multiple stocks are present.
#'
#' @slot Name Character string. Unique name for the stock.
#' @slot CommonName Character string. Common name.
#' @slot Species Character string. Scientific name.
#' @slot Ages An [Ages()] object (required).
#' @slot Length A [Length()] object.
#' @slot Weight A [Weight()] object.
#' @slot NaturalMortality A [NaturalMortality()] object.
#' @slot Maturity A [Maturity()] object.
#' @slot Fecundity A [Fecundity()] object.
#' @slot SRR A [SRR()] object.
#' @slot Spatial A [Spatial()] object.
#' @slot Depletion A [Depletion()] object.
#' @slot nSim Number of simulations.
#' @slot CurrentYear Final historical year.
#' @slot Years Numeric vector of model years.
#' @slot Misc List. User-defined metadata.
#' @slot Log List. Internal use.
#'
#' @name Stock
#' @rdname Stock
#' 
#'
NULL


setClass(
  "stock",
  slots = c(
    Name = "char.null",
    CommonName = "char.null",
    Species = "char.null",
    Ages = "ages",
    Length = "length",
    Weight = "weight",
    NaturalMortality = "naturalmortality",
    Maturity = "maturity",
    Fecundity = "fecundity",
    SRR = "srr",
    Spatial = "spatial",
    Depletion = "depletion",
    nYear = "num.null",
    pYear = "num.null",
    nSim = "num.null",
    CurrentYear = "num.null",
    Years = "num.null",
    Seasons = "num.null",
    Misc = "list",
    Log = "list"
  )
)

setValidity("stock", function(object) {
  # TODO 
  TRUE
})





