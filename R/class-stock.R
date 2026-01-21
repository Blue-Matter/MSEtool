#' Stock Class
#'
#' The `stock` class defines the biological and population-dynamics
#' properties of a stock used in an operating model.
#'
#' @slot Name Character string. Unique stock name.
#' @slot CommonName Character string. Common name.
#' @slot Species Character string. Scientific name.
#' @slot Ages An [Ages()] object.
#' @slot Length A [Length()] object.
#' @slot Weight A [Weight()] object.
#' @slot NaturalMortality A [NaturalMortality()] object.
#' @slot Maturity A [Maturity()] object.
#' @slot Fecundity A [Fecundity()] object.
#' @slot SRR A [SRR()] object.
#' @slot Spatial A [Spatial()] object.
#' @slot Depletion A [Depletion()] object.
#' @slot nYear Number of historical years.
#' @slot pYear Number of projection years.
#' @slot nSim Number of simulations.
#' @slot CurrentYear Final historical year.
#' @slot Years Numeric vector of model years.
#' @slot Seasons Number of seasons.
#' @slot Misc Miscellaneous list.
#' @slot Log List. Internal use.
#'
#' @include class-unions.R
#' @include class-ages.R
#' @include class-length.R
#' @include class-weight.R 
#' @include class-naturalmortality.R 
#' @include class-maturity.R
#' @include class-fecundity.R
#' @include class-srr.R
#' @include class-spatial.R
#' @include class-depletion.R
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
  # TODO: structural consistency checks
  TRUE
})
