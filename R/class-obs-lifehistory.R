
#' Life-History Observation Component
#'
#' Internal class describing observation error on life-history parameters.
#'
#' @slot Ages Age observations.
#' @slot Length Length observations.
#' @slot Weight Weight observations.
#' @slot NaturalMortality Natural mortality observations.
#' @slot Maturity Maturity observations.
#' @slot Fecundity Fecundity observations.
#' @slot SRR Stock–recruit observations.
#' @slot Spatial Spatial observations.
#' @slot Depletion Depletion observations.
#' @slot Misc Miscellaneous additional information.
#'
#' @include class-unions.R
setClass(
  "lifehistoryobs",
  slots = c(
    Ages             = "list",
    Length           = "list",
    Weight           = "list",
    NaturalMortality = "list",
    Maturity         = "list",
    Fecundity        = "list",
    SRR              = "list",
    Spatial          = "list",
    Depletion        = "list",
    Misc             = "list"
  )
)
