#' Spatial Class
#'
#' An S4 class defining the spatial structure and movement dynamics associated
#' with a [Stock()] object.
#'
#' See [Spatial()] for details.
#' 
#' @slot UnfishedDist Numeric or numeric array giving the relative unfished
#'   biomass distribution over areas.
#' @slot ProbStaying Numeric or numeric array giving the probability of
#'   remaining in an area.
#' @slot RelativeSize Numeric or numeric array giving relative area sizes.
#' @slot Movement Numeric array giving movement probabilities among areas.
#' @slot FracOther Numeric array defining relative movement among areas when
#'   more than two areas are present.
#' @slot Arrangement Numeric matrix defining the spatial layout of areas for
#'   plotting purposes.
#' @slot CVDist Logit-scale CV penalty applied to `UnfishedDist`.
#' @slot CVStay Logit-scale CV penalty applied to `ProbStaying`.
#' @slot Misc Miscellaneous list for user-defined information.
#'
#'
#' @include class-unions.R
#' @name spatial-class
#'
setClass('spatial',
         slots=c(UnfishedDist='num.array.null',
                 ProbStaying='num.array.null',
                 RelativeSize='array.char.num',
                 Movement='array.null',
                 FracOther='array.null',
                 Arrangement='array.null',
                 CVDist='numeric',
                 CVStay='numeric',
                 Misc='list'
         )
)


setValidity('spatial', function(object) {
  # TODO 
  TRUE
})
