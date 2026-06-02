#' The `spatial` S4 Class
#'
#' Defines the spatial structure and movement dynamics for a [stock-class]
#' object. Objects are typically created via [Spatial()], which documents all
#' parameters and validates inputs. This object is optional — leave it empty
#' for non-spatial (single-area) models; see *Default Behaviour* in
#' [Spatial()].
#'
#' @slot UnfishedDist `numeric`, `array`, or `NULL`. Relative unfished biomass
#'   distribution across areas. After [Populate()], a named array with
#'   dimensions `Sim × Area × Age × Year`. For two-area models, specifies the
#'   fraction in Area 1; the Area 2 fraction is derived as `1 - UnfishedDist`.
#'   For multi-area models, values must sum to 1 across areas. See [Spatial()]
#'   for accepted input formats.
#' @slot ProbStaying `numeric` or `array`. Probability of remaining in an area
#'   between time steps. After [Populate()], a named array with dimensions
#'   `Sim × Area × Age × Year`. For two-area models, specifies Area 1 only;
#'   the Area 2 value is solved internally. See [Spatial()] for accepted input
#'   formats.
#' @slot RelativeSize `numeric`, `array`, or `character(1)`. Relative size of
#'   each area, used to compute biomass density. After [Populate()], a named
#'   `Sim × Area` array. The special value `"EqualDensity"` sets
#'   `RelativeSize` equal to the mean `UnfishedDist` across ages and years,
#'   producing constant density across areas. When `NULL`, all areas are
#'   assumed equal in size (`1/nArea` each).
#' @slot Movement `array` or `NULL`. Movement probability matrix with named
#'   dimensions `Sim × FromArea × ToArea × Age × Year`. Row `[i, ]` gives the
#'   probabilities of moving from area `i` to each other area; rows sum to 1.
#'   Populated automatically during [Populate()] from `UnfishedDist`,
#'   `ProbStaying`, and optionally `FracOther`. May also be supplied directly,
#'   in which case the asymptotic unfished distribution is derived from
#'   `Movement` and assigned to `UnfishedDist`.
#' @slot FracOther `array` or `NULL`. Required for models with more than two
#'   areas. Named array with dimensions `Sim × FromArea × ToArea × Age ×
#'   Year`. Diagonal elements must be `NA` (probability of staying is taken
#'   from `ProbStaying`). Off-diagonal element `[i, j]` gives the relative
#'   probability of moving from area `i` to area `j`. Used as a penalty-weight
#'   matrix during movement optimisation.
#' @slot Arrangement `array` or `NULL`. Spatial layout of areas for plotting
#'   purposes. Reserved for future use; currently stored but not applied.
#' @slot CVDist `numeric(1)`. Standard deviation of the log-scale penalty on
#'   the difference between the optimised asymptotic distribution and the
#'   target `UnfishedDist` during multi-area movement optimisation. Larger
#'   values allow the fitted movement matrix to deviate more from the target
#'   distribution. Has no effect for two-area models. Default `0.1`.
#' @slot CVStay `numeric(1)`. Standard deviation of the logit-scale penalty on
#'   the difference between the optimised diagonal (staying probability) and
#'   the target `ProbStaying` during multi-area movement optimisation. Larger
#'   values allow more deviation from the target staying probability. Has no
#'   effect for two-area models. Default `1`.
#' @slot Misc `list`. Used internally.
#'
#' @details
#' Direct construction via [methods::new()] is not recommended; use [Spatial()]
#' instead, which validates inputs and handles dimension expansion.
#'
#' For non-spatial (single-area) models, leave the `Spatial` slot of
#' [stock-class] empty. [Populate()] creates a default single-area structure
#' automatically.
#'
#' The movement matrix is fitted by numerical optimisation to simultaneously
#' reproduce the target `UnfishedDist` and `ProbStaying`. For multi-area
#' models, `CVDist` and `CVStay` control the relative weight given to each
#' target in the objective function.
#'
#' @seealso
#' - [Spatial()] for the constructor and accessor functions.
#' - [Populate()] for array population and movement matrix fitting.
#' - [Stock()] for the enclosing stock constructor.
#'
#' @family spatial
#'
#' @include class-unions.R
#' @name spatial-class
setClass('spatial',
         slots = c(
           UnfishedDist = "num.array.null",
           ProbStaying  = "num.array.null",
           RelativeSize = "array.char.num",
           Movement     = "array.null",
           FracOther    = "array.null",
           Arrangement  = "array.null",
           CVDist       = "numeric",
           CVStay       = "numeric",
           Misc         = "list"
         )
)


setValidity('spatial', function(object) {
  # TODO 
  TRUE
})
