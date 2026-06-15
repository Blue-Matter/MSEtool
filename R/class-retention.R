#' The `retention` S4 Class
#'
#' Defines retention-at-age, retention-at-length, or retention-at-weight for
#' use in a [fleet-class] object. Retention is optional; if not specified, all
#' age and length classes are assumed fully retained. Objects are typically
#' created via the [Retention()] constructor, which documents all parameters
#' in detail.
#'
#' @slot Pars `list`. Named list of retention parameters passed to `Model`.
#'   See [Retention()] and [RetentionModels()].
#' @slot Model `function` or `character` or `NULL`. Retention model identifier
#'   or function. See [RetentionModels()].
#' @slot isRel `logical`, `numeric`, or `character`. Whether length-based
#'   parameters are expressed relative to maturity `L50`. See [Retention()].
#' @slot MeanAtAge `numeric` array or `NULL`. Mean retention-at-age
#'   (`Sim x Age x Year x Area`). See [Retention()].
#' @slot MeanAtLength `numeric` array or `NULL`. Mean retention-at-length
#'   (`Sim x Length x Year x Area`). Values are evaluated at the midpoint of
#'   each bin (halfway between consecutive lower bounds); the `Class` dimension
#'   is labelled by bin lower bounds (see `Classes`). See [Retention()].
#' @slot MeanAtWeight `numeric` array or `NULL`. Mean retention-at-weight
#'   (`Sim x Weight x Year x Area`). Values are evaluated at the midpoint of
#'   each bin; the `Class` dimension is labelled by bin lower bounds (see
#'   `Classes`). See [Retention()].
#' @slot Classes `numeric` or `NULL`. Lower bounds of length or weight bins
#'   corresponding to the second dimension of `MeanAtLength` or `MeanAtWeight`.
#'   Bin `k` spans `[Classes[k], Classes[k+1])`; the final bin is open-ended.
#'   Values in `MeanAtLength`/`MeanAtWeight` are evaluated at bin midpoints,
#'   not at these lower bounds.
#' @slot Misc `list`. Miscellaneous additional inputs. For internal use.
#'
#' @seealso 
#' - [Retention()] for the constructor and full parameter
#'   documentation. 
#' - [RetentionModels()] for available model functions.
#' - [fleet-class] for the enclosing fleet object.
#'
#' @family fleet
#'
#' @include class-unions.R
#' @name retention-class
setClass(
  "retention",
  slots = c(
    Pars         = "list",
    Model        = "fun.char",
    isRel        = "char.log.num",
    MeanAtAge    = "num.array.null",
    MeanAtLength = "num.array.null",
    MeanAtWeight = "num.array.null",
    Classes      = "num.null",
    Misc         = "list"
  )
)

setValidity("retention", function(object) {
  # TODO
  TRUE
})
