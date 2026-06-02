#' The `selectivity` S4 Class
#'
#' Defines selectivity-at-age, selectivity-at-length, or
#' selectivity-at-weight for use in a [fleet-class] object. Selectivity is
#' required for all fleets. Objects are typically created via the
#' [Selectivity()] constructor, which documents all parameters in detail.
#'
#' @slot Pars `list`. Named list of selectivity parameters passed to `Model`.
#'   See [Selectivity()] and [SelectivityModels()].
#' @slot Model `function` or `character` or `NULL`. Selectivity model
#'   identifier or function. See [SelectivityModels()].
#' @slot isRel `logical`, `numeric`, or `character`. Whether length-based
#'   parameters are expressed relative to maturity `L50`. See [Selectivity()].
#' @slot MeanAtAge `numeric` array or `NULL`. Mean selectivity-at-age
#'   (`Sim x Age x Year x Area`). See [Selectivity()].
#' @slot MeanAtLength `numeric` array or `NULL`. Mean selectivity-at-length
#'   (`Sim x Length x Year x Area`). See [Selectivity()].
#' @slot MeanAtWeight `numeric` array or `NULL`. Mean selectivity-at-weight
#'   (`Sim x Weight x Year x Area`). See [Selectivity()].
#' @slot Classes `numeric` or `NULL`. Length or weight class midpoints
#'   corresponding to the second dimension of `MeanAtLength` or
#'   `MeanAtWeight`.
#' @slot Misc `list`. Miscellaneous additional inputs. Used internally.
#'
#' @seealso 
#' - [Selectivity()] for the constructor and full parameter
#'   documentation. 
#'  - [SelectivityModels()] for available model functions.
#'  - [fleet-class] for the enclosing fleet object.
#'
#' @family fleet
#'
#' @include class-unions.R
#' @name selectivity-class
setClass(
  "selectivity",
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

setValidity("selectivity", function(object) {
  # TODO
  TRUE
})
