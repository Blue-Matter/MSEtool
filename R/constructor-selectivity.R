#' Selectivity
#'
#' Construct and manipulate a [selectivity-class] object defining
#' selectivity-at-age or selectivity-at-length for a [Fleet()] object.
#' Selectivity is required for all [fleet-class] objects.
#'
#' @param Pars Named list of selectivity parameters passed to `Model`. See
#'   [SelectivityModels()] for available models and their required parameters.
#'   If `Pars` is a [fleet-class] object, the `Selectivity` slot of that
#'   fleet is returned. If `Pars` and `Model` are both provided, they will
#'   be used to populate `MeanAtAge`, overwriting any existing values. To
#'   preserve user-specified values in `MeanAtAge`, set `Pars = list()`
#'   (default).
#' @param Model Character. Selectivity model identifier. If `NULL` (default),
#'   the model is inferred from `Pars` where possible. See
#'   [SelectivityModels()] for available models. 
#' @param MeanAtAge Numeric array. Mean selectivity at age, with named
#'   dimensions `Sim`, `Age`, and `Year`. If `MeanAtLength` is provided,
#'   `MeanAtAge` will be calculated from it. Otherwise, `MeanAtAge` is used
#'   directly, and `MeanAtLength` is calculated from `MeanAtAge` and a
#'   [Length()] object unless `MeanAtLength` is already populated. **Note:**
#'   if `Pars` and `Model` are both provided, any values supplied here will
#'   be overwritten.
#' @param MeanAtLength Numeric array. Mean selectivity at length, with named
#'   dimensions `Sim`, `Length`, and `Year`. If provided, takes precedence
#'   and `MeanAtAge` will be derived from it.
#' @param MeanAtWeight Numeric array. Mean selectivity at weight, with named
#'   dimensions `Sim`, `Weight`, and `Year`. Default `NULL`.
#' @param Classes Numeric vector. Age or length classes corresponding to the
#'   `Age` or `Length` dimension of `MeanAtAge` or `MeanAtLength`. Default
#'   `NULL`.
#' @param isRel Logical. Whether selectivity parameters are specified relative
#'   to maturity-at-length (e.g., `L50`). Default `FALSE`.
#' @param Misc List. Miscellaneous additional inputs. Default `list()`.
#' @param x A [selectivity-class] object, or a [fleet-class] object for
#'   `Selectivity<-`.
#' @param value For `Selectivity<-`: a [selectivity-class] object. For
#'   `isRel<-`: a logical value.
#'
#' @details
#' Selectivity is required for all [fleet-class] objects and defines the
#' probability of a fish being caught by the gear as a function of age,
#' length, or weight.
#'
#' ## Populating MeanAtAge and MeanAtLength
#'
#' The precedence for populating `MeanAtAge` is:
#'
#' 1. If `Pars` and `Model` are both provided, `MeanAtAge` is calculated from
#'    the model and **any existing values in `MeanAtAge` are overwritten**. To
#'    preserve user-specified values in `MeanAtAge`, leave `Pars = list()`
#'    (the default).
#' 2. If `MeanAtLength` is provided (and `Pars` is empty), `MeanAtAge` is
#'    derived from `MeanAtLength`.
#' 3. Otherwise, `MeanAtAge` is used directly and `MeanAtLength` is calculated
#'    from `MeanAtAge` and a [Length()] object, unless `MeanAtLength` is
#'    already populated.
#'
#' ## Relative Parameters
#'
#' When `isRel = TRUE`, length-based parameters (e.g., `L5`, `LFS`) are
#' interpreted as multiples of the length-at-50%-maturity (`L50`) rather than
#' absolute length values.
#'
#' ## Attaching to a Fleet
#'
#' A `Selectivity` object can be attached to a [Fleet()] with
#' `Selectivity(Fleet) <- MySelectivity` and retrieved with
#' `Selectivity(Fleet)`.
#'
#' Individual slots may be accessed or modified using [Pars()], [Model()],
#' [MeanAtAge()], [MeanAtLength()], [MeanAtWeight()], [Classes()], and
#' [isRel()].
#'
#' `r  AdviceArrayInfo('selectivity')`
#' 
#' `r TechManLink()`
#'
#' @return
#' - `Selectivity()` returns a [selectivity-class] object. If `Pars` is a
#'   [fleet-class] object, the `Selectivity` slot of that fleet is returned.
#' - `Selectivity<-` returns `x` with the `Selectivity` slot replaced.
#' - `isRel()` returns the `isRel` slot of `x`.
#' - `isRel<-` returns `x` with the `isRel` slot updated.
#'
#' @seealso [selectivity-class], [Fleet()], [SelectivityModels()],
#'   [Retention()], [DiscardMortality()]
#'
#' @examples
#' # Specify via model parameters
#' s <- Selectivity(Pars = list(L5 = 10, LFS = 20, Vmaxlen = 1))
#'
#' # Specify MeanAtAge directly (Pars must be empty to avoid overwriting)
#' s2 <- Selectivity(MeanAtAge = my_array)
#'
#' isRel(s)
#' isRel(s) <- TRUE
#'
#' @export
Selectivity <- function(Pars = list(),
                        Model = NULL,
                        MeanAtAge = NULL,
                        MeanAtLength = NULL,
                        MeanAtWeight = NULL,
                        Classes = NULL,
                        isRel = FALSE,
                        Misc = list()) {
  

  if (methods::is(Pars, "fleet"))
    return(Pars@Selectivity)
  
  methods::new(
    "selectivity",
    Pars = Pars,
    Model = Model,
    isRel = isRel,
    MeanAtAge = MeanAtAge,
    MeanAtLength = MeanAtLength,
    MeanAtWeight = MeanAtWeight,
    Classes = Classes,
    Misc = Misc
  )
}



#' @rdname Selectivity
#' @export
isRel <- function(x) {
  CheckClass(x, "selectivity", "x")
  x@isRel
}

#' @rdname Selectivity
#' @export
`isRel<-` <- function(x, value) {
  CheckClass(x, "selectivity", "x")
  x@isRel <- value
  methods::validObject(x)
  x
}

#' @rdname Selectivity
#' @export
`Selectivity<-`<- function(x,value) {
  AssignSlot(x, value, 'Selectivity')
}













