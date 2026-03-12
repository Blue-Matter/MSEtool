#' Retention
#'
#' Construct and manipulate a [retention-class] object defining
#' retention-at-age or retention-at-length for a [Fleet()] object. Retention
#' is optional for [fleet-class] objects. If not specified, the model assumes
#' that all age and length classes are fully retained.
#'
#' @param Pars Named list of retention parameters passed to `Model`. See
#'   [RetentionModels()] for available models and their required parameters.
#'   If `Pars` is a [fleet-class] object, the `Retention` slot of that fleet
#'   is returned. If `Pars` and `Model` are both provided, they will be used
#'   to populate `MeanAtAge`, overwriting any existing values. To preserve
#'   user-specified values in `MeanAtAge`, set `Pars = list()` (default).
#' @param Model Character. Retention model identifier. If `NULL` (default),
#'   the model is inferred from `Pars` where possible. See
#'   [RetentionModels()] for available models. 
#' @param MeanAtAge Numeric array. Mean retention at age, with named
#'   dimensions `Sim`, `Age`, and `Year`. If `MeanAtLength` is provided,
#'   `MeanAtAge` will be calculated from it. Otherwise, `MeanAtAge` is used
#'   directly, and `MeanAtLength` is calculated from `MeanAtAge` and a
#'   [Length()] object unless `MeanAtLength` is already populated. **Note:**
#'   if `Pars` and `Model` are both provided, any values supplied here will
#'   be overwritten.
#' @param MeanAtLength Numeric array. Mean retention at length, with named
#'   dimensions `Sim`, `Length`, and `Year`. If provided, takes precedence
#'   and `MeanAtAge` will be derived from it.
#' @param MeanAtWeight Numeric array. Mean retention at weight, with named
#'   dimensions `Sim`, `Weight`, and `Year`. Default `NULL`.
#' @param Classes Numeric vector. Age or length classes corresponding to the
#'   `Age` or `Length` dimension of `MeanAtAge` or `MeanAtLength`. Default
#'   `NULL`.
#' @param isRel Logical. Whether retention parameters are specified relative
#'   to maturity-at-length (e.g., `L50`). Default `FALSE`.
#' @param Misc List. Miscellaneous additional inputs. Default `list()`.
#' @param x A [retention-class] object, or a [fleet-class] object for
#'   `Retention<-`.
#' @param value For `Retention<-`: a [retention-class] object.
#'
#' @details
#' Retention is optional for all [fleet-class] objects. If not specified, the
#' model assumes that all age and length classes are fully retained (i.e.,
#' retention = 1 for all classes).
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
#' When `isRel = TRUE`, length-based parameters (e.g., `LR5`, `LFR`) are
#' interpreted as multiples of the length-at-50%-maturity (`L50`) rather than
#' absolute length values.
#'
#' ## Attaching to a Fleet
#'
#' A `Retention` object can be attached to a [Fleet()] with
#' `Retention(Fleet) <- MyRetention` and retrieved with `Retention(Fleet)`.
#'
#' Individual slots may be accessed or modified using [Pars()], [Model()],
#' [MeanAtAge()], [MeanAtLength()], [MeanAtWeight()], and [Classes()].
#'
#' `r  AdviceArrayInfo('retention')`
#' 
#' `r TechManLink()`
#'
#' @return
#' - `Retention()` returns a [retention-class] object. If `Pars` is a
#'   [fleet-class] object, the `Retention` slot of that fleet is returned.
#' - `Retention<-` returns `x` with the `Retention` slot replaced.
#'
#' @seealso [retention-class], [Fleet()], [RetentionModels()],
#'   [Selectivity()], [DiscardMortality()]
#'
#' @examples
#' # Specify via model parameters
#' r <- Retention(Pars = list(LR5 = 10, LFR = 20, Rmaxlen = 1))
#'
#' # Specify MeanAtAge directly (Pars must be empty to avoid overwriting)
#' r2 <- Retention(MeanAtAge = my_array)
#'
#' # Default: full retention for all classes
#' r3 <- Retention()
#'
#' @export
Retention <- function(Pars = list(),
                      Model = NULL,
                      MeanAtAge = NULL,
                      MeanAtLength = NULL,
                      MeanAtWeight = NULL,
                      Classes = NULL,
                      isRel = FALSE,
                      Misc = list()) {
  
  ## Fleet pass-through
  if (methods::is(Pars, "fleet"))
    return(Pars@Retention)
  
  object <- methods::new(
    "retention",
    Pars = Pars,
    Model = Model,
    isRel = isRel,
    MeanAtAge = MeanAtAge,
    MeanAtLength = MeanAtLength,
    MeanAtWeight = MeanAtWeight,
    Classes = Classes,
    Misc = Misc
  )
  object

}

#' @rdname Retention
#' @export
`Retention<-` <- function(x,value) {
  AssignSlot(x, value, 'Retention')
}




