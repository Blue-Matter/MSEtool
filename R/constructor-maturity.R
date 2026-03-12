#' Maturity
#'
#' Construct and manipulate a [maturity-class] object defining the maturity
#' schedule associated with a [Stock()] object. A `Maturity` object is
#' required for all [stock-class] objects.
#'
#' @param Pars Named list of maturity parameters corresponding to `Model`. See
#'   [MaturityModels()] for available models and required parameters. If `Pars`
#'   is a [stock-class] object, the `Maturity` slot of that stock is returned
#'   (see Details). If `Pars` is non-empty, `Model` is `NULL`, and all values
#'   are non-`NA`, the model is inferred automatically via [FindModel()].
#' @param Model Character or function. Maturity model identifier, or a numeric
#'   index when `Pars` is a list of stocks (see Details). If `NULL` (default),
#'   the model is inferred from `Pars` where possible. See [MaturityModels()]
#'   for available models.
#' @param MeanAtAge Numeric array. Mean maturity at age, with named dimensions
#'   `Sim`, `Age`, and `Year`. Populated automatically during [Populate()] if
#'   `Pars` and `Model` are provided. Default `NULL`.
#' @param MeanAtLength Numeric array. Mean maturity at length, with named
#'   dimensions `Sim`, `Length`, and `Year`. Default `NULL`.
#' @param MeanAtWeight Numeric array. Mean maturity at weight, with named
#'   dimensions `Sim`, `Weight`, and `Year`. Default `NULL`.
#' @param Classes Numeric vector. Age, length, or weight class midpoints.
#'   Default `NULL`.
#' @param Semelparous Logical or numeric array. If `TRUE` or an array of `1`s,
#'   the stock is semelparous (spawns once then dies). Default `FALSE`.
#' @param Misc List. Miscellaneous additional inputs. Default `list()`.
#' @param x A [maturity-class] object for slot accessors, or a [stock-class]
#'   object for `Maturity<-`.
#' @param value For `Maturity<-`: a [maturity-class] object. For
#'   `Semelparous<-`: a logical or numeric array.
#'
#' @details
#' A [maturity-class] object is required for all [stock-class] objects. It
#' defines the relationship between age, length, or weight and reproductive
#' maturity, which is used throughout the operating model for spawning biomass
#' and recruitment calculations.
#'
#' ## Pass-Through Access from a Stock
#'
#' When `Pars` is a [stock-class] object, `Maturity()` acts as an accessor:
#' - `Maturity(stock)` returns `stock@Maturity`.
#' - `Maturity(stocklist, i)` returns `stocklist[[i]]@Maturity` when `Pars`
#'   is a list of stocks and `Model` is a numeric index.
#'
#' ## Specifying Maturity
#'
#' Maturity schedules may be specified in two ways:
#'
#' 1. **Model-based**: provide `Pars` and optionally `Model`. `MeanAtAge`
#'    is populated automatically during [Populate()]. If `Model` is `NULL`
#'    and `Pars` is non-empty with no `NA` values, the model is inferred via
#'    [FindModel()].
#' 2. **Direct array**: provide `MeanAtAge`, `MeanAtLength`, or
#'    `MeanAtWeight` directly with `Pars = list()` (default).
#'
#' ## Semelparity
#'
#' When `Semelparous = TRUE`, the stock spawns once and all mature individuals
#' die after spawning. This modifies the natural mortality schedule applied
#' after the spawning event.
#'
#' ## Attaching to a Stock
#'
#' A `Maturity` object can be attached to a [Stock()] with
#' `Maturity(Stock) <- MyMaturity` and retrieved with `Maturity(Stock)`.
#'
#' Individual slots may be accessed or modified using [Pars()], [Model()],
#' [MeanAtAge()], [MeanAtLength()], [MeanAtWeight()], [Classes()], and
#' [Semelparous()].
#'
#' `r TechManLink()`
#'
#' @return
#' - `Maturity()` returns a [maturity-class] object. If `Pars` is a
#'   [stock-class] object or list of stocks, the corresponding `Maturity`
#'   slot is returned.
#' - `Maturity<-` returns `x` with the `Maturity` slot replaced.
#' - `Semelparous()` returns the `Semelparous` slot from `x`.
#' - `Semelparous<-` returns `x` with the `Semelparous` slot updated.
#'
#' @seealso [maturity-class], [Stock()], [MaturityModels()]
#'
#' @export
Maturity <- function(Pars = list(),
                     Model = NULL,
                     MeanAtAge = NULL,
                     MeanAtLength = NULL,
                     MeanAtWeight = NULL,
                     Classes = NULL,
                     Semelparous = FALSE,
                     Misc = list()) {
  
  if (inherits(Pars, 'stock')) {
    if (inherits(Model, 'numeric') || is.list(Pars)) {
      return(Pars[[Model]]@Maturity)
    }
    return(Pars@Maturity)
  }
  
  object <- methods::new(
    "maturity",
    Pars = Pars,
    Model = Model,
    MeanAtAge = MeanAtAge,
    MeanAtLength = MeanAtLength,
    MeanAtWeight = MeanAtWeight,
    Classes = Classes,
    Semelparous = Semelparous,
    Misc = Misc
  )
  
  if (length(Pars) > 0 &&
      !is.null(names(Pars)) &&
      all(!is.na(unlist(Pars))) &&
      is.null(Model)) {
    object@Model <- FindModel(object)
  }
  
  methods::validObject(object)
  object
}



#' @rdname Maturity
#' @export
Semelparous <- function(x) {
  CheckClass(x, "maturity", "x")
  x@Semelparous
}

#' @rdname Maturity
#' @export
`Semelparous<-` <- function(x, value) {
  CheckClass(x, "maturity", "x")
  x@Semelparous <- value
  methods::validObject(x)
  x
}

#' @rdname Maturity
#' @export
`Maturity<-` <- function(x, value) {
  CheckClass(x, "stock", "x")
  x@Maturity <- value
  methods::validObject(x)
  x
}
