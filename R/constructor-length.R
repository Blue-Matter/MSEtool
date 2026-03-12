#' Length
#'
#' Construct and manipulate a [length-class] object defining the
#' length-at-age structure associated with a [Stock()] object. A `Length`
#' object is required for all [stock-class] objects.
#'
#' @param Pars Named list of growth parameters corresponding to `Model`. See
#'   [LengthModels()] for available models and required parameters. If `Pars`
#'   is a [stock-class] object, the `Length` slot of that stock is returned.
#'   If `Pars` is non-empty and `Model` is `NULL`, the model is inferred
#'   automatically via [FindModel()].
#' @param Model Character. Growth model identifier. If `NULL` (default), the
#'   model is inferred from `Pars` where possible. See [LengthModels()] for
#'   available models.
#' @param Units Character. Length units (e.g., `"mm"`, `"cm"`). See
#'   [ValidUnits()] for valid options. Default `"mm"`.
#' @param MeanAtAge Numeric array. Mean length at age, with named dimensions
#'   `Sim`, `Age`, and `Year`. If `Pars` and `Model` are provided, this array
#'   is populated automatically during [Populate()]. Default `NULL`.
#' @param CVatAge Numeric. Coefficient of variation of length at age, used to
#'   generate the length distribution within each age class. Default `0.1`.
#' @param Dist Character. Distribution of length-at-age (e.g., `"normal"`).
#'   Default `"normal"`.
#' @param TruncSD Numeric. Truncation of the length-at-age distribution in
#'   standard deviation units. Default `2`.
#' @param Timing Numeric. Timing of length measurement within the time step
#'   (0 = start, 1 = end). Default `0`.
#' @param Random Numeric array. Random effects on growth parameters. Default
#'   `NULL`.
#' @param ALK Numeric array. Age-length key. Default `NULL`.
#' @param Classes Numeric vector. Length class midpoints. Default `NULL`.
#' @param Misc List. Miscellaneous additional inputs. Default `list()`.
#' @param x A [length-class] object for slot accessors, or a [stock-class]
#'   object for `Length<-`.
#' @param value For `Length<-`: a [length-class] object. For `ALK<-`: the new
#'   age-length key array.
#'
#' @details
#' A [length-class] object is required for all [stock-class] objects. It
#' defines the relationship between age and length, which is used throughout
#' the operating model to convert age-based quantities to length-based
#' quantities.
#'
#' ## Specifying Length-at-Age
#'
#' Length schedules may be specified in two ways:
#'
#' 1. **Model-based**: provide `Pars` and optionally `Model`. The `MeanAtAge`
#'    array is populated automatically during [Populate()]. If `Model` is
#'    `NULL` and `Pars` is non-empty with no `NA` values, the model is
#'    inferred via [FindModel()].
#' 2. **Direct array**: provide `MeanAtAge` directly with `Pars = list()`
#'    (default). Any existing values in `MeanAtAge` will be preserved.
#'
#' ## Length Distribution
#'
#' Within each age class, lengths are assumed to follow the distribution
#' specified by `Dist`, with coefficient of variation `CVatAge`, truncated at
#' `TruncSD` standard deviations. This distribution is used to construct the
#' age-length key (`ALK`).
#'
#' ## Attaching to a Stock
#'
#' A `Length` object can be attached to a [Stock()] with
#' `Length(Stock) <- MyLength` and retrieved with `Length(Stock)`.
#'
#' Individual slots may be accessed or modified using [Pars()], [Model()],
#' [Units()], [MeanAtAge()], [CVatAge()], [Dist()], [TruncSD()], [Timing()],
#' [ALK()], and [Classes()].
#'
#' `r TechManLink()`
#'
#' @return
#' - `Length()` returns a [length-class] object. If `Pars` is a [stock-class]
#'   object, the `Length` slot of that stock is returned.
#' - `Length<-` returns `x` with the `Length` slot replaced.
#' - `ALK()` returns the age-length key from `x`.
#' - `ALK<-` returns `x` with the `ALK` slot updated.
#'
#' @seealso [length-class], [Stock()], [LengthModels()], [ValidUnits()],
#'   [Populate()], [FindModel()], [Weight()], [Maturity()], [Pars()],
#'   [MeanAtAge()], [CVatAge()], [Units()], [ALK()], [Classes()]
#'
#' @example man-examples/class-Length.R
#'
#' @export
Length <- function(Pars = list(),
                   Model = NULL,
                   Units = "mm",
                   MeanAtAge = NULL,
                   CVatAge = 0.1,
                   Dist = "normal",
                   TruncSD = 2,
                   Timing = 0,
                   Random = NULL,
                   ALK = NULL,
                   Classes = NULL,
                   Misc = list()) {
  
  if (inherits(Pars, 'stock'))
    return(Pars@Length)
  
  object <- methods::new("length",
                         Pars = Pars,
                         Model = Model,
                         Units = Units,
                         MeanAtAge = MeanAtAge,
                         CVatAge = CVatAge,
                         Dist = Dist,
                         TruncSD = TruncSD,
                         Timing = Timing,
                         Random = Random,
                         ALK  = ALK,
                         Classes = Classes,
                         Misc = Misc)
  
  if (length(Pars) > 0 &&
      !is.null(names(Pars)) &&
      all(!is.na(unlist(Pars))) &&
      is.null(Model))
    object@Model <- FindModel(object)
  
  methods::validObject(object)
  object
}



#' @rdname Length
#' @export
ALK <- function(x) {
  CheckClass(x, "length", "x")
  x@ALK
}

#' @rdname Length
#' @export
`ALK<-` <- function(x, value) {
  CheckClass(x, "length", "x")
  x@ALK <- value
  methods::validObject(x)
  x
}

#' @rdname Length
#' @export
`Length<-` <- function(x, value) {
  CheckClass(x, "stock", "x")
  AssignSlot(x, value, 'Length')
}


