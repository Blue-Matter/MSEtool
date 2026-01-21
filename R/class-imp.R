
#' Implementation Error Component
#'
#' Internal class used to describe implementation uncertainty associated
#' with a management control (e.g. TAC, effort, or size regulations).
#'
#' @slot Mean Numeric array or list of arrays specifying the mean implemented
#'   value.
#'
#' @slot SD Numeric array or list of arrays specifying implementation
#'   variability.
#'
#' @slot Compliance Numeric array or list of arrays specifying compliance
#'   rates.
#'
#' @slot Error Numeric array or list of arrays specifying realized
#'   implementation error.
#'
#' @slot Misc Miscellaneous additional information.
#'
#' @include class-unions.R
setClass(
  "impslot",
  slots = c(
    Mean       = "num.array.list",
    SD         = "num.array.list",
    Compliance = "num.array.list",
    Error      = "num.array.list",
    Misc       = "list"
  )
)

#' `Imp` Object
#'
#' The `imp` class defines implementation error associated with management
#' advice. It controls how management recommendations (e.g. TAC, effort,
#' size limits) are imperfectly applied in the operating model.
#'
#' @slot Name Name of the implementation model.
#'
#' @slot TAC Implementation error associated with total allowable catch.
#'
#' @slot Effort Implementation error associated with effort controls.
#'
#' @slot Size Implementation error associated with size-based regulations.
#'
#' @slot Misc Miscellaneous additional objects.
#'
#' @seealso [Imp()], [OM()], [Advice()]
#'
#' @include class-unions.R
#' @name ImpClass
#' @export
setClass(
  "imp",
  slots = c(
    Name   = "character",
    TAC    = "impslot",
    Effort = "impslot",
    Size   = "impslot",
    Misc   = "list"
  )
)


#' @rdname ImpClass
#' @export
Imp <- function(object = NULL) {
  if (inherits(object, "om"))
    return(object@Imp)
  
  .Object <- methods::new("imp")
  validObject(.Object)
  .Object
}

setValidity("imp", function(object) {
  # TODO: structural and dimensional checks
  TRUE
})


