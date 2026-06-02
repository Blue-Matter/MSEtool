#' The `advicedata` S4 Class
#'
#' Stores management advice outputs generated during a model run or provided
#' as input. Used in the `Advice` slot of a [data-class] object and accessed
#' by [LastTAC()] when retrieving the most recent catch limit.
#'
#' @slot TAC A numeric vector, array, or list of Total Allowable Catch (TAC)
#'   values. When a vector, elements correspond to successive advice years.
#' @slot Effort A numeric vector, array, or list of advised fishing effort
#'   values. Structured analogously to `TAC`.
#' @slot Misc A named list for any additional advice-level metadata (e.g.,
#'   harvest control rule outputs, reference point comparisons).
#'
#' `AdviceData()` creates a new `advicedata` object. 
#'
#' @return `AdviceData()` returns a `advicedata` object.
#' 
#' @seealso [data-class], [Data()], [LastTAC()]
#' @include class-unions.R
#' @name advicedata-class
#' @aliases advicedata
#' @export
setClass(
  "advicedata",
  slots = c(
    TAC    = "num.array.list",
    Effort = "num.array.list",
    Misc   = "list"
  )
)

#' @rdname advicedata-class
#' @export
AdviceData <- function() {
  new('advicedata')
}
