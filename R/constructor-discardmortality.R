#' Discard Mortality
#'
#' Construct and manipulate a [discardmortality-class] object defining the
#' proportion of discarded catch that dies, for use in a [Fleet()] object.
#'
#' @param MeanAtAge Numeric array. Mean discard mortality at age, with named
#'   dimensions `Sim`, `Age`, and `Year`. See `Details` section.
#' @param MeanAtLength Numeric array. Mean discard mortality at length, with
#'   named dimensions `Sim`, `Length`, and `Year`.  See `Details` section.
#' @param Classes Numeric vector. Age or length classes corresponding to the
#'   `Age` or `Length` dimension of `MeanAtAge` or `MeanAtLength`. Default
#'   `NULL`.
#' @param Misc List. Miscellaneous additional inputs. Default `list()`.
#' @param x A [discardmortality-class] object, or a [fleet-class] object for
#'   `DiscardMortality<-`.
#' @param value For `DiscardMortality<-`: a [discardmortality-class] object.
#'
#' @details
#' Discard mortality describes the proportion of discarded catch that dies,
#' defined either at age (`MeanAtAge`) or at length (`MeanAtLength`). Values
#' should be between 0 (all discards survive) and 1 (all discards die).
#' 
#' Discard Mortality objects are optional. If they are not supplied, the 
#' model will assume discard mortality is 0 for all age-classes
#'
#' A `DiscardMortality` object can be attached to a [Fleet()] with
#' `DiscardMortality(Fleet) <- MyDiscardMortality` and retrieved with
#' `DiscardMortality(Fleet)`.
#'
#' Individual slots may be accessed or modified using [MeanAtAge()],
#' [MeanAtLength()], and [Classes()].
#' 
#' `r  AdviceArrayInfo('discardmortality')`
#' 
#' `r TechManLink()`
#'
#' @return
#' - `DiscardMortality()` returns a [discardmortality-class] object. If
#'   `MeanAtAge` is a [fleet-class] object, the `DiscardMortality` slot of
#'   that fleet is returned.
#' - `DiscardMortality<-` returns `x` with the `DiscardMortality` slot
#'   replaced.
#'
#' @seealso [discardmortality-class], [Fleet()], [MeanAtAge()],
#'   [MeanAtLength()], [Classes()], [Selectivity()], [Retention()]
#'
#' @examples
#' d <- DiscardMortality()
#' MeanAtAge(d)
#' Classes(d)
#'
#' @export
DiscardMortality <- function(MeanAtAge    = NULL,
                             MeanAtLength = NULL,
                             Classes      = NULL,
                             Misc         = list()) {
  
  if (methods::is(MeanAtAge, "fleet"))
    return(MeanAtAge@DiscardMortality)
  
  methods::new(
    "discardmortality",
    MeanAtAge    = MeanAtAge,
    MeanAtLength = MeanAtLength,
    Classes      = Classes,
    Misc         = Misc
  )
}


#' @rdname DiscardMortality
#' @export
`DiscardMortality<-`<- function(x,value) {
  AssignSlot(x, value, 'DiscardMortality')
}
