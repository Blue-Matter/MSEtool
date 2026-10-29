#' Check if a Slot is NULL Across All Advice Objects
#'
#' Checks whether a specified slot is `NULL` for all advice objects across
#' all simulations and stocks in an advice list. Typically used to determine
#' whether management advice (e.g. closures, catch limits) has been set.
#'
#' @param AdviceSimList A nested list of advice objects with structure
#'   `[[sim]][[stock]]`, where each element is an S4 object with the slot
#'   specified by `slot_name`.
#' @param slot_name Character; name of the slot to check. Default is
#'   `"Closure"`.
#'
#' @return Logical; `TRUE` if the specified slot is `NULL` for all sim-stock
#'   combinations, `FALSE` if any are non-`NULL`.
#' @keywords internal
.AllAdviceNull <- function(AdviceSimList, slot_name='Closure') {
  purrr::map(AdviceSimList, \(AdviceSim)
             purrr::map(AdviceSim, \(Advice) {
               if (isS4(Advice) && .hasSlot(Advice, slot_name))
               is.null(slot(Advice, slot_name))
             })
  ) |>
    unlist() |>
    all()
}
