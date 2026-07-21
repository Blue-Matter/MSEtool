#' Join Two Objects Along the Year Dimension
#'
#' Recursively combines two objects of the same structure by appending
#' `object2` after `object1` along the year dimension. Handles S4 objects,
#' lists, named arrays, and numeric vectors. Intended for joining early and
#' late year segments of MSEtool data objects (e.g. historical and projection
#' periods).
#'
#' @param object1 The earlier object. Must have years that precede those in
#'   `object2`. Can be an S4 object, a `list`, a named `array` with a `"Year"`
#'   dimension, or a `numeric` vector.
#' @param object2 The later object with the same structure as `object1`.
#'   Its years must be strictly greater than those in `object1`.
#'
#' @return An object of the same class and structure as `object1`, with
#'   `object2` appended along the year dimension. Non-array, non-numeric,
#'   and non-list slots or elements are returned unchanged from `object1`.
#'
#' @details
#' `JoinYear` recurses through the structure of `object1` and `object2`
#' in parallel, dispatching on the type of each element:
#'
#' - **S4 objects** — each slot is processed recursively via `Recall()`. If
#'   a slot is `NULL` on `object1` but populated on `object2` (e.g. `Advice`
#'   during the historical period, before any MP has run), the joined
#'   result is `object2`'s value rather than `NULL`.
#' - **Lists** — each element is processed recursively via `Recall()`.
#' - **Named arrays with a `"Year"` dimension** — joined along the year
#'   axis using [abind::abind()]. An error is thrown if years are not
#'   strictly increasing across `object1` and `object2`.
#' - **Numeric vectors** — concatenated with `c()`. If all values exceed
#'   1000 (i.e. are likely calendar years), strict monotonicity is checked
#'   and an error is thrown if the combined sequence is not increasing.
#' - **All other types** — returned unchanged from `object1`.
#'
#' @section Error handling:
#' If years are not strictly increasing across the two objects,
#' [cli::cli_abort()] is called with a diagnostic message showing the
#' year values from both objects and a hint that the arguments may be
#' in the wrong order.
#'
#' @seealso [abind::abind()]
#'
#'
#' @export
JoinYear <- function(object1, object2) {
  

  if (isS4(object1)) {
    slots <- slotNames(object1)
    for (s in slots) {
      val1 <- slot(object1, s)
      val2 <- slot(object2, s)

      if (is.null(val1)) {
        # Nothing on the object1 (early) side to prepend -- e.g. `Advice`
        # is unset during the historical period, since no MP has run yet.
        # The joined result is just whatever object2 (later) has, not NULL.
        if (!is.null(val2))
          slot(object1, s) <- val2
      } else {
        slot(object1, s) <- Recall(val1, val2)
      }
    }
    return(object1)
  }
  
  if (is.array(object1)) {
    dnames <- dimnames(object1)

    if (!is.null(dnames) && "Year" %in% names(dnames)) {
      YrInd <- which(names(dnames)=='Year')

      # object1 contributes no historical rows -- nothing to prepend, and its
      # other dimensions (e.g. Fleet) may be a leftover shape from whichever
      # MP's data supplied the historical segment, with no bearing on
      # object2's own shape (e.g. an MP issuing a single combined TAC vs.
      # one issuing per-fleet TAC). Just use object2 as-is.
      if (dim(object1)[YrInd] == 0)
        return(object2)

      years1 <- as.numeric(dnames$Year)
      years2 <- as.numeric(dimnames(object2)$Year)

      if (!all(diff(c(years1, years2)) > 0))
        cli::cli_abort(c('x'='Years are not increasing. Did you pass early years as `object1`?',
                         'i'="`Years 1` = {.val {years1}}",
                         'i'="`Years 2` = {.val {years2}}"
        ))

      return(
        abind::abind(object1, object2, along=YrInd, use.dnns=TRUE)
            )

    }
    return(object1)
  }

  # This shortcut must run after the array branch: `all(is.na(x))` is
  # vacuously TRUE for a zero-length x (e.g. a "Year"-dimensioned array
  # subset to a period with no rows), which would otherwise return the
  # empty object1 unchanged and silently drop object2's values.
  if (all(is.na(object1)))
    return(object1)

  if (!length(object1))
    return(object1)

  if (is.list(object1)) {
    for (j in seq_along(object1)) {
      if (!is.null(object1[[j]]))
        object1[[j]] <- Recall(object1[[j]], object2[[j]])
    }
    return(object1)
  }

  if (is.numeric(object1)) {
    if (length(object1)==1 && object1 == object2)
      return(object1)
    
    out <- c(object1, object2)
    if (all(out > 1000)) { # years (probably)
      if (!all(diff(out) > 0)) 
        cli::cli_abort(c('x'='Years are not increasing. Did you pass early years as `object1`?',
                         'i'="`object1` = {.val {object1}}",
                         'i'="`object2` = {.val {object2}}"
        ))  
      return(out)
      
    }
    
    return(c(object1, object2))
  }
  
  object1
  
}
