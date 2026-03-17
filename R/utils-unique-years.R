#' Identify Unique Year Slices in an Array
#'
#' Returns the indices of years whose values differ from the preceding year,
#' by comparing adjacent slices along the `Year` dimension. Used to detect
#' whether an array is constant over time, which can be exploited to skip
#' redundant computation.
#'
#' Returns `NULL` if the array has no `Year` dimension, and `1` if the `Year`
#' dimension has length 1.
#'
#' @param array A named-dimnames array with an optional `"Year"` dimension.
#'
#' @return An integer vector of year indices where the slice differs from the
#'   previous year, `1` if there is only one year, or `NULL` if no `Year`
#'   dimension exists. Throws an error if `array` is not an array.
#' @keywords internal
UniqueYears <- function(array) {
  if (!is.array(array))
    cli::cli_abort('{.arg array} must be an array.')
  
  dnames <- dimnames(array)
  year_dim <- which(names(dnames) == 'Year')
  
  if (!length(year_dim))
    return(NULL)
  
  nTS <- dim(array)[year_dim]
  if (nTS == 1)
    return(1L)
  
  changed <- c(
    TRUE,
    vapply(seq_len(nTS)[-1], function(i) {
      slice_curr <- abind::asub(array, i,     year_dim)
      slice_prev <- abind::asub(array, i - 1, year_dim)
      any(round(slice_curr, 4) != round(slice_prev, 4))
    }, logical(1))
  )
  
  # Seasonal - causes problems in Extend  
  # if (any(as.numeric(dnames$Year) %% 1 != 0)) {
  #   ind <- as.numeric(dnames$Year) %% 1 == 0
  #   nSeason <- min(which(ind[-1]))
  #   changed[seq_len(nSeason)] <- TRUE
  # }
    
  which(changed)

}

#' Check Whether an Array is Constant Across Years
#'
#' A convenience wrapper around [UniqueYears()] that returns a logical
#' indicating whether all year slices in `array` are identical, or optionally
#' returns the unique year indices directly. Non-array inputs are treated as
#' constant (returns `TRUE`).
#'
#' @param array A named-dimnames array, or any non-array object.
#' @param logical Logical. If `TRUE` (default), returns a single `TRUE`/`FALSE`
#'   indicating whether the array is constant across years. If `FALSE`, returns
#'   the integer vector of unique year indices from [UniqueYears()].
#'
#' @return If `logical = TRUE`, a single logical: `TRUE` if all year slices are
#'   identical (or if `array` is not an array), `FALSE` otherwise. If
#'   `logical = FALSE`, the integer vector returned by [UniqueYears()].
#' @keywords internal
IdenticalYears <- function(array, logical=TRUE) {
  if (!is.array(array))
    return(TRUE)
  
  unique <- UniqueYears(array)
  
  if (!logical)
    return(unique)
  
  is.null(unique) || length(unique) == 1L
}