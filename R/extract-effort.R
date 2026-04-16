#' Extract effort as array or data frame
#'
#' Retrieves the `Effort` slot from a [fleet-class], [effort-class],
#' [obs-class], or [mse-class] object, optionally converting it to a tidy
#' `data.frame`. Users should typically call `Effort(object, df = TRUE)`
#' rather than this function directly.
#'
#' @param object A [fleet-class], [effort-class], [hist-class], [obs-class],
#'   or [mse-class] object.
#' @param df Logical. If `FALSE` (default) the raw `Effort` array slot is
#'   returned. If `TRUE` a tidy `data.frame` with columns `Sim`, `Year`,
#'   `Value`, and `Variable = "Effort"` is returned. Note that `df = TRUE` is
#'   only supported for [hist-class] and [mse-class] objects; for
#'   [fleet-class], [effort-class], and [obs-class] objects the array is
#'   always returned regardless of `df`.
#'
#' @return A numeric array (`df = FALSE`), or a `data.frame` (`df = TRUE`)
#'   with columns `Sim`, `Year`, `Fleet`, `Value`, and `Variable` and in case
#'   of [mse-class] objects, a column `MP`.
#' @return
#' * `df = FALSE` — the raw `Effort` slot (usually an array)
#' * `df = TRUE` — a tidy `data.frame` with columns `Sim`, `Year`, `Period`, 
#'  `Fleet`, `MP` (MSE only), `Value` and `Variable`.
#'   
#' @seealso [Effort()]
#' @keywords internal
extract_effort <- function(object, df=FALSE) {
  
  if (inherits(object, c('fleet', 'effort', 'obs')) || !df)
    return(object@Effort)
  

  if (inherits(object, 'hist')) {
    return(
      .extract_effort(object)
    )
  }
  
  hist <- .extract_effort(object@Hist) |>
    dplyr::mutate(MP = 'Historical')
  
  proj <- .extract_effort(object)
  out <- dplyr::bind_rows(hist, proj)
  class(out) <- c('effort.df', class(out))
  out
  
}

.extract_effort <- function(object) {
  isMSE <- inherits(object, 'mse')
  
  Array2DF(object@Effort) |> dplyr::mutate(Variable="Effort") |>
    dplyr::mutate(Variable = 'Effort',
                  Period   = ifelse(isMSE, 'Projection', 'Historical')) |>
    dplyr::relocate('Sim', 'Year', 'Period') |>
    dplyr::arrange(Sim, Year)
}
