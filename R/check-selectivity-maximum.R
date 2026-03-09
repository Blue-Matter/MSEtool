#' Check and Standardise Selectivity-at-Age to a Maximum of 1
#'
#' Checks whether the maximum selectivity-at-age value equals 1 for each
#' simulation, year, and (optionally) area. Where the maximum is below 0.99
#' and non-zero, the slice is rescaled so its maximum equals 1. Returns
#' `MeanAtAge` unchanged if all slices already have a maximum >= 0.99.
#'
#' Selectivity-at-age not peaking at 1 means that apical fishing mortality
#' does not correspond with F-at-age, which can produce unexpected behaviour
#' in downstream calculations. If `alert = TRUE`, a warning is emitted
#' identifying the affected simulations, years, and areas.
#'
#' @param MeanAtAge Numeric array of selectivity-at-age values. Dimensions
#'   `Sim × Age × Year` or `Sim × Age × Year × Area`.
#' @param alert Logical. If `TRUE` (default), emits a warning and lists the
#'   affected simulations, years, and areas.
#'
#' @return `MeanAtAge` with each affected slice rescaled so its maximum
#'   equals 1, with original `dimnames` preserved.
#' @keywords internal
CheckSelectivityMaximum <- function(MeanAtAge, alert=TRUE) {
  dnames <- dimnames(MeanAtAge)
  byArea <- !is.null(dnames[['Area']])
  
  margin    <- if (byArea) c('Sim', 'Year', 'Area') else c('Sim', 'Year')
  MaxValues <- round(apply(MeanAtAge, margin, max), 3)
  
  ind <- MaxValues < 0.99 & MaxValues != 0
  if (!any(ind))
    return(MeanAtAge)
  
  if (alert) {
    cli::cli_alert_warning(
      "Selectivity-at-Age does not reach a maximum of 1. \\
       F-at-Age will not correspond with apical F."
    )
    cli::cli_alert_warning(
      "Standardizing to a maximum of 1. Check the selectivity schedule in the OM."
    )
    
    trunc_vec <- list("vec-trunc"=5)
    sims <- which(apply(ind, 'Sim',  any)) |> cli::cli_vec(trunc_vec)
    yrs  <- which(apply(ind, 'Year', any)) |> cli::cli_vec(trunc_vec)
    
    if (byArea) {
      areas <- which(apply(ind, 'Area', any)) |> cli::cli_vec(trunc_vec)
      cli::cli_alert('Simulations: {.val {sims}}; Years: {.val {yrs}}; Areas: {.val {areas}}')
    } else {
      cli::cli_alert('Simulations: {.val {sims}}; Years: {.val {yrs}}')
    }
  }
  
  # Normalise each Sim x Year (x Area) slice so max == 1.
  # apply() moves the margin dims to position 1, so aperm() restores
  # Age back to dimension 2.
  normalise_slice <- function(x) x / max(x, na.rm=TRUE)
  
  MeanAtAge <- if (byArea) {
    aperm(
      apply(MeanAtAge, c('Sim', 'Year', 'Area'), normalise_slice),
      c(2, 1, 3, 4)
    )
  } else {
    aperm(
      apply(MeanAtAge, c('Sim', 'Year'), normalise_slice),
      c(2, 1, 3)
    )
  }
  
  dimnames(MeanAtAge) <- dnames
  MeanAtAge
}