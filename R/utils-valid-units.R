#' Return Valid Units
#'
#' Returns the valid units for [Ages()], [Length()], and [Weight()] objects.
#'
#' @param x An [ages-class], [length-class], or [weight-class] object, or a
#'   character string specifying the class: one of `"ages"`, `"length"`, or
#'   `"weight"` (case-insensitive). If `NULL`, returns a named list of valid
#'   units for all three classes. Default `"ages"`.
#'
#' @return
#' - If `x` is `NULL`: a named list with elements `Age`, `Length`, and
#'   `Weight`, each containing a character vector of valid unit strings.
#' - If `x` is an [ages-class] object or `"ages"`: a character vector of valid
#'   age units: `"year"`, `"half-year"`, `"quarter"`, `"month"`, `"week"`.
#' - If `x` is a [length-class] object or `"length"`: a character vector of
#'   valid length units: `"mm"`, `"cm"`, `"inch"`.
#' - If `x` is a [weight-class] object or `"weight"`: a character vector of
#'   valid weight units: `"g"`, `"kg"`, `"lb"`.
#' - If no match is found: the string `"No units found"`.
#'
#' @seealso [Ages()], [Length()], [Weight()]
#'
#' @examples
#' ValidUnits()           # age units (default)
#' ValidUnits("length")   # length units
#' ValidUnits("weight")   # weight units
#' ValidUnits(NULL)       # all units as a named list
#' ValidUnits(Ages())     # age units from an ages-class object
#'
#' @export
ValidUnits <- function(x='ages') {
  ll <- list()
  ll$Age <- c('year', 'half-year', 'quarter', 'month', 'week')
  ll$Length <- c('mm', 'cm', 'inch')
  ll$Weight <- c('g', 'kg', 'lb')

  if (is.null(x))
    return(ll)

  if (isS4(x)) {
    if (is(x, 'ages'))
      return(ll$Age)
    if (is(x, 'length'))
      return(ll$Length)
    if (is(x, 'weight'))
      return(ll$Weight)
  }

  if (is.character(x)) {
    x <- tolower(x)
    if (x=='ages')
      return(ll$Age)
    if (x=='length')
      return(ll$Length)
    if (x=='weight')
      return(ll$Weight)
  }
  paste('No units found')
}



