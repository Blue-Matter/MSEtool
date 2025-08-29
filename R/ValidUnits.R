#' Return Valid Units
#'
#' Returns the valid units for [Ages()], [Length()], and [Weight()] objects
#'
#' @param x Either an [Ages()], [Length()], or [Weight()] object, or a character
#' string with object class, i.e., one of: `c('ages', 'length', 'weight')` or
#' `c('Ages', 'Length', 'Weight')`
#'
#' @export
#' @examples
#' ValidUnits(Ages())
#'
ValidUnits <- function(x=NULL) {
  ll <- list()
  ll$Age <- c('year', 'half-year', 'quarter', 'month', 'week', 'day')
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



