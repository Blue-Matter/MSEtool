#' Time steps values
#' 
#' 
#' @name Timesteps
#' @param x A [OM()] object
#' @param value A new value to assign
NULL


#' @rdname Timesteps
#' @export
CurrentYear <- function(x) {
  CheckClass(x)
  x@CurrentYear
}

#' @rdname Timesteps
#' @export
`CurrentYear<-` <- function(x, value) {
  CheckClass(x)
  x@CurrentYear <- value
  x
}



#' @rdname Timesteps
#' @export
nYear <- function(x) {
  # CheckClass(x,)
  x@nYear
}

#' @rdname Timesteps
#' @export
`nYear<-` <- function(x, value) {
  # CheckClass(x)
  x@nYear <- value
  x
}

#' @rdname Timesteps
#' @export
pYear <- function(x) {
  # CheckClass(x)
  x@pYear
}

#' @rdname Timesteps
#' @export
`pYear<-` <- function(x, value) {
  # CheckClass(x)
  x@pYear <- value
  x
}


#' @rdname Timesteps
#' @export
proyears <- function(x) {
  CheckClass(x)
  x@pYear
}

#' @rdname Timesteps
#' @export
`proyears<-` <- function(x, value) {
  CheckClass(x)
  x@pYear <- value
  x
}

#' @rdname Timesteps
#' @export
Seasons <- function(x) {
  if (inherits(x,'hist')) x <- x@OM
  if (inherits(x,'mse')) x <- x@OM
  CheckClass(x)
  x@Seasons
}

#' @rdname Timesteps
#' @export
`Seasons<-` <- function(x, value) {
  CheckClass(x)
  x@Seasons <- value
  x@Years <- CalcYears(x@nYear, x@pYear, x@CurrentYear, x@Seasons)
  x
}

#' @rdname Timesteps
#' @export
Years <- function(x, Period=NULL) {
  if (isS4(x)) {
    if (inherits(x, 'mse') | inherits(x, 'hist'))
      x <- x@OM
    
    # Years <- x@Years
    # if (is.null(Period))
    #   return(Years)
    # if (Period=='All')
    #   return(Years)
    return(CalcYears(x@nYear, x@pYear, x@CurrentYear, x@Seasons, Period))
  }
  
  if (is.list(x))
    purrr::map(x, Years, Period)
  
}



#' @rdname Timesteps
#' @export
SPFrom <- function(x) {
  x@SPFrom
}


#' @rdname Timesteps
#' @export
`SPFrom<-` <- function(x, value) {
  # CheckClass(x)
  x@SPFrom <- value
  x
}
