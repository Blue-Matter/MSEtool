#' Bioeconomic
#'
#' Create a [bioeconomic-class] object.
#'
#' A `Bioeconomic` object stores revenue, cost, and investment dynamics
#' used in fleet-level or stock-level bioeconomic analyses.
#'
#' @param Revenue Revenue array.
#' @param Cost Operating cost per unit of effort.
#' @param Investment Cost of adding effort.
#' @param Disinvestment Cost of removing effort.
#' @param Depreciation Depreciation rate of effort units.
#' @param Discount Discount factor.
#' @param Misc Miscellaneous list.
#' 
#' The `Bioeconomic` object is not currently used.
#' 
#' A `Bioeconomic` object can be attached to a [Fleet()] using `Bioeconomic(Fleet) <- MyBioeconomic` and
#' retrieved using `MyBioeconomic <- Bioeconomic(Fleet)`
#'
#' Individual components may be accessed or modified using accessor and
#' replacement functions such as [Revenue()], [Cost()], and [Investment()].
#' 
#' `r TechManLink()`
#'
#' @return A [bioeconomic-class] object.
#'
#' @seealso [Fleet()]
#'
#' @export
Bioeconomic <- function(Revenue = NULL,
                        Cost = NULL,
                        Investment = NULL,
                        Disinvestment = NULL,
                        Depreciation = NULL,
                        Discount = NULL,
                        Misc = list()) {
  
  if (methods::is(Revenue, "fleet"))
    return(Revenue@Bioeconomic)
  
  methods::new(
    "bioeconomic",
    Revenue = Revenue,
    Cost = Cost,
    Investment = Investment,
    Disinvestment = Disinvestment,
    Depreciation = Depreciation,
    Discount = Discount,
    Misc = Misc
  )
}



#' @rdname Bioeconomic 
#' @export
`Bioeconomic<-` <- function(x, value) {
  AssignSlot(x, value, 'Bioeconomic')
}

#' @rdname Bioeconomic 
#' @export
Revenue <- function(x) {
  CheckClass(x, "bioeconomic", "x")
  x@Revenue
}

#' @rdname Bioeconomic 
#' @export
`Revenue<-` <- function(x, value) {
  CheckClass(x, "bioeconomic", "x")
  x@Revenue <- value
  methods::validObject(x)
  x
}


#' @rdname Bioeconomic 
#' @export
Cost <- function(x) {
  CheckClass(x, "bioeconomic", "x")
  x@Cost
}

#' @rdname Bioeconomic 
#' @export
`Cost<-` <- function(x, value) {
  CheckClass(x, "bioeconomic", "x")
  x@Cost <- value
  methods::validObject(x)
  x
}


#' @rdname Bioeconomic 
#' @export
Investment <- function(x) {
  CheckClass(x, "bioeconomic", "x")
  x@Investment
}

#' @rdname Bioeconomic 
#' @export
`Investment<-` <- function(x, value) {
  CheckClass(x, "bioeconomic", "x")
  x@Investment <- value
  methods::validObject(x)
  x
}


#' @rdname Bioeconomic 
#' @export
Disinvestment <- function(x) {
  CheckClass(x, "bioeconomic", "x")
  x@Disinvestment
}

#' @rdname Bioeconomic 
#' @export
`Disinvestment<-` <- function(x, value) {
  CheckClass(x, "bioeconomic", "x")
  x@Disinvestment <- value
  methods::validObject(x)
  x
}


#' @rdname Bioeconomic 
#' @export
Depreciation <- function(x) {
  CheckClass(x, "bioeconomic", "x")
  x@Depreciation
}

#' @rdname Bioeconomic 
#' @export
`Depreciation<-` <- function(x, value) {
  CheckClass(x, "bioeconomic", "x")
  x@Depreciation <- value
  methods::validObject(x)
  x
}


#' @rdname Bioeconomic 
#' @export
Discount <- function(x) {
  CheckClass(x, "bioeconomic", "x")
  x@Discount
}

#' @rdname Bioeconomic 
#' @export
`Discount<-` <- function(x, value) {
  CheckClass(x, "bioeconomic", "x")
  x@Discount <- value
  methods::validObject(x)
  x
}




