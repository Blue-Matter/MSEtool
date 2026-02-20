#' Effort 
#' 
#' Construct a [effort-class] object for a [Fleet()] object.
#' 
#' The `Effort` object stores historical fishing effort and
#' associated spatial structure for [Fleet()] object. Effort may be supplied
#' directly as an array or generated stochastically from a data frame.
#' 
#' @details
#' Effort represents total fishing activity prior to spatial allocation.
#' 
#' It is only required for spatial operating models.
#'
#' When `Distribution` is supplied, effort is partitioned explicitly
#' across areas. Otherwise, spatial allocation is derived internally
#' using spatial utility calculations and fleet behaviour.
#'
#' If `Effort` is supplied as a correctly structured `data.frame`,
#' it will be used by [GenHistEffort()] to generate stochastic
#' historical effort.
#' 
#' A `Effort` object can be attached to a [Fleet()] using `Effort(Fleet) <- MyEffort` and
#' retrieved using `MyEffort <- Length(Fleet)`
#'
#' Individual components may be accessed or modified using accessor and
#' replacement functions such as [Effort()], [Units()], and [Distribution()].
#' 
#' `r TechManLink()`
#' 
#' @seealso [Fleet()], [GenHistEffort()]
#'
#' @include class-unions.R
#' 
#' @return An [effort-class] object
#'
#' @name Effort
#' @export
Effort <- function(Effort       = NULL,
                   Units        = NULL,
                   Distribution = NULL,
                   Targeting    = NULL,
                   Maximum      = NULL,
                   Misc         = list()) {
  
  if (inherits(Effort, 'fleet')) {
    return(Effort@Effort)
  }
  
  if (inherits(Effort, 'effort')) {
    return(Effort@Effort)
  }
  
  if (inherits(Effort, 'hist')) {
    return(Effort@Effort)
  }
  
  if (inherits(Effort, 'mse')) {
    return(Effort@Effort)
  }
  
  methods::new(
    "effort",
    Effort       = Effort,
    Units        = Units,
    Distribution = Distribution,
    Targeting    = Targeting,
    Maximum      = Maximum,
    Misc         = Misc
  )
}

#' @rdname Effort
#' @export
`Effort<-` <- function(x, value) {
  AssignSlot(x,value,'Effort')
}


#' @rdname Effort
#' @export
Distribution <- function(x) {
  CheckClass(x, "effort", "x")
  x@Distribution
}

#' @rdname Effort
#' @export
`Distribution<-` <- function(x, value) {
  CheckClass(x, "effort", "x")
  x@Distribution <- value
  methods::validObject(x)
  x
}

#' @rdname Effort
#' @export
Targeting <- function(x) {
  CheckClass(x, "effort", "x")
  x@Targeting
}

#' @rdname Effort
#' @export
`Targeting<-` <- function(x, value) {
  CheckClass(x, "effort", "x")
  x@Targeting <- value
  methods::validObject(x)
  x
}

#' @rdname Effort
#' @export
Maximum <- function(x) {
  CheckClass(x, "effort", "x")
  x@Maximum
}

#' @rdname Effort
#' @export
`Maximum<-` <- function(x, value) {
  CheckClass(x, "effort", "x")
  x@Maximum <- value
  methods::validObject(x)
  x
}


