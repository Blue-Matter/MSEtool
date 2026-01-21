#' Advice 
#'
#' Construct an [Advice()] object defining management advice returned by a
#' management procedure.
#'
#' @param TAC Numeric array specifying total allowable catch. See `Details`
#'
#' @param Effort Numeric array specifying relative or absolute fishing effort. See `Details`
#'
#' @param EffType Character string specifying effort interpretation:
#' `"Rel"` for relative to the last historical year, or `"Abs"` for absolute
#' effort units (in units of [Effort()]).
#'
#' @param Closure Numeric or logical array specifying spatial or temporal closures. See `Details`
#'
#' @param Selectivity A [Selectivity()] object or an `nFleet`-length list of such objects. See `Details`
#'
#' @param Retention A [Retention()] object or an `nFleet`-length list of such objects. See `Details`
#'
#' @param DiscardMortality A [DiscardMortality()] object or an `nFleet`-length list
#' of such objects. See `Details`
#'
#' @param apicalF Numeric array specifying target apical fishing mortality. Not currently used
#'
#' @param Misc Miscellaneous list.
#' 
#' @details
#' 
#' To be added! 
#' 
#' 
#' 
#'
#' @return An [Advice()] object.
#'
#' @seealso [Selectivity()], [Retention()], [DiscardMortality()]
#'
#' @rdname Advice
#' @export
Advice <- function(TAC = NULL,
                   Effort = NULL,
                   EffType = 'Rel',
                   Closure = NULL,
                   Selectivity = new('selectivity'),
                   Retention = new('retention'),
                   DiscardMortality = new('discardmortality'),
                   apicalF = NULL,
                   Misc = list()) {
  
  methods::new("advice",
               TAC = TAC,
               Effort = Effort,
               EffType = EffType,
               Closure = Closure,
               Selectivity = Selectivity,
               Retention = Retention,
               DiscardMortality = DiscardMortality,
               apicalF = apicalF,
               Misc = Misc,
               Log = list())
}



#' Internal Accessors for `Advice`
#'
#' Internal `Get*` and `Set*` functions used to access and modify slots of an
#' [Advice()] object. These functions are not intended to be called directly by
#' users. User-facing access is provided via accessor and assignment functions
#' such as [TAC()], [Effort()], [Selectivity()], etc.
#'
#' The `Get*` functions return the value of the corresponding slot.
#' The `Set*` functions assign a new value to the slot and return the modified
#' object.
#'
#' @param object An [Advice()] object.
#' @param value Value to assign to the slot.
#'
#' @return
#' * `Get*` functions return the value of the requested slot.
#' * `Set*` functions return a modified [Advice()] object.
#'
#' @seealso
#' [Advice()], [TAC()], [Effort()], [Selectivity()], [Retention()], [DiscardMortality()]
#'
#' @name Advice-internal
NULL

#' @rdname Advice-internal
#' @export
GetTAC <- function(object) object@TAC

#' @rdname Advice-internal
#' @export
SetTAC <- function(object, value) {
  object@TAC <- value
  object
}

#' @rdname Advice-internal
#' @export
GetEffort <- function(object) object@Effort

#' @rdname Advice-internal
#' @export
SetEffort <- function(object, value) {
  object@Effort <- value
  object
}

#' @rdname Advice-internal
#' @export
GetEffType <- function(object) object@EffType

#' @rdname Advice-internal
#' @export
SetEffType <- function(object, value) {
  object@EffType <- value
  object
}

#' @rdname Advice-internal
#' @export
GetClosure <- function(object) object@Closure

#' @rdname Advice-internal
#' @export
SetClosure <- function(object, value) {
  object@Closure <- value
  object
}

#' @rdname Advice-internal
#' @export
GetSelectivity <- function(object) object@Selectivity

#' @rdname Advice-internal
#' @export
SetSelectivity <- function(object, value) {
  object@Selectivity <- value
  object
}

#' @rdname Advice-internal
#' @export
GetRetention <- function(object) object@Retention

#' @rdname Advice-internal
#' @export
SetRetention <- function(object, value) {
  object@Retention <- value
  object
}

#' @rdname Advice-internal
#' @export
GetDiscardMortality <- function(object) object@DiscardMortality

#' @rdname Advice-internal
#' @export
SetDiscardMortality <- function(object, value) {
  object@DiscardMortality <- value
  object
}

#' @rdname Advice-internal
#' @export
GetApicalF <- function(object) object@apicalF

#' @rdname Advice-internal
#' @export
SetApicalF <- function(object, value) {
  object@apicalF <- value
  object
}

#' @rdname Advice-internal
#' @export
GetMisc <- function(object) object@Misc

#' @rdname Advice-internal
#' @export
SetMisc <- function(object, value) {
  object@Misc <- value
  object
}
