#' SexPars Object
#'
#' @name SexPars
#' @rdname SexPars
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#' 
#'
#' @export
setClass("sexpars",
         slots=c(SPFrom='array.null',
                 Herm='list.null',
                 SharePar='num.log',
                 Misc='list.null'
         )
)

setValidity('sexpars', isValidObject)


setMethod("initialize", "sexpars", function(.Object,
                                            SPFrom=NULL,
                                            Herm=NULL,
                                            SharePar=TRUE,
                                            Misc=NULL) {
  .Object@SPFrom <- SPFrom
  .Object@Herm <- Herm
  .Object@SharePar <- SharePar
  if (!is.null(Misc)) {
    .Object@Misc <- Misc
  } else {
    .Object@Misc <- list()
    .Object@Misc$Stock <- c('Depletion', 'SRR')
    .Object@Misc$Fleet <- c('Effort', 'Distribution')
    .Object@Misc$Obs <- TRUE
    .Object@Misc$Imp <- TRUE
  }
  
  #   .Object@Created <- Sys.time()
  .Object
})

#' @describeIn SexPars Create a new `SexPars` object
#' @export
SexPars <- function(SPFrom=NULL,
                    Herm=list(),
                    SharePar=TRUE,
                    Misc=NULL) {
  
  if (methods::is(SPFrom, 'om'))
    return(SPFrom@SexPars)
  
  methods::new('sexpars',
               SPFrom=SPFrom,
               Herm=Herm,
               SharePar=SharePar,
               Misc=Misc)
}


#' @describeIn SexPars Assign an `SexPars` object to an [OM()] object
#' @param x An [OM()] object
#' @param value A `SexPars` object to assign to `x`
#' @export
`SexPars<-` <- function(x, value) {
  assignSlot(x, value, 'SexPars')
}
