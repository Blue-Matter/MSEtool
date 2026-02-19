#' Access Slots from a Compatible Object
#'
#' Generic accessor functions to retrieve named slots from a compatible S4 object.
#' 
#' These are common to several different S4 classes
#'
#' @param x An S4 object with the corresponding slot.
#'
#' @return The value stored in the named slot of `x`.
#'
#' @examples
#' MyLength <- Length()
#' MeanAtAge(MyLength)
#'
#' @name Access
NULL


#' @rdname Access
#' @export
AC <- function(x) {
  x@AC
}

#' @rdname Access
#' @export
`AC<-` <- function(x, value) {
  AssignSlot(x, value, 'AC')
}




#' @rdname Access
#' @export
Classes <- function(x) {
  validclasses <- c('ages',
                    'length',
                    'weight', 
                    'maturity',
                    'fecundity',
                    'selectivity',
                    'discardmortality',
                    'retention'
  )
  CheckClass(x, c('stock', 'StockList', validclasses), 'x')
  
  if (inherits(x, validclasses))
    return(x@Classes)
  
  if (inherits(x, 'stock'))
    return(x@Ages@Classes)
  
  if (inherits(x, 'StockList'))
    return(
      purrr::map(x, Recall)
    )
}

#' @rdname Access
#' @export
`Classes<-` <- function(x, value) {
  AssignSlot(x, value, 'Classes')
}



#' @rdname Access
#' @export
CVatAge <- function(x) {
  AccessSlot(x,'CVatAge')
}


#' @rdname Access
#' @export
`CVatAge<-` <- function(x, value) {
  AssignSlot(x, value, 'CVatAge')
}


#' @rdname Access
#' @export
Dist <- function(x) {
  AccessSlot(x,'Dist')
}

#' @rdname Access
#' @export
MeanAtAge <- function(x) {
  AccessSlot(x,'MeanAtAge')
}

#' @rdname Access
#' @export
`MeanAtAge<-` <- function(x, value) {
  AssignSlot(x, value, 'MeanAtAge')
}

#' @rdname Access
#' @export
MeanAtLength <- function(x) {
  AccessSlot(x,'MeanAtLength')
}

#' @rdname Access
#' @export
`MeanAtLength<-` <- function(x, value) {
  AssignSlot(x, value, 'MeanAtLength')
}


#' @rdname Access
#' @export
MeanAtWeight <- function(x) {
  AccessSlot(x,'MeanAtWeight')
}

#' @rdname Access
#' @export
`MeanAtWeight<-` <- function(x, value) {
  AssignSlot(x, value, 'MeanAtWeight')
}



#' @rdname Access
#' @export
Misc <- function(x) {
  AccessSlot(x,'Misc')
}

#' @rdname Access
#' @export
`Misc<-` <- function(x, value) {
  AssignSlot(x, value, 'Misc')
}


#' @rdname Access
#' @export
Model <- function(x) {
  AccessSlot(x,'Model')
}

#' @rdname Access
#' @export
`Model<-` <- function(x, value) {
  AssignSlot(x, value, 'Model')
}

#' @rdname Access
#' @export
Name <- function(x) {
  AccessSlot(x,'Name')
}

#' @rdname Access
#' @export
`Name<-` <- function(x, value) {
  AssignSlot(x, value, 'Name')
}


#' @rdname Access
#' @export
nSim <- function(x) {
  if (isS4(x)) {
    slots <- slotNames(x)
    if ('nSim' %in% slots)
      return(x@nSim)
    return(x@OM@nSim)
  }
  
  if (is.list(x)) {
    return(purrr::map(x, nSim) |> unlist())
  }
  
  dnames <- dimnames(x)
  if (!is.null(dnames))
    return(length(dnames[['Sim']]))
}


#' @rdname Access
#' @export
`nSim<-` <- function(x, value) {
  AssignSlot(x, value, 'nSim')
}



#' @rdname Access
#' @export
nArea <- function(x, st=1) {
  if (inherits(x, 'hist') || inherits(x,'mse')) {
    x <- x@OM 
  }
  
  if (inherits(x, 'data')) {
    return(x@nArea)
  }
  
  if (inherits(x, 'om')) {
    stock <- x@Stock
    if (is.list(stock)) {
      stock <- stock[[st]]
    }
    dd <- dim(stock@Spatial@UnfishedDist)
    d1 <- length(stock@Spatial@UnfishedDist)
  } else {
    stock <- x
    dd <- dim(stock@Spatial@UnfishedDist)
    d1 <- length(stock@Spatial@UnfishedDist)
  }
  
  if (length(dd)<1) {
    if (length(d1)>0)
      return(d1)
    return(1)
  }
  
  nms <- names(dimnames(stock@Spatial@UnfishedDist))
  as.numeric(dd[which(nms=='Area')])
  
}

#' @rdname Access
#' @export
nAge <- function(x, st=NULL) {
  if (inherits(x, 'hist')) {
    x <- x@OM 
  }
  
  if (inherits(x, 'mse')) {
    x <- x@OM 
  }
  
  if (inherits(x, 'om')) {
    stock <- x@Stock
    if (is.list(stock)) {
      if (!is.null(st)) {
        return(
          length(stock[[st]]@Ages@Classes)
        )
      } else {
        return(lapply(stock, nAge))
      }
      
    }
  }
  if (inherits(x, 'stock')) {
    return(length(x@Ages@Classes))
  }
  
}

#' @rdname Access
#' @export
nStock <- function(object) {
  
  CheckClass(object, c('om', 'hist', 'mse'), 'object')
  
  if (inherits(object,'om')) {
    return(length(object@Stock))
  }
  
  if (inherits(object,'hist')) {
    return(length(object@OM@Stock))
  }
  
  if (inherits(object,'mse')) {
    return(length(object@OM@Stock))
  }
  
}

#' @rdname Access
#' @export
nFleet <- function(object) {
  CheckClass(object, c('om', 'hist', 'mse', 'data'), 'object')
  
  if (inherits(object,'om')) {
    fleet <- object@Fleet
    if (is.null(fleet))
      return(0)
    if (inherits(fleet, 'fleet'))
      return(1)
    if (is.list(fleet[[1]]))
      return(length(fleet[[1]]))
    if(isS4(fleet[[1]])) {
      dd <- dim(object@Fleet[[1]]@Selectivity@MeanAtAge)
      return(dd[3])
    }
  }
  
  if (inherits(object, 'data')) {
    return(
      lapply(list(object@Landings@Value,
                  object@Discards@Value,
                object@Survey@Value,
                object@CPUE@Value,
                NULL), ncol) |> 
      unlist() |> max()
    )
  }
  
  return(
    dim(object@LandingsAtAge[[1]])[[4]]
  )
  
}


#' @rdname Access
#' @export
Pars <- function(x) {
  AccessSlot(x,'Pars')
}

#' @rdname Access
#' @export
`Pars<-` <- function(x, value) {
  AssignSlot(x, value, 'Pars')
}

#' @rdname Access
#' @export
Period <- function(x) {
  AccessSlot(x,'Period')
}

#' @rdname Access
#' @export
`Period<-` <- function(x, value) {
  AssignSlot(x, value, 'Period')
}


#' @rdname Access
#' @export
Random <- function(x) {
  AccessSlot(x,'Random')
}


#' @rdname Access
#' @export
Reference <- function(x) {
  AccessSlot(x,'Reference')
}

#' @rdname Access
#' @export
`Reference<-` <- function(x, value) {
  AssignSlot(x, value, 'Reference')
}


#' @rdname Access
#' @export
Seed <- function(x) {
  AccessSlot(x,'Seed')
}

#' @rdname Access
#' @export
`Seed<-` <- function(x, value) {
  AssignSlot(x, value, 'Seed')
}

#' @rdname Access
#' @export
SD <- function(x) {
  AccessSlot(x,'SD')
}

#' @rdname Access
#' @export
`SD<-` <- function(x, value) {
  AssignSlot(x, value, 'SD')
}

#' @rdname Access
#' @export
Timing <- function(x) {
  AccessSlot(x,'Timing')
}


#' @rdname Access
#' @export
TruncSD <- function(x) {
  AccessSlot(x,'TruncSD')
}

#' @rdname Access
#' @export
`TruncSD<-` <- function(x, value) {
  AssignSlot(x, value, 'TruncSD')
}


#' @rdname Access
#' @export
Units <- function(x) {
  AccessSlot(x,'Units')
}

#' @rdname Access
#' @export
`Units<-` <- function(x, value) {
  AssignSlot(x, value, 'Units')
}

# ---- Helpers -----

AccessSlot <- function(x, slotname) {
  CheckClass(slotname, 'character', 'slotname')
  if (!isS4(x))
    cli::cli_abort("`x` is not an S4 object")
  
  if (!slotname %in% slotNames(x))
    cli::cli_abort("Slot {.val {slotname}} is not found in object class {.val {class(x)}}")
  
  slot(x, slotname)
}

AssignSlot <- function(x, value, slot) {
  nms <- slotNames(x)
  if (!slot %in% nms) {
    cli::cli_alert_warning('Slot {.code {slot}} not found in object class: {.code {class(x# )}}')
    return()
  }
  slot(x, slot) <- value
  methods::validObject(x)
  x
}