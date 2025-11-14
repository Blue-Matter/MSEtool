ExtendStockObject <- function(object, nSim, nAges, Years) {
  nms <- slotNames(object)
  
  if ("Pars" %in% nms)
    if (!all(unlist(lapply(object@Pars, is.na)))) {
      object@Pars <- lapply(object@Pars, ArrayExpand, nSim, nAges, Years)
    }
  
  if ("RelRecFun" %in% nms) {
    if (!is.null(object@Model) && inherits(object@Model, 'character')) {
      if (is.null(object@RelRecFun)) {
        mod <- get(paste0(object@Model, 'RelRec'))
        class(mod) <- 'function'
        object@RelRecFun <- mod
      }
    }
  }
  
  if ("Model" %in% nms) {
    if (!is.null(object@Model)) {
      if (inherits(object@Model, 'character')) {
        mod <- get(object@Model)
        class(mod) <- 'function'
        object@Model <- mod
      }
    }
  }
  
  if ("MeanAtAge" %in% nms)
    object@MeanAtAge <- ArrayExpand(object@MeanAtAge, nSim, nAges, Years)
  
  if ("CVatAge" %in% nms)
    object@CVatAge <- ArrayExpand(object@CVatAge, nSim, nAges, Years)
  
  if ("MeanAtLength" %in% nms)
    object@MeanAtLength <- ArrayExpand(object@MeanAtLength, nSim, nAges=NULL, Years=NULL)
  
  if ("MeanAtWeight" %in% nms)
    object@MeanAtWeight <- ArrayExpand(object@MeanAtWeight, nSim, nAges, Years)
  
  if ("Semelparous" %in% nms)
    object@Semelparous  <- ArrayExpand(object@Semelparous, nSim, nAges, Years)
  
  if ("Random" %in% nms)
    object@Random <- ArrayExpand(object@Random, nSim, nAges, Years)
  
  # if ("ASK" %in% nms) # too large in some cases
  #   object@ASK <- ArrayExpand(object@ASK, nSim, nAges, Years)
  
  object
}