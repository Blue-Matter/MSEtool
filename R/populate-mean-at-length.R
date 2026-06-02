# Populates the `MeanAtLength` and `Classes` slot for a given object
PopulateMeanAtLength <- function(object, 
                                 Length=NULL, 
                                 Years=NULL, 
                                 Ages=NULL, 
                                 seed=NULL, 
                                 silent=FALSE) {
  
  if (is.null(object@Model))
    return(object)
  
  if (ParsEmpty(object@Pars)) 
    return(object)
  
  if (is.null(object@Model))
    object@Model <- FindModel(object)
  
  args <- names(formals(object@Model))
  
  if ('Ages' %in% args)
    return(object)
  
  if ('Length' %in% args) 
    CheckRequiredObject(Length, 'length', 'Length')
  
  if (is.null(object@Classes)) 
    object@Classes <- Length@Classes
  
  object@MeanAtLength <- GenMeanAtLength(Model  = object@Model,
                                         Pars   = object@Pars,
                                         Length = object@Classes)
  
 
  
  dd <- dim(object@MeanAtLength)
  if (length(dd)==3) {
    dimnames(object@MeanAtLength) <- list(Sim   = 1:dd[1],
                                          Class = object@Classes,
                                          Year  = Years[1:dd[3]])
  } else {
    dimnames(object@MeanAtLength) <- list(Sim   = 1:dd[1],
                                          Class = object@Classes,
                                          Year  = Years[1:dd[3]],
                                          Area  = 1:dd[4])
  }
  
  object
}

