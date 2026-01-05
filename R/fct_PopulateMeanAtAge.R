PopulateMeanAtAge <- function(object, 
                              Ages=NULL, 
                              Years=NULL,
                              Length=NULL) {
  
  if (is.null(object@Model))
    return(object)
  
  if (ParsEmpty(object@Pars)) {
    return(Object)
  }
  
  if (is.null(object@Model)) 
    object@Model <- FindModel(object)
  
  args <- names(formals(object@Model))
  
  if ('Length' %in% args) {
    CheckRequiredObject(Length, 'length', 'Length')
    
    object@MeanAtAge <- GenerateMeanatLength(Model=object@Model,
                                             Pars=object@Pars,
                                             Length=Length@MeanAtAge)
    object@Classes <- Length@Classes
  } else {
    if ('Ages' %in% args) {
      CheckRequiredObject(Ages, 'ages', 'Ages')
      if ('Timing' %in% slotNames(object)) {
        Ages@Classes <- Ages@Classes+object@Timing
      }
    }
    object@MeanAtAge <- GenerateMeanAtAge(Model=object@Model,
                                          Pars=object@Pars,
                                          Ages=Ages@Classes)
    # object@Classes <- Ages@Classes
    
  }
  
  dd <- dim(object@MeanAtAge)
  
  if (length(dd)==3) {
    dimnames(object@MeanAtAge) <- list(Sim=1:dd[1],
                                       Age=Ages@Classes[1:dd[2]],
                                       Year=Years[1:dd[3]])
  } else {
    dimnames(object@MeanAtAge) <- list(Sim=1:dd[1],
                                          Age=Ages@Classes[1:dd[2]],
                                          Year=Years[1:dd[3]],
                                          Area=1:dd[4])
  }
  
  object
}