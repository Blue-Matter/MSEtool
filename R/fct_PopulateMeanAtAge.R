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
      if ('Timing' %in% slotNames(object) && !is.null(object@Timing)) {
        Ages@Classes <- Ages@Classes+object@Timing
      }
    }
    object@MeanAtAge <- GenerateMeanAtAge(Model=object@Model,
                                          Pars=object@Pars,
                                          Ages=Ages@Classes)
    # object@Classes <- Ages@Classes
    
  }
  
  object
}