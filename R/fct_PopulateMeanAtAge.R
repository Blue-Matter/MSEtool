PopulateMeanAtAge <- function(object, 
                              Ages=NULL, 
                              Years=NULL,
                              Length=NULL) {
  
  
  if (!is.null(object@MeanAtAge)) {
    if (!is.array(object@MeanAtAge)) {
      if (length(object@MeanAtAge) != length(Ages@Classes)) {
        cli::cli_abort(c("x"="If `MeanAtAge` is numeric vector it must be length `nAge` ({.val {nAge(Ages)}})",
                         "i"="Error occured on object class {.val {as.character(class(object))}}")
        )
      }
      object@MeanAtAge <- array(object@MeanAtAge, dim=c(1, nAge(Ages), 1),
                                dimnames = list(
                                  Sim=1,
                                  Age=Ages@Classes,
                                  Year=Years[1]
                                ))
      
    }
    dnames <- dimnames(object@MeanAtAge)
    if (is.null(dnames)) {
      dd <- dim(object@MeanAtAge)
      dimnames(object@MeanAtAge) <- list(
        Sim=1:dd[1],
        Age=Ages@Classes,
        Years=Years[1:dd[3]]
      )
    }
    
  }
  
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
    
    object@MeanAtAge <- GenMeanAtLength(Model=object@Model,
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
    object@MeanAtAge <- GenMeanAtAge(Model=object@Model,
                                          Pars=object@Pars,
                                          Ages=Ages@Classes)
    # object@Classes <- Ages@Classes
    
  }
  
  object
}