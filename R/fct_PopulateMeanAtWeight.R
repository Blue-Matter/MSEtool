PopulateMeanAtWeight <- function(object, 
                                 Weight=NULL, 
                                 Years=NULL, 
                                 Ages=NULL, 
                                 nsim=NULL,
                                 seed=NULL, 
                                 silent=FALSE) {
  
  if (is.null(object@Model))
    return(object)
  
  if (ParsEmpty(object@Pars)) {
    return(Object)
  }
  
  
  if (ParsNotEmpty(object@Pars)) 
    
    if (is.null(object@Model))
      object@Model <- FindModel(object)
  
  args <- names(formals(object@Model))
  
  if ('Ages' %in% args)
    return(object)
  
  if ('Length' %in% args) {
    CheckRequiredObject(Length, 'length', 'Length')
  }
  object@MeanAtWeight <- GenerateMeanatWeight(Model=object@Model,
                                              Pars=object@Pars,
                                              Weight=Weight@Classes)
  
  object@Classes <- Weight@Classes
  
  dd <- dim(object@MeanAtWeight)
  if (length(dd)==3) {
    dimnames(object@MeanAtWeight) <- list(Sim=1:dd[1],
                                          Class=Weight@Classes,
                                          Year=Years[1:dd[3]])
  } else {
    dimnames(object@MeanAtWeight) <- list(Sim=1:dd[1],
                                          Class=Weight@Classes,
                                          Year=Years[1:dd[3]],
                                          Area=1:dd[4])
  }
  
  object
}