
UpdateRetention <- function(ProjSim, MPAdviceList, MPAdviceList_Previous, Year, YearsProj) {
  UpdateSelectivity(ProjSim, MPAdviceList, MPAdviceList_Previous, Year, YearsProj, 'Retention')
}


UpdateSelectivity <- function(ProjSim, MPAdviceList, MPAdviceList_Previous, 
                              Year, YearsProj,
                              type=c('Selectivity', 'Retention')) {
  type <- match.arg(type, c('Selectivity', 'Retention'))

  FleetNames <- FleetNames(ProjSim@OM)
  Complexes <- ProjSim@OM@Complexes
  YearsProj <- YearsProj[YearsProj>=Year]
  for (complex in seq_along(MPAdviceList)) {
    stocks <- Complexes[[complex]]
    MPAdvice <- MPAdviceList[[complex]]
    MPAdvicePrevious <- MPAdviceList_Previous[[complex]]
    
    if (SelectivityUnchanged(MPAdvice, MPAdvicePrevious, type))
      next()
    
    SelectivityList <- slot(MPAdvice, type)
    if (!is.list(SelectivityList)) 
      SelectivityList <- list(SelectivityList)
    
    if (length(SelectivityList)>1 && length(SelectivityList)!=length(FleetNames)) {
      if (type =='Selectivity')
        cli::cli_abort('`Advice@Selectivity` must be a `Selectivity()` object or an `nFleet` long list of `Selectivity()` objects')
      cli::cli_abort('`Advice@Retention` must be a `Retention()` object or an `nFleet` long list of `Retention()` objects')
    } 
  
    for (st in stocks) {
      Ages <- ProjSim@OM@Stock[[st]]@Ages
      
      for (fl in seq_along(FleetNames)) {
        if (length(SelectivityList)>1) {
          Selectivity <- SelectivityList[[fl]]
        } else {
          Selectivity <- SelectivityList[[1]]
        }
        
        if (length(Selectivity@Pars)) {
          ProjSim <- ProcessAdvice_SelectivityPars(Selectivity, ProjSim, YearsProj, Ages, type, st, fl)
        } else if (length(Selectivity@MeanAtAge)) {
          ProjSim <- ProcessAdvice_SelectivityMeanAtAge(Selectivity, ProjSim, YearsProj, Ages@Classes, type, st, fl)
        } else if (length(Selectivity@MeanAtLength)) {
          ProjSim <- ProcessAdvice_SelectivityMeanAtLength(Selectivity, ProjSim, YearsProj, Ages, type, st, fl)
        } else if (length(Selectivity@MeanAtWeight)) {
          ProjSim <-  ProcessAdvice_SelectivityMeanAtWeight(Selectivity, ProjSim, YearsProj, Ages, type, st, fl)
        } else {
          if (type=='Selectivity')
            cli::cli_abort("Could not process `Advice@Selectivity`", .internal=TRUE)
          cli::cli_abort("Could not process `Advice@Retention`", .internal=TRUE)
        }
      }
    }
  }
  ProjSim
}

SelectivityUnchanged <- function(MPAdvice, MPAdvicePrevious, type) {
  !is.null(MPAdvicePrevious) && 
    (IdenticalS4(slot(MPAdvice, type), slot(MPAdvicePrevious, type))) ||
    EmptyObject(slot(MPAdvice, type))
}

ProcessAdvice_SelectivityMeanAtAge <- function(Selectivity, ProjSim, YearsProj, AgeClasses, type, st, fl) {
  
  if (length(Selectivity@MeanAtAge) != length(AgeClasses))
    cli::cli_abort(c('x'="`Advice@Selectivity@MeanAtAge` must be a numeric vector with length `nAges` ({.val {length(AgeClasses)}})",
                     'i'='Currently length {length(Selectivity@MeanAtAge)}'),
                   call=NULL
    )
  
  MeanAtAge <- array(Selectivity@MeanAtAge, dim=c(length(AgeClasses), 1),
                     dimnames = list(
                       Age=AgeClasses,
                       Year=YearsProj[1]
                     ))
  
  dimnames(MeanAtAge) <- list(Age=Ages@Classes,
                              Year=YearsProj[1]) 
  
  ArrayFill(slot(ProjSim@OM@Fleet[[st]],type)@MeanAtAge[,,fl]) <- MeanAtAge |>
    ExtendYears(YearsProj)
  
  ProjSim
}

ProcessAdvice_SelectivityMeanAtLength <- function(Selectivity, ProjSim, YearsProj, Ages, type, st, fl) {
  
  Length <- ProjSim@OM@Stock[[st]]@Length 
  if (!is.null(Selectivity@Classes))
    Length@Classes <- Selectivity@Classes
  if (is.null(Length@Classes))
    Length@Classes <- slot(ProjSim@OM@Fleet[[st]], type)@Classes[[fl]]
  if (is.null(Length@Classes))
    Length@Classes <- ProjSim@OM@Fleet[[st]]@Selectivity@Classes[[fl]]
  
  Selectivity@Classes <- Length@Classes
  
  if (length(Selectivity@MeanAtLength) != length(Selectivity@Classes)) 
    cli::cli_abort(c('x'='=`Selectivity@MeanAtLength` must be the same length as `Selectivity@Classes`'))
  
  Length@MeanAtAge <- Length@MeanAtAge |> ArraySubsetYear(YearsProj)
  Length@CVatAge <- Length@CVatAge |> ArraySubsetYear(YearsProj)
  SDatAge <- ArrayMultiply(Length@MeanAtAge, Length@CVatAge)
  
  if (dim(Length@ASK)[2] != length(Length@Classes)) {
    Length@ALK <- CalcAgeSizeKey_(Length@MeanAtAge, 
                                  SDatAge, 
                                  Length@Classes, 
                                  Length@TruncSD, 
                                  Length@Dist)
  } else {
    Length@ALK <- ArraySubsetYear(Length@ALK, YearsProj)
  }
  
  Selectivity@MeanAtLength <- array(Selectivity@MeanAtLength, 
                                    dim=c(1, length(Selectivity@MeanAtLength),1),
                                    dimnames=list(
                                      Sim=1,
                                      Class=Selectivity@Classes,
                                      Year=YearsProj[1])
  ) |> ExtendYears(YearsProj)
  
  if (type=='Retention') {
    Selectivity <- MeanAtLength2MeanAtAge(Selectivity, Length)   
    
  } else {
    Selectivity <- MeanAtLength2MeanAtAge(Selectivity, Length, max1=TRUE)                                
  }
  Selectivity@MeanAtAge <- Selectivity@MeanAtAge |> ExtendYears(YearsProj) 
  
  MeanAtLength <- Selectivity@MeanAtLength |> DropDimension('Sim')
  MeanAtAge <- Selectivity@MeanAtAge |> DropDimension('Sim')
  
  ArrayFill(slot(ProjSim@OM@Fleet[[st]],type)@MeanAtAge[,,fl]) <- MeanAtAge
  ArrayFill(slot(ProjSim@OM@Fleet[[st]],type)@MeanAtLength[,,fl]) <- MeanAtLength
  ProjSim
}

ProcessAdvice_SelectivityMeanAtWeight <- function(Selectivity, ProjSim, YearsProj, Ages, type, st, fl) {
  
  Weight <- ProjSim@OM@Stock[[st]]@Weight
  if (!is.null(Selectivity@Classes))
    Weight@Classes <- Selectivity@Classes
  if (is.null(Weight@Classes))
    Weight@Classes <- slot(ProjSim@OM@Fleet[[st]], type)@Classes[[fl]]
  if (is.null(Weight@Classes))
    Weight@Classes <- ProjSim@OM@Fleet[[st]]@Selectivity@Classes[[fl]]
  
  Selectivity@Classes <- Weight@Classes
  
  
  if (length(Selectivity@MeanAtWeight) != length(Selectivity@Classes)) 
    cli::cli_abort(c('x'='=`Selectivity@MeanAtWeight` must be the same length as `Selectivity@Classes`'))
  
  Weight@MeanAtAge <- Weight@MeanAtAge |> ArraySubsetYear(YearsProj)
  Weight@CVatAge <- Weight@CVatAge |> ArraySubsetYear(YearsProj)
  SDatAge <- ArrayMultiply(Weight@MeanAtAge, Weight@CVatAge)
  
  Weight@AWK <- CalcAgeSizeKey_(Weight@MeanAtAge, 
                                SDatAge, 
                                Weight@Classes, 
                                Weight@TruncSD, 
                                Weight@Dist)
  
  Selectivity@MeanAtWeight <- array(Selectivity@MeanAtWeight, 
                                    dim=c(1, length(Selectivity@MeanAtWeight),1),
                                    dimnames=list(
                                      Sim=1,
                                      Class=Selectivity@Classes,
                                      Year=YearsProj[1])
  ) |>
    ExtendYears(YearsProj)
  
  Selectivity <- MeanAtWeight2MeanAtAge(Selectivity, Weight)                              
  
  MeanAtWeight <- Selectivity@MeanAtWeight |> DropDimension('Sim')
  MeanAtAge <- Selectivity@MeanAtAge |> DropDimension('Sim')
  
  ArrayFill(slot(ProjSim@OM@Fleet[[st]],type)@MeanAtAge[,,fl]) <- MeanAtAge
  
  if (is.null(slot(ProjSim@OM@Fleet[[st]],type)@MeanAtWeight)) {
    dnamesOut <- dimnames(slot(ProjSim@OM@Fleet[[st]],type)@MeanAtAge)
    dnames <- dimnames(MeanAtWeight)
    dnamesOut[[1]] <- dnames[[1]]
    names(dnamesOut)[1] <- 'Class'
    dd <- unlist(lapply(dnamesOut, length))
    slot(ProjSim@OM@Fleet[[st]],type)@MeanAtWeight <- array(1, dim=dd,  dimnames = dnamesOut)
  }
  
  ArrayFill(slot(ProjSim@OM@Fleet[[st]],type)@MeanAtWeight[,,fl]) <- MeanAtWeight
  ProjSim
}

ProcessAdvice_SelectivityPars <- function(Selectivity, ProjSim, YearsProj, Ages, type, st, fl) {
  Selectivity@Model <- FindModel(Selectivity)
  if (is.null(Selectivity@Model)) {
    if (type=='Selectivity')
      cli::cli_abort(c("x"="`Advice@Selectivity@Pars` is populated but cannot find matching model"))
    cli::cli_abort(c("x"="`Advice@Retention@Pars` is populated but cannot find matching model"))
  }
  
  Selectivity@Pars <- StructurePars(Selectivity@Pars, nSim=1, Years=YearsProj)
  ModelClass <- getModelClass(Selectivity@Model)
  
  LengthModel <- grepl('at-Length', ModelClass)
  WeightModel <- grepl('at-Weight', ModelClass)
  AgeModel <- grepl('at-Age', ModelClass)
  
  if (LengthModel) {
    Length <- ProjSim@OM@Stock[[st]]@Length 
    if (!is.null(Selectivity@Classes))
      Length@Classes <- Selectivity@Classes
    if (is.null(Length@Classes))
      Length@Classes <- slot(ProjSim@OM@Fleet[[st]], type)@Classes[[fl]]
    if (is.null(Length@Classes))
      Length@Classes <- ProjSim@OM@Fleet[[st]]@Selectivity@Classes[[fl]]
    
    Selectivity@Classes <- Length@Classes
    Selectivity@MeanAtLength <- GenMeanAtLength(Model=Selectivity@Model,
                                                     Pars=Selectivity@Pars,
                                                     Length=Selectivity@Classes)[1,,1]
    
    return(
      ProcessAdvice_SelectivityMeanAtLength(Selectivity, ProjSim, YearsProj, Ages, type, st, fl)
    )
  } 
  
  
  if (WeightModel) {
    Weight <- ProjSim@OM@Stock[[st]]@Weight
    if (!is.null(Selectivity@Classes))
      Weight@Classes <- Selectivity@Classes
    if (is.null(Weight@Classes))
      Weight@Classes <- slot(ProjSim@OM@Fleet[[st]], type)@Classes[[fl]]
    if (is.null(Weight@Classes))
      Weight@Classes <- ProjSim@OM@Fleet[[st]]@Selectivity@Classes[[fl]]
    
    Selectivity@Classes <- Weight@Classes
    Selectivity@MeanAtWeight <- GenMeanAtWeight(Model=Selectivity@Model,
                                                     Pars=Selectivity@Pars,
                                                     Weight=Selectivity@Classes)[1,,1]
    
    
    return(
      ProcessAdvice_SelectivityMeanAtWeight(Selectivity, ProjSim, YearsProj, Ages, type, st, fl)
    )
  }
  
  Selectivity@MeanAtAge <- GenMeanAtAge(Model=Selectivity@Model,
                                             Pars=Selectivity@Pars,
                                             Ages=Ages@Classes)[1,,1]
  ProcessAdvice_SelectivityMeanAtAge(Selectivity, ProjSim, YearsProj, Ages@Classes, type, st, fl)
  
  
}
