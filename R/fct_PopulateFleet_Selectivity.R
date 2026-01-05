FindL50_vec <- function(prob_vec) {
  classes <- names(prob_vec) |> as.numeric()
  LinInterp(prob_vec, y=classes, 0.5)
}

FindL50 <- function(Maturity) {
  if (!is.null(Maturity@Pars$L50)) {
    return(Maturity@Pars$L50)
  }
  
  MaturityAtLength <- Maturity@MeanAtLength
  if (is.null(MaturityAtLength)) {
    cli::cli_abort("Values required for `Maturity@MeanAtLength` if `Selectivity@isRel == TRUE`")
  }
  
  apply(Maturity@MeanAtLength, c('Sim', 'Year'), FindL50_vec)
}

PopulateSelectivity <- function(Selectivity,
                                Ages = NULL,
                                Length = NULL,
                                Weight = NULL,
                                Maturity = NULL,
                                nSim = 5,
                                Years = NULL,
                                nArea = 1,
                                CalcAtLength = TRUE,
                                seed = NULL,
                                silent = FALSE,
                                CheckMaxValue = TRUE) {
  argList <- list(Ages, Length, Weight, Years, nArea, nSim, CalcAtLength, seed)
  
  if (CheckDigest(Selectivity, argList)) {
    return(Selectivity)
  }
  
  SetSeed(seed)
  
  Selectivity@Pars <- StructurePars(Pars = Selectivity@Pars, nSim, Years, nArea)
  Selectivity@Model <- FindModel(Selectivity)
  ModelClass <- getModelClass(Selectivity@Model)

  if (!is.null(ModelClass)) {
    # Model & Parameters exist
    
    if (Selectivity@isRel) {
      CheckRequiredObject(Maturity, 'maturity', 'Maturity')
      L50 <- FindL50(Maturity)
      # TODO - extend for other parameters? 
      Selectivity@Pars$L5 <- ArrayMultiply(Selectivity@Pars$L5, L50)
      Selectivity@Pars$LFS <- ArrayMultiply(Selectivity@Pars$LFS, L50)
    }
    
    if (grepl("at-Length", getModelClass(Selectivity@Model))) {
      Selectivity <- PopulateMeanAtLength(
        Selectivity,
        Length,
        Years,
        Ages,
        nSim,
        seed,
        silent
      )
    } else if (grepl("at-Weight", getModelClass(Selectivity@Model))) {
      Selectivity <- PopulateMeanAtWeight(Selectivity, Weight, Years, Ages, nSim, seed, silent)
    } else if (grepl("at-Age", getModelClass(Selectivity@Model))) {
      Selectivity <- PopulateMeanAtAge(Selectivity, Ages, Years, Length)
    }
  }
  
  Selectivity <- MeanAtLength2MeanAtAge(Selectivity, Length, max1 = TRUE)
  Selectivity <- MeanAtWeight2MeanAtAge(Selectivity, Weight, max1 = TRUE)
  
  if (CalcAtLength) {
    Selectivity <- MeanAtAge2MeanAtLength(Selectivity, Length, replace = FALSE)
  }
  
  
  if (is.null(Selectivity@MeanAtAge)) {
    cli::cli_abort(" {.var Selectivity} requires values for either `Model` & `Pars` or `MeanAtAge`")
  }
  
  # Check Selectivity has a max value of 1 across age classes
  if (CheckMaxValue) {
    Selectivity@MeanAtAge <- CheckSelectivityMaximum(Selectivity@MeanAtAge)
  }
  
  # Add Area Dimension 
  Selectivity@MeanAtLength <- AddDimension(Selectivity@MeanAtLength, 'Area')
  Selectivity@MeanAtWeight <- AddDimension(Selectivity@MeanAtWeight, 'Area') 
  Selectivity@MeanAtAge <- AddDimension(Selectivity@MeanAtAge, 'Area')
  
  # Add Dimension Names
  Selectivity@MeanAtLength <- SetDimNames_SCYR(
    Selectivity@MeanAtLength,
    Selectivity@Classes,
    Years
  )
  
  Selectivity@MeanAtWeight <- SetDimNames_SCYR(
    Selectivity@MeanAtWeight,
    Selectivity@Classes,
    Years
  )
  
  Selectivity@MeanAtAge <- SetDimNames_SAYR(
    Selectivity@MeanAtAge,
    Ages@Classes,
    Years
  )
  
  SetDigest(Selectivity, argList)
}
