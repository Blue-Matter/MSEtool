
# devtools::load_all()

# NOTE: Curves do not account for spatial closures

# --- New Classes (temp) ----

# Equilibrium values for a given F 
setClass("Curves",
         slots=c(apicalF='numeric', # vector
                 SPR='data.frame', # 
                 YPR='list',
                 Recruit='list',
                 Yield='list',
                 Biomass='list',
                 SBiomass='list',
                 SP='list',
                 Misc='list'
         ),
         contains='Created_ModifiedClass'
)


CalcSPRCurve <- function(OM, 
                         messages='default',
                         nSim=NULL,
                         parallel=FALSE) {
  
  # TODO add FVector to OM Control or somewhere else 
  boundsF <- c(1E-3, 1)
  FSearch <- exp(seq(log(min(boundsF)), log(max(boundsF)), length.out = 50))
  
  OM@Control$FVector <- FSearch
  
  FVector <- OM@Control$FVector
  
  # TODO Calculate SPR0 as a reference point and pass into function
  # TODO update function to take `hist` object as well
  
  SPR0 <- CalcSPR0(OM) # unfished spawning production per recruit
  df <- data.frame(F=FSearch)
  SPRatF <- list()
  
  cli::cli_progress_step(
    'Calculating Equilibrium Spawning Potential Ratio',
    msg_done = 'Calculated Equilibrium Spawning Potential Ratio',
    spinner = TRUE)

  SPRList <- list()
  for (i in seq_along(FSearch)) {
    SPRatF <- CalcSPR(OM, apicalF=FSearch[i], SPR0=SPR0)
    df  <- purrr::map_df(SPRatF, array2DF, responseName='SPR')
    df$F <- FSearch[i]
    df$Stock <- purrr::map2(as.list(names(SPRatF)), 
                            purrr::map(SPRatF, length), rep) |> 
      unlist()
    cli::cli_progress_update()
    SPRList[[i]] <- df
  }
  cli::cli_progress_done()

  # TODO store as array or data.frame??
  df <- do.call(rbind, SPRList)
  object.size(df)/1E6
  dim(df)
  100000    * 6 * 8
  object.size(df)
  
  df$TEST <- 100
  object.size(df)/1E6
  df <- rbind(df, df)
  object.size(df)/1E6
  
  Stock <- OM@Stock[[1]]
  FleetList <- OM@Fleet[[1]]
  
  apicalF <- 0.05
  
  t1 <- CalcFishedSurvival(OM)
  t2 <- CalcFishedSurvival(OM, apicalF=0.5)
  t1$Albacore[1,,1] |> max()
  t2$Albacore[1,,1] |> max()
  
  
  t1 <- CalcFishedSurvival(OM@Stock, OM@Fleet, apicalF = 0.1)
  t2 <- CalcFishedSurvival(OM@Stock[[1]], OM@Fleet[[1]], apicalF = 0.1)
  t1$Albacore[1,,1] |> max()
  t2[1,,1] |> max()
  
  
  CalculateSPR_ <- function(Stock, FleetList, apicalF) {
    
    
    # fished egg production per recruit
    SPRF <- MultiplyArrays(array1=FishedSurvival, array2=Stock@Fecundity@MeanAtAge) |>
      apply(c(1,3), sum) |> process_cpars()
    
    ## over stocks --
    DivideArrays(SPR0, SPRF)
    
  }
  
  
  for (i in seq_along(OM@Control$FVector)) {
    apicalF <- OM@Control$FVector[i]
    
    
    
    
    
    
    sum(EPF)/sum(EP0)
  }
  
  OM@Fleet$Female$SPN_1@FishingMortality@ApicalF
  OM@Fleet$Female$US_2@FishingMortality@ApicalF
  OM@Fleet$Female$US_2@FishingMortality
  
  
  
  # SP0  # Unfished Spawning Production per Recruit
  
  
  # SPR <- SPF/SP0 
  
  
  
  list(FVector=F_search,
       SPR=SPR)
  
}