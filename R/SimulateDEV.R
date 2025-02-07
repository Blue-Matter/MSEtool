# 2025/01/20
# - NSWO.R - keep working on SPR Curve calcs. Added StockList, StockFleetList, and FleetList S3 classes

# 2025/01/17 
# - NSWO.R - generalize SPR calcs for multiple stocks and fleets 

# 2025/01/16 
# - testOM.R - code RefPoint calcs and compare with Hist - working test after NSWO working 


# TODO
# - add check in Populate for Selectivity to have max value 1. 




#' @describeIn runMSE Development version of `Simulate`
#' @export
SimulateDEV <- function(OM=NULL, 
                        messages='default',
                        nSim=NULL,
                        parallel=FALSE, 
                        silent=FALSE,
                        ...) {
  
  CheckClass(OM)
  
  if (isTRUE(silent)) 
    messages <- FALSE

  # ---- Initial Checks and Setup ----
  chk <- Check(OM) # TODO OM checks
  
  OM <- StartUp(OM, messages, nSim) 
  
  Hist <- new('hist') 
  for (nm in slotNames(OM)) {
    slot(Hist, nm) <- slot(OM, nm)
  }
  
  
  Hist@Unfished <- CalcUnfishedDynamics(OM, messages, parallel=parallel)
  # TODO Dynamic Unfished
  
  # TODO add check for changes to all functions 
  Hist@RefPoints <- CalcRefPoints(OM, Hist@Unfished)
  # TODO - SPRcrash, MGT, RefYield 
  # GetFSPR(SPRarray=Hist@RefPoints@Curves@SPR[[1]], SPRvalue=0.3)
  
  # ---- Optimize Rec Devs for Initial Depletion ----
  # TODO
  if (length(OM@Stock[[1]]@Depletion@Initial)>0)
    cli::cli_abort('Initial depletion not done', .internal=TRUE)
  
  # ---- Non-Equilibrium Initial Time Step ----
  Hist@Number <- purrr::map2(OM@Stock, Hist@Unfished@Equilibrium@Number, InitNumber)
  
  
  ##############################################################################
  CheckBAMN <- function(ts=1) {
    # Match  numbers from BAM to OM 
    ts <- 1
    BAMdata <- BAMGetObject()
    df <- data.frame(BAM=c(0,BAMdata$N.age[ts,]), 
                     OM= rowSums(Hist@Number$`SA Red Snapper`[1,,ts,]))
    plot(df$BAM)
    lines(df$OM)
    invisible(df)
  }
  ##############################################################################
  
  # loop over historical timesteps 
  TimeStepsHist <- TimeSteps(OM, 'Historical')
  
  for (ts in seq_along(TimeStepsHist)[-1]) {
    
    thisTimeStep <- TimeStepsHist[ts]
    lastTimeStep <- TimeStepsHist[ts-1]
  
    # Do MICE stuff during last time step (if applicable)
    if (length(OM@Relations)>0) {
      cli::cli_abort('MICE not done', .interal=TRUE)
    }
    
    
    # Calculate F-at-Age by Fleet 
    
    # for projections:
    # - calculate F-by-fleet-area from MP output, subject to constraint of overall F
    # - update F-at-age schedules
    
    # Hist@Fleet <- CalcFatAge(Hist@Fleet, TimeSteps=lastTimeStep)
    # FTotal <- CalcFTotal(Hist@Fleet, TimeSteps = lastTimeStep)
    
    
    CalcEffort <- function(Fleet) {
      GetApicalF(Fleet)
      Fleet@Effort@Catchability
      
      apicalF <- apply(GetFatAgeArray(Fleet), c(1,3), max)
      
    }
    
    # TODO - calculate Effort from apicalF 
    # TODO - distribute effort across areas subject to overall F constraint
    
    
    
    # Calculate fishing spatial distribution for last time step
    # Distribute fishing effort while maintaining overall F
  
    DistributeFishing <- function(Hist, TimeStep) {
      # each fleet should distribute according to overall utility across stocks
      
      # Calculate fleet-specific utility by area (over all stocks)
      NatAgeArea <- purrr::map(Hist@Number, ArraySubsetTimeStep, TimeSteps=TimeStep)
      FleetWeightAgeArea <- purrr::map2(Hist@Stock, Hist@Fleet, GetFleetWeightAtAge,
                                    TimeSteps=TimeStep) |>
        purrr::map(AddDimension, name='Area') |>
        purrr::map(aperm, perm=c(1,2,3,5,4))
      
      # currently based on retained biomass assuming equal value across stocks
      # TODO: account for price/value per stock and Cost per fleet/area
      # TODO: account for Targeting parameter
      NatAgeAreaFleet <- purrr::map(NatAgeArea, AddDimension, name='Fleet')
      BatAgeAreaFleet <- purrr::map2(NatAgeAreaFleet, FleetWeightAgeArea, ArrayMultiply)
      
      selectivity <- purrr::map(Hist@Fleet, GetSelectivityAtAge, TimeSteps=TimeStep) |>
        purrr::map(function(.x) {
          array <- array(unlist(l), dim=c(dim(l[[1]]), length(l)))
          dimnames(array) <- c(dimnames(l[[1]]), Fleet=list(names(l)))
          AddDimension(array, 'Area') |>
          aperm(c(1,2,3,5,4))
        })
      
      retention <- purrr::map(Hist@Fleet, GetRetentionAtAge, TimeSteps=TimeStep) |>
        purrr::map(function(.x) {
          array <- array(unlist(l), dim=c(dim(l[[1]]), length(l)))
          dimnames(array) <- c(dimnames(l[[1]]), Fleet=list(names(l)))
          AddDimension(array, 'Area') |>
            aperm(c(1,2,3,5,4))
        })
      
      SelectivityRetention <- purrr::map2(selectivity, retention, ArrayMultiply)

      VBatAgeStockAreaFleetList <- purrr::map2(BatAgeAreaFleet, 
                                           SelectivityRetention, 
                                           ArrayMultiply)
      dd <- dim(VBatAgeStockAreaFleetList[[1]]) 
      VBatAgeStockAreaFleet <- array(unlist(VBatAgeStockAreaFleetList), 
                                     dim=c(dd,
                                           length(VBatAgeStockAreaFleetList)))
     
      dimnames(VBatAgeStockAreaFleet) <- c(dimnames(VBatAgeStockAreaFleetList[[1]]),
                                           list(Stock=StockNames(Hist)))
      # TODO
      # account for spatial closure
      # Fleet@Distribution@Closure
      # Fleet@Distribution@Cost
      # Calculate relative value by area, stock, fleet (cost of area & value of age class by stock/fleet)
      
      ValueAreaFleet <-  apply(VBatAgeStockAreaFleet, c(1,4,5), sum)
      nAreas <- dim(ValueAreaFleet)[2]
      TotalValueAreaFleet <- replicate(nAreas, apply(ValueAreaFleet, c(1,3), sum)) |> aperm(c(1,3,2))
      
      RelativeValueAreaFleet <- ValueAreaFleet/TotalValueAreaFleet
      RelativeValueAreaFleet[!is.finite(RelativeValueAreaFleet)] <- 0
    
      # distribute effort by area
      Fleet@Effort@Effort[1,1] * RelativeValueAreaFleet[1,,1]
      
      ########## UPTO HERE #####
      
      # calculate F by area given overall F 
      
      
      
      
      
      
      
      
    }
    
    
    
    # Calculate fishing mortality for last time step
    
    # Calculate Spawning Output  and Recruitment for last time step
    

    # Mortality and Aging into this time step
    
    # Move Stock at beginning of this time step
    
    
    
    
  }
  
  
  
 
  
  
  

  
  
  HistRel <- SetHistRel(OM) 
  
  
  
  # ---- Optimize catchability (q) to fit depletion ----
  # (if needed)
  
  # ---- Run Historical Simulations ----
  
  # ---- Calculate Historical Catch ----
  
  # ---- Condition Observation Object on Real Fishery Data ----
  
  # ---- Simulate Fishery Data ----
  
  # ---- Return `hist` Object ----
  
  
  
  # # --- Sample Obs Parameters ----
  # # TODO - updated Obs object
  # 
  # ObsPars[[p]] <- lapply(1:nf, function(f) {
  #   SampleObsPars(MOM@Obs[[p]][[f]], nsim,
  #                 cpars = SampCpars[[p]][[f]],
  #                 Stock = StockPars[[p]],
  #                 nyears, proyears)
  # })
  # 
  # # --- Sample Imp Parameters ----
  # # TODO - updated Imp object
  # 
  # ImpPars[[p]] <- lapply(1:nf, function(f) {
  #   SampleImpPars(MOM@Imps[[p]][[f]], nsim,
  #                 cpars = SampCpars[[p]][[f]],
  #                 nyears, proyears)
  # })
  
  

  
  
  

    
  
  
  
                        
  
  
  
  
}


