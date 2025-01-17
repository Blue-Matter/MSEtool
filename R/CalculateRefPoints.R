# --- New Classes (temp) ----
setClass("refpoints",
         slots=c(N0='list',
                 B0='list',
                 SB0='list',
                 SP0='list',
                 MSY='list',
                 FMSY='list',
                 BMSY='list',
                 SBMSY='list',
                 SPMSY='list',
                 F01='list',
                 FMax='list',
                 FCrash='list',
                 SPRcrash='list',
                 MGT='list',
                 RefYield='list',
                 BLow='list',
                 Misc='list'
         ),
         contains='Created_ModifiedClass'
)

# Reference Points 

# Unfished 
# - N0 - unfished equilibrium numbers by stock
# - B0 - unfished equilibrium biomass by stock
# - SB0 - unfished equilibrium spawning biomass by stock
# - SP0 - unfished equilibrium spawning production (eg eggs) by stock

# Maximum Sustainable Yield
# - MSY
# - FMSY
# - BMSY
# - SBMSY
# - SPMSY 

# Per Recruit
# - F01
# - Fmax
# - Fcrash
# - Fmed
# - F_SPR
# - SPRcrash


CalcReferencePoints <- function(OM,
                                messages='default',
                                nSim=NULL, 
                                parallel=FALSE, 
                                Unfished=NULL,
                                ...) {
  
  OM <- StartUp(OM, messages, nSim) 


  RefPoints <- new('refpoints') 
  
  
  # TODO - add control option in OM for Ref points to calculate
  # add to OM definition 
  
  OM@Control$RefPoints <- list(Unfished=TRUE,
                               MSY=TRUE,
                               PerRecruit=TRUE,
                               MGT=TRUE,
                               RefYield=TRUE,
                               BLow=TRUE)
  
  #  ---- Unfished ----
  RefPoints@Unfished <- CalcUnfishedRefPoints(OM, parallel, messages)
  

  
  # ---- Per Recruit ----
  CalcPerRecruitRefPoints <- function(OM, 
                                      parallel=FALSE, 
                                      messages='default',
                                      nSim=NULL) {
    OM <- OM |> 
      nSimUpdate(nSim, messages) |>
      Populate(messages=messages) |>
      ConvertToList()
    
    # TODO                    
    if (!is.null(OM@SexPars@Herm))
      stop('Herm not done yet!')
    
    
    PerRecruitRefPoints <- list()
    
    # UPTOHERE ##
    # calculate for a simple stock
    
    
  }
  
  # ---- MSY -----
  

  
  # ---- Mean Generation Time ----
  
  # ---- Reference Yield ----
  
  # ---- B Low ----

}

CalcUnfishedRefPoints <- function(OM, 
                                  parallel=FALSE, 
                                  messages='default',
                                  nSim=NULL, 
                                  Unfished=NULL) {
  
  OM <- OM |> 
    nSimUpdate(nSim, messages) |>
    Populate(messages=messages) |>
    ConvertToList()
  
  
  # TODO                    
  if (!is.null(OM@SexPars@Herm))
    stop('Herm not done yet!')
  
  UnfishedRefPoints <- list()
  
  # average over age of maturity years?
  
}


# CalcMSYRefPoints

# CalcPerRecruitRefPoints


