library(MSEtool)
library(SAMtool)

la <- devtools::load_all

la()


SSDir <- 'C:/Users/Admin/Documents/GitHub/SALB-MSE/Condition/SS3/ALB-S_Stochastic/Condition/SS3'
StochasticDirs <- list.dirs(file.path(SSDir), full.names = TRUE, recursive = FALSE)
StochasticDirs <- StochasticDirs[!grepl('Base', StochasticDirs)]

RepList <- ImportSSReport(StochasticDirs[1:5])

OM <- ImportSS(RepList)

Hist <- SimulateDEV(OM)
Hist@RefPoints@RemovalsMSY

# MPs 


SurplusProduction <- function(Data, ...) {
  fitmodel <- SAMtool::SP(1, data2Data(Data))
  advice <- Advice()
  advice@TAC <- fitmodel@BMSY * fitmodel@FMSY
  advice
}

SurplusProduction4010 <- function(Data, ...) {
  fitmodel <- SAMtool::SP_4010(1, data2Data(Data))
  
}

IndexTarget <- function(Data, ...) {
  
}

MPs <- c('SurplusProduction',
         'SurplusProduction4010',
         'IndexTarget')


MSE <- ProjectDEV(Hist, MPs=MPs[1])



SB_SBMSY <- function(MSE) {
  MPs <- names(MSE@MPs)
  
  # TODO get MP specific MSY ref points if applicable
  SBiomass <- MSE@SBiomass
  timesteps <- dimnames(SBiomass)[[3]]
  SBMSY <- MSE@RefPoints@SBMSY |> ArraySubsetTimeStep(timesteps)  
  
  out <- SBiomass
  out[] <- NA
  for (mp in seq_along(MPs)) {
    out[,,,mp] <- ArrayDivide(abind::adrop(SBiomass[,,,mp, drop=FALSE], 4), SBMSY)
  }
  array2DF(out, 'SB_SBMSY') |> ConvertDF()
}


F_FMSY <- function(MSE) {
  MPs <- names(MSE@MPs)
  
  # TODO get MP specific MSY ref points if applicable
  apicalF <- purrr::map(MSE@FDeadAtAge, \(stock) 
             apply(stock, c('Sim', 'Age', 'TimeStep', 'MP'), sum) |>
               apply(c('Sim', 'TimeStep', 'MP'), max)
             ) |> 
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'TimeStep', 'MP'))
  
  
  timesteps <- dimnames(apicalF)[[3]]
  FMSY <- MSE@RefPoints@FMSY |> ArraySubsetTimeStep(timesteps)  
  
  out <- apicalF
  out[] <- NA
  for (mp in seq_along(MPs)) {
    out[,,,mp] <- ArrayDivide(abind::adrop(apicalF[,,,mp, drop=FALSE], 4), FMSY)
  }
  array2DF(out, 'F_FMSY') |> ConvertDF()
}



MSE@FDeadAtAge$Female |> dimnames()

MSE@FDeadAtAge$Female[1,,,,] |> apply(1:2, sum) |>
  apply(2, max)

MSE@SBiomass[1,1,,1]

MSE@Removals[1,1,,,1] |> apply(1, sum)


multiHist <- SimulateMOM(MOM)

multiHist$Female$Fleet_01@Ref$ReferencePoints$SSBMSY
multiHist$Female$Fleet_01@Ref$ReferencePoints$MSY

############ REF POINTS #################

Hist@Unfished@Equilibrium@Biomass[1,1,]
Hist@Unfished@Equilibrium@SBiomass[1,1,]




refpoints


lines(RepList$`1`$SPAWN_RECR_CURVE$`SSB/SSB_virgin`, RepList$`1`$SPAWN_RECR_CURVE$Recruitment, xlim=c(0,1), col='blue')

abline(v=0.2, lty=2)
abline(h=Stock@SRR@Pars$h[1], lty=2)





RepList$`1`$current_depletion

mainyrs <- RepList$`1`$startyr:RepList$`1`$endyr

B0 <- RepList$`1`$timeseries |> head(1) |> dplyr::select(Bio_all )

B <- RepList$`1`$timeseries |> dplyr::filter(Yr%in%mainyrs) |>
  dplyr::select(Yr, Bio_all)

B/B0$Bio_all


SB0 <- RepList$`1`$timeseries |> head(1) |> dplyr::select(SpawnBio)

SB <- RepList$`1`$timeseries |> dplyr::filter(Yr%in%mainyrs) |>
  dplyr::select(Yr, SpawnBio)

SB[,2]/SB0$SpawnBio

SB[,2]/refpoints$Value[1]


# Project 

Fixed17K <- function(Data) {
  advice <- Advice()
  advice@TAC <- 17000
  advice
}
class(Fixed17K) <- 'mp'

Fixed15K <- function(Data) {
  advice <- Advice()
  advice@TAC <- 15000
  advice
}
class(Fixed15K) <- 'mp'

MPs <- c('Fixed15K', 'Fixed17K')

MSE <- ProjectDEV(Hist, MPs=MPs)


ggplot(SB, aes(x=TimeStep, group=Sim, y=Value)) +
  facet_wrap(~MP) +
  geom_line()


## REFERENCE POINTS 
## MPS 

## RESULTS = Performance Metrics

## Grid Approach






 


