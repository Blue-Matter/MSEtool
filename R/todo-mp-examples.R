
# TODO - add these to manual instead of package 


# TAC by Fleet
AverageCatch_Fleet <- function(Data) {
  LHIndex <- LastHistYearInd(Data)
  
  if (any(Data@Landings@Units !='Biomass' &&
          max(Data@Years) <= Data@YearLH)) {
    cli::cli_alert_warning('TAC is set to average historical landings by fleet but some landings units are not "Biomass"')
  } 
  
  newTAC <- apply(Data@Landings@Value[1:LHIndex,, drop=FALSE], 'Fleet', mean)
  Advice(TAC=newTAC)
}
class(AverageCatch_Fleet) <- 'mp'


# TAC by Fleet-Area
AverageCatch_FleetArea <- function(Data) {
  LHIndex <- LastHistYearInd(Data)
  
  if (any(Data@Landings@Units !='Biomass' &&
          max(Data@Years) <= Data@YearLH)) {
    cli::cli_alert_warning('TAC is set to average historical landings by fleet but some landings units are not "Biomass"')
  } 
  
  newTAC <- apply(Data@Landings@Value[1:LHIndex,, drop=FALSE], 'Fleet', mean)
  nArea <- nArea(Data)
  
  newTAC <- matrix(newTAC, nrow=length(newTAC), ncol=nArea, 
                   dimnames = list(Fleet=names(newTAC),
                                   Area = 1:nArea))
  
  newTAC <- newTAC / nArea
  Advice(TAC=newTAC)
}
class(AverageCatch_FleetArea) <- 'mp'




