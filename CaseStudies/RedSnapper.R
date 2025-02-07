library(MSEtool)

devtools::load_all()


x <- 'Red Snapper'
nSim=48
pYear=50

ImportBAM <- function(x,     
                     nSim=48,
                     pYear=50, 
                     ...) {
  
  # This works for Red Snapper - SEDAR 73
  # May need to be modified for other stocks, with aim of making it as generic
  # and flexible as possible
  
  if(!requireNamespace("bamExtras", quietly = TRUE)) {
    stop("Package `bamExtras` is required for this function. Install with `pak::pkg_install('nikolaifish/bamExtras')`",
         call. = FALSE)
  }
  
  BAMdata <- BAMGetObject(x)
  OM <- BAMSetupOM(BAMdata, nSim, pYear)

  OM@Stock <- list()
  OM@Stock[[BAMdata$info$species]] <- BAM2Stock(BAMdata, 
                                                nSim=nSim(OM),
                                                CurrentYear=OM@CurrentYear,
                                                TimeSteps=OM@TimeSteps)
  OM@Fleet <- BAM2Fleet(x, OM@TimeSteps, OM@Stock[[1]])

  
  BAM2Fleet <- function(x, TimeSteps, Stock) {
    BAMdata <- BAMGetObject(x)
    # Combines Retention and Discard fleets 
    FleetNames <- names(BAMdata$parms)[grepl("F.prop", names(BAMdata$parms))] |>
      vapply(function(x) strsplit(x, "F.prop.")[[1]][2], character(1))
    
    DiscardFleets <- FleetNames[grepl('.D', FleetNames)] |> as.character()
    RetainFleets <- FleetNames[!FleetNames %in% DiscardFleets] |> as.character()
    nFleet <- length(RetainFleets)
    
    # Discard Mortality Values and Time Blocks
    DiscardMortArray <- BAMGetDiscardMortality(x, TimeSteps, nFleet)

    HistTS <- TimeSteps[TimeSteps<=Stock@CurrentYear]
    nHist <- length(HistTS)
    TimeSeries <- BAMdata$t.series |> dplyr::filter(year %in% HistTS)
    
    FCols <- paste0('F.',FleetNames)
    ApicalF <- TimeSeries[FCols]
    
    SelectACols <- paste0('sel.m.',FleetNames)
    SelectAtAge <- BAMdata$sel.age[SelectACols] 
    
    Ages <- BAMdata$a.series$age
    if (min(Ages) !=0) {
      addAges <- 0:(min(Ages)-1)
      nAdd <- length(addAges)
    }
    
    FDeadatAge <- array(0, dim=c(nHist, max(Ages), length(RetainFleets)))
    FRetainatAge <- array(0, dim=c(nHist, max(Ages), length(RetainFleets)))
    for (fl in seq_along(RetainFleets)) {
      fleet <- RetainFleets[fl]
      retain <- paste0('F.', fleet)
      discard <- paste0('F.', fleet, '.D')
      selretain <- paste0('sel.m.', fleet)
      seldiscard <- paste0('sel.m.', fleet, '.D')
      
      FRetainatAge[,,fl] <- ApicalF[[retain]] * SelectAtAge[[selretain]]
      FDeadatAge[,,fl] <-  FRetainatAge[,,fl] + ApicalF[[discard]] * SelectAtAge[[seldiscard]]
    }
    FDeadatAge <- abind::abind(array(0, 
                                     dim=c(length(HistTS), nAdd, length(RetainFleets))),  
                               FDeadatAge, along=2) |>
      aperm(c(2,1,3)) |>
      AddDimNames(c("Age", "Time Step", 'Fleet'), TimeSteps = TimeSteps)
    dimnames(FDeadatAge)$Fleet <- RetainFleets
    
    FRetainatAge <- abind::abind(array(0, 
                                     dim=c(length(HistTS), nAdd, length(RetainFleets))),  
                                 FRetainatAge, along=2) |>
      aperm(c(2,1,3)) |>
      AddDimNames(c("Age", "Time Step", 'Fleet'), TimeSteps = TimeSteps)
    dimnames(FRetainatAge)$Fleet <- RetainFleets
    
    ApicalF <- apply(FDeadatAge, 2:3, max)
    
    FDeadDiscard <- ArraySubtract(FDeadatAge,FRetainatAge)
    
    FInteractatAge <- ArrayAdd(FDeadatAge, ArrayDivide(FDeadDiscard, DiscardMortArray))
    Effort <- apply(FInteractatAge, 2:3, max)
    
    FInteractMax <- replicate(nAge(Stock),apply(FInteractatAge, 2:3, max)) |> aperm(c(3,1,2)) 
    dimnames(FInteractMax) <-  dimnames(FInteractatAge)
    
    SelectivityAtAge <- ArrayDivide(FInteractatAge, FInteractMax) 
    SelectivityAtAge[!is.finite(SelectivityAtAge)] <- 0
    
    RetentionAtAge <- ArrayDivide(FRetainatAge, FInteractatAge)
    RetentionAtAge[!is.finite(RetentionAtAge)] <- 0
    
    FleetList <- vector('list',1)
    names(FleetList) <- Stock@Name
    for (fl in seq_along(RetainFleets)) {
      fleet <- Fleet(Name=RetainFleets[fl])

      apicalF <- ApicalF[,fl, drop=FALSE] |> t()
      dimnames(apicalF) <- list(Sim=1,
                                'Time Step'=  dimnames(apicalF)$`Time Step`)

      fleet@FishingMortality <- FishingMortality(ApicalF=apicalF)
      
      discmort <- AddDimension(DiscardMortArray[,,fl], 'Sim') |>
        aperm(c(3,1,2))
  
      fleet@DiscardMortality <- DiscardMortality(MeanAtAge = discmort)
      
      effort <- AddDimension(Effort[,fl, drop=FALSE], 'Sim') |>
        abind::adrop(2) |> aperm(2:1)
      fleet@Effort <- Effort(Effort=effort)
      
      MeanAtAge <- AddDimension(SelectivityAtAge[,,fl], 'Sim') |> aperm(c(3,1,2))
      fleet@Selectivity <- Selectivity(MeanAtAge=MeanAtAge)
      MeanAtAge <- AddDimension(RetentionAtAge[,,fl], 'Sim') |> aperm(c(3,1,2))
      fleet@Retention <- Retention(MeanAtAge=MeanAtAge)
      
    
      FleetList[[fleet@Name]] <- fleet
    }
    
    t = Populate(fleet, TimeSteps=TimeSteps)
    GetApicalF(t, TimeSteps='2019')
    
    t <- CalcFatAge(t, TimeSteps = TimeSteps)
    
    
    t@FishingMortality@DeadAtAge[1,,70]
    FDeadatAge[,70,3] 
    
    t@FishingMortality@DeadAtAge[1,,70] |> plot()
    lines(FDeadatAge[,70,3])
    
    stop()
    
    
    t@FishingMortality@DeadAtAge[1,,70] |> max()
    FDeadatAge[,70,3] 
    
    t@FishingMortality@RetainAtAge[1,,70] 
    FRetainatAge[,70,3]  
   
    
    FInteractatAge[,70,3] |> max()
    
    
    plot(FDeadatAge[,70,3],type='l')
    lines(t@FishingMortality@DeadAtAge[1,,70], col='blue')
    
    fleet@FishingMortality@ApicalF[1,]
    t@FishingMortality@ApicalF[1,]
    
    fleet@Selectivity@MeanAtAge[1,,70]
    t@Selectivity@MeanAtAge[1,,70]
    
    # check DeadatAge 
    
    
    
    
    
  }


  

  

  
  

  om@Fleet <- list()
  om@Fleet[[rdat$info$species]]
  om
  
}




messages='default'
nSim=NULL
parallel=FALSE
silent=FALSE

SimulateDEV

# compare with BAM model 

# Add Spatial 





MOM <- readRDS('../SAFMC-MSE/OM_Objects/BaseCase_RS.OM')
MOM@nsim <- 4

OMa <- Convert(MOM, Populate = FALSE)  
OM <- Populate(OMa)

messages='default'
nSim=NULL
parallel=FALSE
silent=FALSE

SimulateDEV


# TODO: match MSY, F30% etc with Table 27 in SEDAR 73

multiHist <- Simulate(MOM)

fl <- tempfile()
fl
saveRDS(multiHist, fl)
# SPR F 30%
multiHist$`Red Snapper`$`Commercial Line`@Ref$ByYear$F_SPR[1,,70]

t <- SAMSE:::Calc_RS_Ref_Points(multiHist)

multiHist[[1]][[1]]@TSdata$Unfished_Equilibrium$N_at_age[1,1,70,] 

multiHist[[1]][[1]]@SampPars$Stock$mov[1,1,,,1]
multiHist[[1]][[1]]@SampPars$Stock$initdist[1,1,] |> round(2)


multiHist[[1]][[1]]@SampPars$Stock$[1,21,] |> round(2)

t <- multiHist[[1]][[1]]@AtAge$Number[1,1,70,]
round(t/sum(t),2)




OM@Stock$`Red Snapper`@Spatial@FracOther
parallel=FALSE
messages='default'
nSim=NULL
silent=FALSE


SimulateDEV
