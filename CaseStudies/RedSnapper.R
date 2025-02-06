library(MSEtool)

devtools::load_all()

BAMRedSnapper <- bamExtras::rdat_RedSnapper |> bamExtras::standardize_rdat()

x <- BAMRedSnapper

ImportBAM <- function(x,     
                     nSim,
                     pYears, 
                     ...) {
  
  # This works for Red Snapper - SEDAR 73
  # May need to be modified for other stocks, with aim of making it as generic
  # and flexible as possible
  
  BAMdata <- x

  AgeSeries <- BAMdata$a.series
  Ages <- AgeSeries$age
  if (min(Ages) !=0) {
    addAges <- 0:(min(Ages)-1)
    nAdd <- length(addAges)
  }
  
  
  ages <- Ages(MaxAge=max(Ages))
  
  length <- Length(Pars=list(
    Linf=BAMdat$parm.cons$Linf[1],
    K=BAMdat$parm.cons$K[1],
    t0=BAMdat$parm.cons$t0[1]),
    Units= BAMdat$info$units.length,
    CVatAge=c(rep(AgeSeries$length.cv[1], nAdd), AgeSeries$length.cv),
    Timing=0.5
  )
  
  weight <- Weight(Pars=list(),
                   MeanAtAge = c(rep(0, nAdd), AgeSeries$weight),
                   Units = BAMdat$info$units.weight)
  


  naturalmortality <- NaturalMortality(Pars=list(),
                                       MeanAtAge = c(rep(1E-6, nAdd), AgeSeries$M))
  

  # Stock 
  stock <- Stock(Name = BAMdata$info$species,
                 Ages = ages,
                 Length = length,
                 Weight = weight,
                 NaturalMortality = naturalmortality,
                 Maturity,
                 Fecundity,
                 SRR)
 
  
  
  # Fleet 
  
  
  
  om <- OM(Name=paste(rdat$info$title, rdat$info$species),
           nSim,
           nYear,
           pYear,
           CurrentYear
           Stock,
           Fleet,
           
           )
  
  snameBAM <- 
  
  
  
 
  
  
  
  
  
}

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
