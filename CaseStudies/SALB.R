library(MSEtool)

# TODO - reduce Hist/MSE object sims/TS

la <- devtools::load_all

la()


pYear <- 30

Interval <- 3 
Name <- 'Southern Atlantic Albacore'
StockName <- "Albacore"
Species <- "Thunnus alalunga"
Region <- 'South Atlantic'
Agency <- 'ICCAT'
DataLag <- 1 # lagged by 1 year?

SSDir <- 'C:/Users/Admin/Documents/GitHub/SALB-MSE/Condition/SS3/ALB-S_Stochastic/Condition/SS3'

SSDir <- 'G:/Shared drives/BM shared/1. Projects/TOF-MSE-SALB/ALB-S_Stochastic/ALB-S_Stochastic/Condition/SS3'

StochasticDirs <- list.dirs(file.path(SSDir), full.names = TRUE, recursive = FALSE)
StochasticDirs <- StochasticDirs[!grepl('Base', StochasticDirs)]

StochasticDirs <- StochasticDirs[1:5] # fewer for development

RepList <- ImportSSReport(StochasticDirs)

OM <- ImportSS(RepList, 
               pYear = pYear,
               Name=Name,
               Agency=Agency,
               Region=Region,
               StockName=StockName,
               Species=Species,
               Interval=Interval,
               DataLag=DataLag)

parallel=FALSE
silent=FALSE
nsim=NULL
nSim=NULL

################################################################################

# Why don't ref points match up??

# 67 is ok, 68 is very different

replist67 <- RepList[[67]]
replist68 <- RepList[[68]]

OM67 <- ImportSS(replist67, nsim=1)
OM68 <- ImportSS(replist68, nsim=1)

Hist67 <- Simulate(OM67)
Hist68 <- Simulate(OM68)

CompareSSRefPoints(replist67, Hist67)

CompareSSRefPoints(replist68, Hist68)

SP0(Hist68) |> dplyr::filter(TimeStep==max(TimeStep))
replist68$timeseries |> dplyr::filter(Era=='VIRG') |> dplyr::select(SpawnBio)



MSE <- Project(Hist67, MPs='FMSYRef')

################################################################################





Hist <- Simulate(OM)

SP_SPMSY(Hist) |> dplyr::filter(TimeStep==max(TimeStep)) |>
  dplyr::summarise(Median=median(Value),
                   Lower=quantile(Value,0.025),
                   Upper=quantile(Value,0.975))


sphist <- SP_SPMSY(Hist)
sbhist <- SB_SBMSY(Hist)

SP_SPMSY(Hist) |> dplyr::filter(TimeStep==max(TimeStep)) |>
  dplyr::summarise(Lower=quantile(Value,0.025),
                   Median=median(Value),
                   Upper=quantile(Value,0.975))

SpawnBio |> dplyr::filter(Year==max(Year)) |> 
  dplyr::summarise(Lower=quantile(SB_SBMSY,0.025),
                   Median=median(SB_SBMSY),
                   Upper=quantile(SB_SBMSY,0.975))


sphist |> dplyr::filter(TimeStep==max(TimeStep), Value>=3.4)
SpawnBio |> dplyr::filter(Year==max(Year), SB_SBMSY>=3.4)

sim <- 68
SpawnBio |> dplyr::filter(Sim==sim)
sphist |> dplyr::filter(Sim==sim)
sbhist |> dplyr::filter(Sim==sim)


CompareSSNumber(replist, Hist)

SProduction(Hist) |> dplyr::filter(TimeStep==max(TimeStep), Sim==1)
SPMSY(Hist) |> dplyr::filter(TimeStep==max(TimeStep), Sim==1)
SP_SPMSY(Hist) |> dplyr::filter(TimeStep==max(TimeStep), Sim==1)


SB0(Hist) |> dplyr::filter(TimeStep==max(TimeStep), Sim==1)
SBMSY(Hist) |> dplyr::filter(TimeStep==max(TimeStep), Sim==1)
SPMSY(Hist) |> dplyr::filter(TimeStep==max(TimeStep), Sim==1)

t <- SProduction(Hist) |> dplyr::filter(Sim==1)
plot(t$TimeStep, t$Value, type='l')


t <- SBiomass(Hist) |> dplyr::filter(Sim==1)
plot(t$TimeStep, t$Value, type='l')


SP_SPMSY(Hist) |> dplyr::filter(TimeStep==max(TimeStep)) |>
  dplyr::summarise(Median=median(Value),
                   Lower=quantile(Value,0.025),
                   Upper=quantile(Value,0.975))

SSRefs <- purrr::map(RepList, \(replist) {
  refs <- replist$derived_quants |> dplyr::filter(Label%in% c('Dead_Catch_MSY',
                                                      'Ret_Catch_MSY',
                                                      'SSB_MSY', 
                                                      'SPR_MSY')) |>
    dplyr::select(Label, Value) |>
    dplyr::mutate(Value=as.numeric(Value))
  rownames(refs) <- NULL
  refs
  }) |> List2Array('Sim') |> array2DF() |> tidyr::pivot_wider(values_from = Value, names_from = Var2) |>
  ConvertDF()

mainyrs <- RepList[[1]]$startyr:RepList[[1]]$endyr

SBMSY <- SSRefs |> dplyr::filter(Label=='SSB_MSY') |>
  dplyr::mutate(SBMSY=Value) |>
  dplyr::select(Sim, SBMSY)

SpawnBio <- purrr::imap(RepList, \(replist,idx) {
  replist$timeseries |> dplyr::filter(Yr %in% mainyrs) |>
    dplyr::select(Year=Yr, SBiomass=SpawnBio) |>
    dplyr::mutate(Sim=idx)
  
}) 

SpawnBio <- do.call('rbind', SpawnBio)  |> ConvertDF()  |>
  dplyr::left_join(SBMSY) |>
  dplyr::mutate(SB_SBMSY=SBiomass/SBMSY)
  
SpawnBio |> dplyr::filter(Year==max(Year), Sim==1)
SB_SBMSY(Hist) |> dplyr::filter(TimeStep==max(TimeStep), Sim==1)

SP_SPMSY(Hist) |> dplyr::filter(TimeStep==max(TimeStep), Sim==1)


Hist@RefPoints@MSYRefPoints@SPMSY[1,,]







SSRefs |> dplyr::filter(Label=='SSB_MSY') |>
  dplyr::summarise(Lower=quantile(Value,0.025),
                   Median=median(Value),,
                   Upper=quantile(Value,0.975))



SSRefs |> dplyr::filter(Label=='Dead_Catch_MSY') |>
  dplyr::summarise(Lower=quantile(Value,0.025),
                   Median=median(Value),,
                   Upper=quantile(Value,0.975))






F_FMSY(Hist) |> dplyr::filter(TimeStep==max(TimeStep)) |>
  dplyr::summarise(Median=median(Value))


MSY(Hist)


RepList$`1`$derived_quants |> dplyr::filter(Label%in% c('Dead_Catch_MSY',
                                                        'Ret_Catch_MSY',
                                                        'SSB_MSY', 
                                                        'SPR_MSY')) |>
  dplyr::select(Label, Value)


Hist <- readRDS('C:/Users/Admin/Documents/GitHub/SALB-MSE/Hist/Stochastic.hist')

source("C:/Users/Admin/Documents/GitHub/SALB-MSE/3. DefineCMPs.R")

ISlope <- function(Data) {
  Rec <- DLMtool::Islope1(1, data2Data(Data))
  advice <- Advice()
  advice@TAC <- as.numeric(Rec@TAC)
  advice
}

MPs <- avail('mp')

MPs <- 'ISlope'

MSE <- Project(Hist, MPs)

SB_SBMSY(MSE) |> dplyr::group_by(MP, TimeStep) |>
  dplyr::filter(Period=='Projection') |>
  dplyr::summarise(Mean=mean(Value)) |>
  tidyr::pivot_wider(names_from = MP, values_from = Mean) |>
  print(n=90)


Landings(MSE) |> dplyr::group_by(MP, TimeStep) |>
  dplyr::filter(Period=='Projection') |>
  dplyr::summarise(Mean=median(Value)) |>
  tidyr::pivot_wider(names_from = MP, values_from = Mean) |>
  print(n=90)


apicalF(MSE) |> dplyr::group_by(MP, TimeStep) |>
  dplyr::filter(Period=='Projection') |>
  dplyr::summarise(Max=max(Value)) |>
  tidyr::pivot_wider(names_from = MP, values_from = Max) |>
  print(n=90)


F_FMSY(MSE) |> dplyr::group_by(MP, TimeStep) |>
  dplyr::filter(Period=='Projection') |>
  dplyr::summarise(Mean=median(Value)) |>
  tidyr::pivot_wider(names_from = MP, values_from = Mean) |>
  print(n=90)

