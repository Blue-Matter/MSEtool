library(MSEtool)

# TODO - reduce Hist/MSE object sims/TS

# la <- devtools::load_all
# 
# la()

nSim <- 200
pYear <- 30

Interval <- 3 
Name <- 'Southern Atlantic Albacore'
StockName <- "Albacore"
Species <- "Thunnus alalunga"
Region <- 'South Atlantic'
Agency <- 'ICCAT'
DataLag <- 1 # lagged by 1 year?

SSDir <- 'C:/Users/Admin/Documents/GitHub/SALB-MSE/Condition/SS3/ALB-S_Stochastic/Condition/SS3'

StochasticDirs <- list.dirs(file.path(SSDir), full.names = TRUE, recursive = FALSE)
StochasticDirs <- StochasticDirs[!grepl('Base', StochasticDirs)]

StochasticDirs <- StochasticDirs[1:5] # fewer for development

RepList <- ImportSSReport(StochasticDirs)

OM <- ImportSS(RepList, 
               nSim=nSim, 
               pYear = pYear,
               Name=Name,
               Agency=Agency,
               Region=Region,
               StockName=StockName,
               Species=Species,
               Interval=Interval,
               DataLag=DataLag)

Hist <- Simulate(OM)

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

