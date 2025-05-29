


library(MSEtool)

la <- devtools::load_all

la()

nsim <- 10

octopusOM <- OM('Day Octopus OM',
                Author='Adrian Hordyk',
                Email='adrian@bluematterscience.com',
                Sponsor='[Blue Ventures Indonesia](https://blueventures.org/about/team/indonesia/)',
                Region='NE Sulawesi, Indonesia',
                Latitude=1.741653,
                Longitude = 125.085258,
                nYear=5,
                pYear=5,
                TimeUnits = 'month',
                nSim=nsim)

octopusOM@TimeSteps


## ---- create_stock ----

octopus <- Stock('Day octopus',
                 CommonName='Gurita',
                 Species='Octopus cyanea')

## ---- ages ---
Ages(octopus) <- Ages(MaxAge=14,
                      Units='month',
                      PlusGroup = FALSE)

## ---- length ----
# Schnute Growth Model

Schnute <- function(Ages, SizeAgeZero, SizeInflectionPoint, GrowthRateCoefficient, ShapeParameter) {
  (SizeAgeZero^ShapeParameter + ((SizeInflectionPoint^ShapeParameter)/(1-ShapeParameter)-SizeAgeZero^ShapeParameter) *
     (1-exp(-GrowthRateCoefficient*Ages)))^(1/ShapeParameter)
}


Length(octopus) <- Length(Pars=list(SizeAgeZero=c(30, 40),
                                   SizeInflectionPoint=c(150, 200),
                                   GrowthRateCoefficient=c(0.6, 0.9),
                                   ShapeParameter=c(-5,-3)),
                         Model=Schnute,
                         CVatAge=c(0.1,0.2))

## ---- weight ----

Weight(octopus) <- Weight(Pars=list(Alpha=0.0721,
                                    Beta=1.9181),
                          Units='g',
                          Classes=seq(from=0.25, by=0.5, to=4))

## ---- natural_mortality ----

NaturalMortality(octopus) <- NaturalMortality(Pars=list(M=c(0.1, 0.2)),
                                              Units='month')

## ---- maturity ----

Maturity(octopus) <- Maturity(Pars=list(A50=c(10, 11),
                                        A50_95=c(0.1, 0.5)),
                              Semelparous=TRUE)

## ---- stockrecruit ----

# Ricker
SRR(octopus) <- SRR(Pars=list(hR=c(0.85, 0.95)),
                    R0=10000,
                    SD=c(0.3,0.5),
                    SpawnTimeFrac = 0.5)


## ---- spatialdistribution ----

Spatial(octopus) <- Spatial(UnfishedDist=c(0.3, 0.3),
                            ProbStaying=c(0.95, 0.99),
                            RelativeSize='EqualDensity')

## ---- depletion ----
Depletion(octopus) <- Depletion(Final=c(0.35, 0.45), Reference="SB0")


## ---- fleet ----
octopus_fleet <- Fleet('Octopus Fleet')

## ---- effort ----

Effort(octopus_fleet) <- Effort(Effort=data.frame(TimeStep=c(0, 0.125, 0.5, 1),
                                                  Lower=c(0, 0.4, 0.9, 0.9),
                                                  Upper=c(0, 0.6, 1, 1),
                                                  CV=0.1))

## ---- selectivity ----

Selectivity(octopus_fleet) <- Selectivity(Pars=list(A50=c(4, 6),
                                                    A50_95=c(1,2)))

## ---- populateOM ----

Stock(octopusOM) <- octopus
Fleet(octopusOM) <- octopus_fleet

octopusOM@Control$RefYield$lastnTS <- 12

OM <- Populate(octopusOM)


# ---- Simulate Historical -----
messages='default'
nSim=NULL
parallel=FALSE
silent=FALSE

Hist <- SimulateDEV(OM)


# # ---- Project Forward -----
source("OctopusMPs.R")

MPs=c('Open', 
      'Spatial_1',
      'Seasonal_1',
      'Spatial_2',
      'Spatial_3',
      'Spatial_6',
      'Spatial_12')
      

MSE <- ProjectDEV(OMListHist, MPs=c('Open', 
                                    'Spatial_1',
                                    'Seasonal_1',
                                    'Spatial_2',
                                    'Spatial_3',
                                    'Spatial_6',
                                    'Spatial_12'))



# # Make Figures 
# library(ggplot2)
# 
# ConvertToDF <- function(array, responseName ="Value") {
#   DF <- array2DF(array, responseName) 
#   nms <- names(DF)
#   if ("Sim" %in% nms)
#     DF$Sim <- as.numeric(DF$Sim)
#   
#   if ("TimeStep" %in% nms)
#     DF["TimeStep"] <- as.numeric(DF$TimeStep)
#   
#   if ("MP" %in% nms)
#     DF$MP <- factor(DF$MP, ordered = TRUE, levels=unique(DF$MP))
#   
#   if ("Stock" %in% nms)
#     DF$Stock <- factor(DF$Stock, ordered = TRUE, levels=unique(DF$Stock))
#   
#   if ("Fleet" %in% nms)
#     DF$Fleet <- factor(DF$Fleet, ordered = TRUE, levels=unique(DF$Fleet))
# 
#   DF
# }
# 
# # Make Data frames 
# TimeStepsHist <- OMListHist[[1]]$TimeStepsHist[[1]]
# TimeStepsProj <- OMListHist[[1]]$TimeStepsProj[[1]]
# 
# 
# SBiomassProj <- ConvertToDF(MSE$SBiomass)
# 
# SB0 <- purrr::map(OMListHist, \(x)
#                   x$SB0) |>
#   ReverseList() |>
#   purrr::map(List2Array, 'Sim', 'TimeStep') |>
#   purrr::map(\(x) apply(x, c('Sim', 'TimeStep'), sum)) |>
#   List2Array("Stock") |> 
#   aperm(c("Sim", "Stock", "TimeStep")) |>
#   ConvertToDF('SB0') |>
#   dplyr::filter(TimeStep%in% TimeStepsProj)
# 
# SBiomassProj <- dplyr::left_join(SBiomassProj, SB0)
#                
# SBiomass <- SBiomassProj |> 
#   dplyr::arrange("TimeStep", "Sim", "Stock") |>
#   dplyr::group_by(Sim, TimeStep) |>
#   dplyr::mutate(Value=Value/SB0) |>
#   dplyr::group_by(TimeStep, MP) |>
#   dplyr::mutate(Median=median(Value))
# 
# 
# ggplot(SBiomass, aes(x=TimeStep, y=Median)) +
#   geom_line() +
#   expand_limits(y=c(0,1)) +
#   facet_wrap(~MP) +
#   theme_bw() +
#   labs(x='Time', y='Relative Spawning Biomass')
# 
# 
# RefCatch <- purrr::map(OMListHist, \(x)
#                   x$RefCatch) |> unlist()
# 
# RefCatch <- data.frame(Sim=1:length(RefCatch), RefCatch)
# 
# 
# RemovalProj <- left_join(ConvertToDF(MSE$Removal),RefCatch) |>
#   dplyr::group_by(MP, Sim) |>
#   dplyr::mutate(Value=Value/RefCatch) |> 
#   dplyr::group_by(TimeStep, MP) |>
#   dplyr::mutate(Median=median(Value))
# 
# RemovalProj |> 
#   dplyr::filter(Sim==1, MP=='Open')
# 
# RemovalProj |> 
#   dplyr::filter(Sim==1, MP=='Closed_12')
# 
# 
# ggplot(RemovalProj, aes(x=TimeStep, y=Median)) +
#   geom_line() +
#   expand_limits(y=0) +
#   facet_wrap(~MP) +
#   theme_bw() +
#   labs(x='Time', y='Relative Catch')
# 
# 
# RemovalMean <- RemovalProj |> 
#   dplyr::group_by(MP) |>
#   dplyr::summarise(Mean=mean(Value))
# 
# SBiomassMean <- SBiomassProj |> 
#   dplyr::group_by(Sim, TimeStep) |>
#   dplyr::mutate(Value=Value/SB0) |>
#   dplyr::group_by(MP) |>
#   dplyr::summarise(Mean=mean(Value))
# 
# plot(SBiomassMean$Mean,  RemovalMean$Mean )
# 
# barplot(SBiomassMean$Mean, names.arg=SBiomassMean$MP)
# barplot(RemovalMean$Mean, names.arg=RemovalMean$MP)
# 
# 
# ggplot(RemovalProj, aes(x=TimeStep, y=Median, col=MP)) +
#   geom_line() +
#   expand_limits(y=0) +
#   theme_bw() +
#   labs(x='Time', y='Relative Catch')
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# BiomassProj <- ConvertToDF(MSE$Biomass)
# 
#  
# 
# B0 <- purrr::map(OMListHist, \(x)
#                   x$B0) |>
#   ReverseList() |>
#   purrr::map(List2Array, 'Sim', 'TimeStep') |>
#   purrr::map(\(x) apply(x, c('Sim', 'TimeStep'), sum)) |>
#   List2Array("Stock") |> 
#   aperm(c("Sim", "Stock", "TimeStep")) |>
#   ConvertToDF('B0') |>
#   dplyr::filter(TimeStep%in% TimeStepsProj)
# 
# BiomassProj <- dplyr::left_join(BiomassProj, B0)
# 
# Biomass <- BiomassProj |> 
#   dplyr::arrange("TimeStep", "Sim", "Stock") |>
#   dplyr::group_by(Sim, TimeStep) |>
#   dplyr::mutate(Value=Value/B0) |>
#   dplyr::group_by(TimeStep, MP) |>
#   dplyr::summarise(Biomass=median(Value))
# 
# ggplot(Biomass, aes(x=TimeStep, y=Biomass, col=MP)) +
#   geom_line() +
#   expand_limits(y=0) +
#   facet_wrap(~MP)
# 
# w = BiomassProj |> dplyr::filter(MP=="Open", TimeStep==2026)
# w$Value/w$B0
# 
# r = BiomassProj |> dplyr::filter(MP=="CloseArea_1_12", TimeStep==2026)
# r$Value/r$B0
# 
# w
# r
# 
# SBiomassProj
# 
# # final depletion isn't right!
# # why is depletion different across MPs?
# 
# MSE$Biomass[1,1,,1]
