
library(MSEtool)

la <- devtools::load_all

la()

# ---- Operating Model ----
OM <- readRDS("C:/Users/Admin/Documents/GitHub/IndonesiaHarvestStrategies/Octopus/octopus.om")

octopus_obs <- Obs()
octopus_obs@Landings@CV <- 0.3
octopus_obs@CPUE@CV <- 0.3
octopus_obs@CPUE@Ref <- 0.001 # either nsim 
octopus_obs@Effort@CV <- 0.001

octopus_obs@CAL@ESS <- 200

Obs(OM) <- octopus_obs


# ---- Simulate Historical Fishery ----
Hist <- Simulate(OM, nSim=5)


Data <- Hist@Data$`1`$`Day octopus`



sim <- 2
par(mfrow=c(1,2))
plot(MSE@Effort[sim,1,,1,1], type='l')
plot(MSE@PPD$Eff10$`1`$`Day octopus`@TimeSteps,MSE@PPD$Eff10$`1`$`Day octopus`@CPUE@Value[,1], type='l')
abline(h=MSE@PPD$Eff10$`1`$`Day octopus`@CPUE@Ref, lty=2)

Data <- MSE@PPD$Eff10$`1`$`Day octopus`

Biomass(MSE) |> dplyr::group_by(TimeStep) |>
  dplyr::summarise(Value=mean(Value)) |>
  ggplot(aes(x=TimeStep, y=Value)) +
  geom_line() +
  theme_bw()

Landings(MSE) |> dplyr::group_by(TimeStep) |>
  dplyr::summarise(Value=mean(Value)) |>
  ggplot(aes(x=TimeStep, y=Value)) +
  geom_line() +
  theme_bw()


f = MSE@FDeadAtAge$`Day octopus` |> apply(c(1,3), max)

matplot(t(f), type='b')


sim <- 3
par(mfrow=c(1,1))
df = Biomass(MSE) |> dplyr::filter(Sim==sim)
plot(MSE@PPD$Eff10[[sim]]$`Day octopus`@TimeSteps, MSE@PPD$Eff10[[sim]]$`Day octopus`@CPUE@Value[,1], type='l')
abline(h=MSE@PPD$Eff10[[sim]]$`Day octopus`@CPUE@Ref, lty=2)

lines(df$TimeStep, df$Value/mean(df$Value), col='blue')
abline(h=Hist@RefPoints@MSYRefPoints@BMSY[sim,1,1]/mean(df$Value), lty=2, col='blue')


Data <- Hist@Data$`1`$`Day octopus`






Hist <- readRDS("C:/Users/Admin/Documents/GitHub/IndonesiaHarvestStrategies/Octopus/octopus.hist")


source("C:/Users/Admin/Documents/GitHub/IndonesiaHarvestStrategies/HarvestStrategies.r")

## ---- Status Quo ----
MPs="StatusQuo"
parallel <- FALSE
silent=FALSE 
nSim = NULL

MSE_SQ <- Project(Hist=Hist, MPs="StatusQuo")


MPs <- c('MW400',
             'MW500',
             'MW600',
             'MW700',
             'MW800')

MSE_MSL <- Project(Hist=Hist, MPs=MPs)


CS_MPs <- MPs <-  c('CS01',
                    'CS03',
                    'CS06',
                    'CS09')

MSE_CS <- Project(Hist=Hist, MPs=CS_MPs)


OM_10 <- OM

Spatial(OM_10@Stock) <- Spatial(UnfishedDist=c(0.1, 0.1),
                                ProbStaying=c(0.9, 0.95),
                                RelativeSize='EqualDensity')

Catchability <- Hist@OM@Fleet[[1]]@Effort@Catchability
Catchability <- MSEtool:::ArraySubsetTimeStep(Catchability, TimeSteps=TimeSteps(OM, 'Historical'))

OM_10@Fleet@Effort@Catchability <- Catchability[,,1]

Hist_10 <- Simulate(OM_10, nSim=5)

Spatio_Temporal_MPs <- MPs <- c('SC01',
                         'SC03',
                         'SC06',
                         'SC09',
                         'SC12')

MSE_ST <- Project(Hist=Hist_10, MPs=Spatio_Temporal_MPs)

MSE_ST@EffortArea$`Day octopus`[1,,1,,2]







matplot(MSE_CS@Effort[1,1,,1,] |> ExpandTimeSteps(TimeSteps(OM,'Projection')), type='b')

plot(MSE_CS@Effort[1,1,,1,1], type='b')
plot(MSE_CS@Effort[1,1,,1,2], type='b')
plot(MSE_CS@Effort[1,1,,1,3], type='b')
plot(MSE_CS@Effort[1,1,,1,4], type='b')


Biomass(MSE_CS) |>dplyr::group_by(TimeStep, MP) |> dplyr::summarise(Value=mean(Value)) |>
  ggplot(aes(x=TimeStep, y=Value, color=MP)) + 
  geom_line()


df = dplyr::bind_rows(Biomass(MSE_SQ), Biomass(MSE_MSL), Biomass(MSE_CS)) |> 
  dplyr::group_by(TimeStep, MP) |> dplyr::summarise(Value=mean(Value))

ggplot(df, aes(x=TimeStep, y=Value, color=MP)) + 
  geom_line()


df = dplyr::bind_rows(Landings(MSE_SQ), Landings(MSE_MSL)) |> 
  dplyr::group_by(TimeStep, MP) |> dplyr::summarise(Value=mean(Value))

ggplot(df, aes(x=TimeStep, y=Value, color=MP)) + 
  facet_wrap(~MP) +
  geom_line()
