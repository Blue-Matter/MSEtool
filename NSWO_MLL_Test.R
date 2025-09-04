library(MSEtool)
library(SWOMSE)

la <- devtools::load_all
la()

library(SWOMSE)
SSDir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/2024_OMs/Reference/005_M0.2_sigmaR0.2_steepness0.80_cpuelambda1_llq1_env7'

OM <- ImportSS(SSDir, nSim=3)

Hist <- Simulate(OM)

source('C:/Users/Admin/Documents/GitHub/nswo-mse/Additional_Robustness_Tests/MinimumSizeLimits/MLL_MPs.R')

MSE <- Project(Hist, MPs='MCC11_FR')


MSE <- readRDS('C:/Users/Admin/Documents/GitHub/nswo-mse/Additional_Robustness_Tests/MinimumSizeLimits/FullRetention.mse')

l <- Landings(MSE, ByFleet=TRUE)
d <- Discards(MSE,  ByFleet=TRUE)



# by MP 
RetentionAtAge <- GetRetentionAtAge(MSE) 










Project





SSDir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/2024_OMs/Reference/005_M0.2_sigmaR0.2_steepness0.80_cpuelambda1_llq1_env7'

RepList <- ImportSSReport(SSDir)

nSim <- 5
OM <- ImportSS(RepList, nSim=nSim, DataLag = 2)
OM@Obs$`Female Male`$Combined_CPUE@Survey@TimeSteps <- 1999:2020

Hist <- Simulate(OM)



sel <- Hist@OM@Fleet$Female@Selectivity@MeanAtAge[1,,1,1]
ret <- Hist@OM@Fleet$Female@Retention@MeanAtAge[1,,1,1]
disc <- Hist@OM@Fleet$Female@DiscardMortality@MeanAtAge[1,,1,1]

plot(sel)
lines(sel * (ret + (1-ret)*disc))
lines(ret, col='blue')


CompareSSLandings(RepList[[1]], Hist)

CompareSSRemovals(RepList[[1]], Hist)

l <- Hist@Landings$Female + Hist@Landings$Male
d <- Hist@Discards$Female + Hist@Discards$Male

apply(d[1,,,,1], 2:3, sum)[,1] + 
apply(l[1,,,,1], 2:3, sum)[,1]

OM@Data$`Female Male`@Discards@Value[,1] + OM@Data$`Female Male`@Landings@Value[,1]

ArrayReduceDims(Hist@OM@Fleet$Female@Selectivity@MeanAtAge)[1,,,1]
ArrayReduceDims(Hist@OM@Fleet$Female@Retention@MeanAtAge)[1,,,1]

ArrayReduceDims(Hist@OM@Fleet$Female@Selectivity@MeanAtLength)[1,,1,1]  |> plot()
ArrayReduceDims(Hist@OM@Fleet$Female@Retention@MeanAtLength)[1,,1,1] |> lines()


ArrayReduceDims(Hist@OM@Fleet$Female@Selectivity@MeanAtAge)[1,,1,1]  |> plot()
ArrayReduceDims(Hist@OM@Fleet$Female@Retention@MeanAtAge)[1,,1,1] |> lines()


ArrayReduceDims(Hist@OM@Fleet$Female@DiscardMortality@MeanAtAge)[1,,,1]


Removals

Landings

Discards 




L <- Landings(Hist, TRUE) |> dplyr::filter(Sim==1, TimeStep==max(TimeStep)) |>
  dplyr::group_by(Fleet) |>
  dplyr::filter(Value>0)

KeepFleets <- unique(L$Fleet)

SelectivityAtAge <- GetSelectivityAtAge(Hist) |> dplyr::filter(TimeStep==max(TimeStep), Fleet %in% KeepFleets)
SelectivityAtLength <- GetSelectivityAtLength(Hist) |> dplyr::filter(TimeStep==max(TimeStep), Fleet %in% KeepFleets)
RetentionAtAge <- GetRetentionAtAge(Hist) |> dplyr::filter(TimeStep==max(TimeStep), Fleet %in% KeepFleets)
RetentionAtLength <- GetRetentionAtLength(Hist) |> dplyr::filter(TimeStep==max(TimeStep), Fleet %in% KeepFleets)

DF <- dplyr::bind_rows(SelectivityAtAge, RetentionAtAge)
plot(DF |> dplyr::filter(Stock=='Female'), color='Variable', ylab='Probability')

DF <- dplyr::bind_rows(SelectivityAtLength, RetentionAtLength)
plot(DF |> dplyr::filter(Stock=='Female'), color='Variable', xlab='Length', ylab='Probability')

l <- Landings(Hist, ByAge=TRUE) |>
  dplyr::filter(TimeStep==max(TimeStep), Sim==1) |>
  dplyr::group_by(Age, TimeStep, Variable) |>
  dplyr::reframe(Value=sum(Value))

r <- Removals(Hist, ByAge=TRUE) |> 
  dplyr::filter(TimeStep==max(TimeStep), Sim==1) |>
  dplyr::group_by(Age, TimeStep, Variable) |>
  dplyr::reframe(Value=sum(Value))

d <- r
d$Value <- r$Value - l$Value
d$Variable <- 'Dead Discards'

ggplot(dplyr::bind_rows(l,d), aes(x=Age, y=Value, fill=Variable)) +
  geom_bar(stat='identity') +
  theme_bw() +
  labs(fill='')




MPs <- c('MCC11', 'MCC11_FR')

MSE <- Project(Hist, MPs)

Data <- DataTrim(MSE@PPD$MCC11$`1`$`Female Male`, 2024)
Data@Survey@Value[,8]

MSE@PPD$MCC11$`1`$`Female Male`@Survey@Value[,8]

MSE@PPD$MCC11$`1`$`Female Male`@TAC

b <- Biomass(MSE) |> dplyr::filter(Sim==1) |>
  dplyr::group_by(TimeStep, MP) |>
  dplyr::summarise(Value=sum(Value))

ggplot(b, aes(x=TimeStep, y=Value, color=MP)) +
  
  geom_line() +
  theme_bw()

l <- Landings(MSE, ByFleet=TRUE) |> dplyr::filter(Sim==1) |>
  dplyr::group_by(TimeStep, Fleet, MP) |>
  dplyr::summarise(Value=sum(Value))

ggplot(l, aes(x=TimeStep, y=Value, color=MP)) +
  facet_wrap(~Fleet) +
  geom_line() +
  theme_bw()

l |> dplyr::filter(TimeStep==2022) |> print(n=30)



# Run 2 - Shift Selectivity Curves to Left 

Hist@OM@Fleet$Female@Selectivity@MeanAtAge 
Hist@OM@Fleet$Female@Selectivity@MeanAtLength |> dim()

devtools::load_all()

object <- OM@Fleet$Female[[3]]@Selectivity 
Length <- Hist@OM@Stock$Female@Length

Length@Classes <- object@Classes
Length@ASK <- CalcAgeSizeKey(Length@MeanAtAge, Length@CVatAge, Length@Classes)
r <- AtAge2AtSize(object, Length) |> ArrayReduceDims()

plot(object@Classes, object@MeanAtLength[1,,2] , ylim=c(0,1), type='l')
lines(Length@Classes, r[1,,2], col='blue')



t <- AtSize2AtAge(object, Length)
plot(object@MeanAtAge[1,,2] , type='l', ylim=c(0,1))
lines(t[1,,2], type='l', col='blue')

