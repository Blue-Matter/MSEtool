# TODO - fix data lag for seasonal models 


library(MSEtool)
la()

# ----- NPSWO Initial Demo Code ----

# TODO:
# - calculate MSY ref points in seasonal model
# - develop method to distribute TAC over seasons
# - check historical and projectd indices
# - check fleet allocation 


# File path to the SS3 files in WCNPOSWO-2023
SSDir <- '../WCNPOSWO-2023/Final Base-case'

# Import the SS3 Output
RepList <- ImportSSReport(SSDir)

# Correct M-at-Age for initial age class
# for some reason, M for age-0 is exactly half the actual value
RepList[[1]]$M_at_age[,4] <- RepList[[1]]$M_at_age[,4] * 2

# some catches are reported in numbers but this appears to be an error
RepList$`1`$catch_units[] <- 1

Biomass
# Meta-data
Name <- 'North Pacific Swordfish'
StockName <- c("Female", 'Male')
Species <- "Xiphias gladius"
Region <- 'North Pacific'

# OM Settings - to be updated
Interval <- 3 # Management Interval
DataLag <- 1 # data lagged by 1 year?
nSim <- 2 # small for demo
pYear <- 30 # number of projection years

# Generate OM from SS3 output
OM <- ImportSS(RepList,
               Name,
               nSim,
               pYear,
               Region=Region,
               StockName=StockName,
               Species=Species,
               Interval=Interval,
               DataLag=DataLag)


# Simulate Historical Fishery
Hist <- Simulate(OM)

# Compare OM Dynamics with SS3 Output

# slight mismatch - due to 
CompareSS_Number(RepList, Hist)
CompareSS_Biomass(RepList, Hist)
CompareSS_Removals(RepList, Hist)
CompareSS_Landings(RepList, Hist)

HistLandings <- Landings(Hist, byFleet=TRUE) |> 
  dplyr::filter(Period=='Historical', Sim==1) |>
  dplyr::filter(Year==max(Year)) |>
  dplyr::group_by(Fleet) |>
  dplyr::summarise(Value=sum(Value))

data.frame(HistLandings, 
           ObsData=OM@Data$`Female Male`@Landings@Value[188,])



replist <- RepList$`1`

number <- CompareSS_Number(RepList, Hist)
biomass <- CompareSS_Biomass(RepList, Hist)
landings <- CompareSS_Landings(RepList, Hist)

number |> dplyr::filter(Year==max(Year))
biomass |> dplyr::filter(Year==max(Year))
landings |> dplyr::filter(Year==max(Year), Fleet=='F19_WCPFC')

SSAgeClasses <- GetSSAgeClasses(replist)

SS_F <- replist$fatage |> dplyr::filter(Sex==1, Yr==2000, Seas==1) |>
  dplyr::select(as.character(SSAgeClasses)) |> t()
  
ind <- match(2000, Years(OM,'H'))
OM_F <- Hist@FDead$Female[1,,ind,]

replist$growthseries |> head()

round(SS_F[,1]/4,3)
round(OM_F[,1],3)
CompareSS_Landings

OM@Fleet$Female$F10_JPN_WCNPO_OSDF@WeightFleet[1,,1]


ImportSS


Hist <- Simulate(OM)

CompareSS_Number(RepList, Hist)
CompareSS_Biomass(RepList, Hist)
CompareSS_Landings(RepList, Hist)

HistLandings <- Landings(Hist, byFleet=TRUE) |> 
  dplyr::filter(Period=='Historical', Sim==1) |>
  dplyr::filter(Year==max(Year)) |>
  dplyr::group_by(Fleet) |>
  dplyr::summarise(Value=sum(Value))

data.frame(HistLandings, 
           ObsData=OM@Data$`Female Male`@Landings@Value[YearLH_Ind,])



Hist@OM@Obs$`Female Male`$F1_JPN_WCNPO_OSDWCOLL_late_Area1@Landings@Bias[1,1]
Hist@OM@Obs$`Female Male`$F1_JPN_WCNPO_OSDWCOLL_late_Area1@Landings@Error[188]




ConstantEffort <- function(Data) {
  advice <- Advice()
  advice@Effort <- 1
  advice
}
class(ConstantEffort) <- 'mp'

Data <- Hist@Data$`1`$`Female Male`

ConstantCatch <- function(Data) {
  advice <- Advice()
  lastHistTS <- max(Data@Years[Data@Years< Data@YearLH+1])
  YearLH_Ind <- match(lastHistTS, Data@Years)
  LastHistoricalCatch <- Data@Landings@Value[YearLH_Ind ,] 
  advice@TAC <- LastHistoricalCatch
  advice
}
class(ConstantCatch) <- 'mp'

MSE <- Project(Hist, MPs=c('ConstantEffort',
                           'ConstantCatch'))



Landings <- Landings(MSE, byFleet=TRUE) |>
  dplyr::filter(Sim==1) |>
  dplyr::group_by(Year, Fleet, Period, MP) |>
  dplyr::summarise(Value=sum(Value))
  

ggplot(Landings, aes(x=Year, y=Value, color=Period)) +
  facet_grid(Fleet~MP, scales='free_y') +
  geom_line() +
  expand_limits(y=0) +
  theme_bw()


YearLH_Ind <- match(Data@YearLH, Data@Years)
LastHistoricalCatch <- Data@Landings@Value[YearLH_Ind ,] 

HistLandings <- Landings(Hist, byFleet=TRUE) |> 
  dplyr::filter(Period=='Historical', Sim==1) |>
  dplyr::filter(Year==max(Year)) |>
  dplyr::group_by(Fleet) |>
  dplyr::summarise(Value=sum(Value))


data.frame(OM=HistLandings, 
           ObsData=OM@Data$`Female Male`@Landings@Value[YearLH_Ind,],
           SimData=LastHistoricalCatch) 


Hist@OM@Obs$`Female Male`$F3_US_WCNPO_LL_shallow_late@Landings@Error[1,188]







Hist@RefPointsMSY@FMSY

"SSB_MSY"
"SPR_MSY"
"annF_MSY"
"Dead_Catch_MSY"

RepList$`1`$derived_quants$Label |> unique()

MeanAtLength




# TODO
# - check if catches are really in numbers for some fleets
# - check projected catches under constant effort/catch scenarios - should be on same scale as historical
# - test SALB
# - test NSWO





# Check projected catches - data and real 
# Check conditioning for catch in numbers 
Hist@OM@Obs$`Female Male`$F1_JPN_WCNPO_OSDWCOLL_late_Area1@Landings@Error[1,]
OM@Data$`Female Male`@Landings@Value[,1]

LoadArgs('Project_hist')

replist <- RepList[[1]]

CompareSSNumber(replist, Hist)

CompareSSLandings(replist, Hist)
