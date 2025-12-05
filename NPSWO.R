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
# Age-0 is half the actual value, presumably because SS3 has 2 seasons for Age-0
# while OM has 4 seasons for all age-classes
RepList[[1]]$M_at_age[,4] <- RepList[[1]]$M_at_age[,4] * 2

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


################################################################################
replist <- RepList$`1`
yr <- 2021
fl <- 3
replist$catch |> dplyr::filter(Yr==yr, Seas==1, Fleet==fl)

sel_a1 <- OM@Fleet$Female[[fl]]@Selectivity@MeanAtAge[1,seq(1, by=4, to=62),185]
sel_a2 <- OM@Fleet$Male[[fl]]@Selectivity@MeanAtAge[1,seq(1, by=4, to=62),185]

# replist$catage |> DropXXCols() |> dplyr::filter(Yr==yr, Seas==1, Fleet==1)
sel_l <- replist$sizeselex |> dplyr::filter(Yr==yr, Fleet==fl, Factor=='Lsel')

# Sex 1
tempvec_l1 <- sel_l[1,6:56] * replist$biology$Wt_F
tempvec_l2 <- sel_l[2,6:56] * replist$biology$Wt_M

ALK <- replist$ALK[51:1,,1,drop=TRUE]

sel_bio1 <- sel_a1 * (as.numeric(tempvec_l1) %*% ALK) 
sel_bio2 <- sel_a2 * (as.numeric(tempvec_l2) %*% ALK) 

MatAge1 <- OM@Stock$Female@NaturalMortality@MeanAtAge[1,seq(1, by=4, to=62),1]
MatAge2 <- OM@Stock$Male@NaturalMortality@MeanAtAge[1,seq(1, by=4, to=62),1]

FatAge1 <- (replist$fatage |> dplyr::filter(Sex==1, Yr==yr, Seas==1))[,8:23]
FatAge2 <- (replist$fatage |> dplyr::filter(Sex==2, Yr==yr, Seas==1))[,8:23]

FatAge1 <- FatAge1/4
FatAge2 <- FatAge2/4
ZatAge1 <- colSums(FatAge1) + MatAge1
ZatAge2 <- colSums(FatAge2) + MatAge2

apZatAge1 <- max(ZatAge1)
apZatAge2 <- max(ZatAge2)

apicF1 <- max(FatAge1[fl,])
apicF2 <- max(FatAge2[fl,])

NAA1 <- (replist$natage |> dplyr::filter(Sex==1, Yr==yr, Seas==1,
                                        `Beg/Mid`=='B'))[13:28]

NAA2 <- (replist$natage |> dplyr::filter(Sex==2, Yr==yr, Seas==1,
                                         `Beg/Mid`=='B'))[13:28]

Ndead1 <- colSums(FatAge1)/ZatAge1 * (NAA1) * (1-exp(-ZatAge1))
Ndead2 <- colSums(FatAge2)/ZatAge2 * (NAA2) * (1-exp(-ZatAge2))

Ndead1_fl1 <- FatAge1[fl,]/ZatAge1 * (NAA1) * (1-exp(-ZatAge1))
Ndead2_fl2 <- FatAge2[fl,]/ZatAge2 * (NAA2) * (1-exp(-ZatAge2))

sum(Ndead1+Ndead2)
replist$catch |> dplyr::filter(Yr==yr, Seas==1) |>
  dplyr::reframe(n=sum(kill_num))
sum(Ndead1_fl1+Ndead2_fl2)
replist$catch |> dplyr::filter(Yr==yr, Seas==1, Fleet==fl) |>
  dplyr::reframe(n=sum(kill_num))

overallsel1 <- colSums(FatAge1)/max(colSums(FatAge1))
overallsel2 <- colSums(FatAge2)/max(colSums(FatAge2))

Zrate1 <- sum(Ndead1)/sum(NAA1 * overallsel1)
Zrate2 <- sum(Ndead2)/sum(NAA2 * overallsel2)

harv1 <- sum(Ndead1_fl1)/sum(NAA1 * overallsel1)
harv2 <- sum(Ndead2_fl2)/sum(NAA2 * overallsel2)

sum(harv1 * sum(NAA1)) +
  sum(harv2 * sum(NAA2))

c1 <- FatAge1[fl,]/ZatAge1 * (NAA1) * (1-exp(-ZatAge1))
c2 <- FatAge2[fl,]/ZatAge2 * (NAA2)  * (1-exp(-ZatAge2))
sum(c1) + sum(c2)
replist$catch |> dplyr::filter(Yr==yr, Seas==1, Fleet==fl)


sum(c1 * OM@Stock$Female@Weight@MeanAtAge[1,seq(1, by=4, 62),1] +
      c2 * OM@Stock$Male@Weight@MeanAtAge[1,seq(1, by=4, 62),1])

0.0171894* (NAA1 * sel_bio1[1,]) * (1-exp(-apZatAge1))



c1 <- apicF1/apZatAge1 * (NAA1 * sel_bio1[1,]) * (1-exp(-apZatAge1))
c2 <- apicF2/apZatAge2 * (NAA2 * sel_bio2[1,]) * (1-exp(-apZatAge2))

sum(c1) +
  sum(c2)

replist$timeseries |> dplyr::filter(Yr==yr, Seas==1)

replist$catch |> dplyr::filter(Yr==yr, Seas==1, Fleet==fl)


c1 <- FatAge1[1,]/apZatAge1 * (NAA1 * sel_bio1[1,]) * (1-exp(-apZatAge1))
c2 <- FatAge2[1,]/apZatAge2 * (NAA2 * sel_bio2[1,])  * (1-exp(-apZatAge2))

sum(c1) +
sum(c2)

replist$catch |> dplyr::filter(Yr==yr, Seas==1, Fleet==1)


# Sex 2


replist$biology 
replist$ALK |> dimnames()


replist$catch_units
OM@Data$`Female Male`@Landings@Units
OM@Data$`Female Male`@Landings@Value[,1:2] 

fl <- 3
# Calculate Catch-at-Length
ts <- match(2020, OM@Years)
area <- 1
NAA <- Hist@Number$Female[,ts,area]
ASK <- Hist@OM@Stock$Female@Length@ASK[,,1]

NAL <- NAA %*% ASK

SelL <- Hist@OM@Fleet$Female@Selectivity@MeanAtLength[,ts,fl]
WghtLen <- replist$biology$Wt_F

SelW <- SelL * WghtLen # line 2022 in SS3 SS_selex.tpl
SelW_a <- SelW %*% t(ASK)

SelL <- Hist@OM@Fleet$Male@Selectivity@MeanAtLength[,ts,fl]
WghtLen <- replist$biology$Wt_M
SelW <- SelL * WghtLen # line 2022 in SS3 SS_selex.tpl
SelW_a2 <- SelW %*% t( Hist@OM@Stock$Male@Length@ASK[,,1])


# TODO - add retention etc 
M_age <- Hist@OM@Stock$Female@NaturalMortality@MeanAtAge[,ts]
F_fleet <- Hist@FDead$Female[,ts,] 
F_this_fleet <- max(F_fleet[,fl])
F_total <- apply(F_fleet, 'Age', sum) |> max()
Z <- apply(F_fleet+M_age, 'Age', sum) |> max()

CAL <- NAL * SelL * F_this_fleet/Z * (1-exp(-Z))

sum(CAL)


replist$timeseries |> dplyr::filter(Yr==2020, Seas==1) |>
  dplyr::pull(paste0('F:_', fl)) /4

replist$timeseries |> dplyr::filter(Yr==2020, Seas==1) |>
  dplyr::select(paste0('F:_', 1:18)) |> sum() / 4

# Calculate Catch Biomass

# Compare with replist catch biomass

replist$catch |> dplyr::filter(Yr==2020, Seas==1, Fleet==fl)
Hist@LandingsAtAge$Female$`2020`[,fl,1] |> sum() +
Hist@LandingsAtAge$Male$`2020`[,fl,1] |> sum()

sum(Hist@LandingsAtAge$Female$`2020`[,fl,1] * SelW_a) +
sum(Hist@LandingsAtAge$Male$`2020`[,fl,1] *SelW_a2) # should match replist bio
# but doesn't ...

SelA1 <- Hist@OM@Fleet$Female@Selectivity@MeanAtAge[,ts,fl]
SelA2 <- Hist@OM@Fleet$Male@Selectivity@MeanAtAge[,ts,fl]

sum(SelA1 * SelW_a)
sum(SelA2 * SelW_a2)

165/217

Hist <- SimList$`1`

replist$sizeselex$Label |> unique()


################################################################################

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
