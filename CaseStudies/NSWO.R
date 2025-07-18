library(MSEtool)
library(ggplot2)
library(openMSE)

la <- devtools::load_all

la()


nsim <- 5
SSdir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/grid_2022/000_base_case'

MOM <- SS2MOM(SSdir=SSdir,nsim=nsim, Name='North Atlantic Swordfish') 
MOM@nsim <- nsim

# hack to fix discard mortality reverting to 0 in projections for some fleets
for (st in 1:2) {
  for (fl in 1:length(MOM@cpars[[1]])) {
    # d <- dim(MOM@cpars[[st]][[fl]]$Fdisc_array1)
    MOM@cpars[[st]][[fl]]$Fdisc_array1[,,72:121] <- MOM@cpars[[st]][[fl]]$Fdisc_array1[,,71, drop=FALSE]
    MOM@cpars[[st]][[fl]]$Fdisc_array2[,,72:121] <- MOM@cpars[[st]][[fl]]$Fdisc_array2[,,71, drop=FALSE]
  }
}


OMImport <- ImportSS(SSdir, nsim)
OMConvert <- Convert(MOM)


HistImport <- SimulateDEV(OMImport)  
HistConvert <- SimulateDEV(OMConvert)
HistOld <- Simulate(MOM)


NumberOld <- get_Number_at_Age(HistOld) |>
  dplyr::mutate(TimeStep=Year, Stock=Model, Model='Old') |>
  dplyr::group_by(Stock, TimeStep, Model, Sim) |>
  dplyr::summarise(Value=sum(Value))

replist <- r4ss::SS_output(SSdir)
mainyrs <- replist$startyr:replist$endyr
AgeClasses <- GetSSAgeClasses(replist$natage)
SSN <- replist$natage[,as.character(AgeClasses)]

NatAge <- replist$natage |> 
  dplyr::filter(Yr%in%mainyrs, `Beg/Mid`=='B') |>
  dplyr::rename(TimeStep=Yr, Stock=Sex) |>
  tidyr::pivot_longer(cols=as.character(AgeClasses)) |>
  dplyr::group_by(Stock, TimeStep) |>
  dplyr::summarise(Value=sum(value), Model='SS3') |>
  dplyr::mutate(Sim=1)

NatAge$Stock <- StockNames(OMImport)[NatAge$Stock]
  


df <- dplyr::bind_rows(
  Number(HistImport) |> dplyr::mutate(Model='Import'),
  Number(HistConvert) |> dplyr::mutate(Model='Convert'),
  NumberOld,
  NatAge
) |> dplyr::filter(Sim==1)

ggplot(df, aes(x=TimeStep, y=Value, color=Model)) +
  facet_grid(~Stock) +
  geom_line() +
  theme_bw()

df |> dplyr::filter(TimeStep==1992) |>
  dplyr::select(Stock, Model, Value) |>
  tidyr::pivot_wider(names_from = Model, values_from = Value)


##############################################################################
# Fix Import!

# something to do with selectivity, retention, and discard mortality!!

t <- which(TimeSteps(OMImport) == 1994)
HistOld$Female$SPN_1@AtAge$F.Mortality[1,,t,1] |> plot()
HistImport@FDeadAtAge$Female[1,,t,1] |> lines()

HistOld$Female$SPN_1@AtAge$F.Mortality[1,,t,1] |> max()
HistImport@FDeadAtAge$Female[1,,t,1] |> max()


HistImport@OM@Fleet$Female@Effort@Effort[1,t,1] *
HistImport@OM@Fleet$Female@Effort@Catchability[1,t,1] 
HistImport@OM@Fleet$Female@Selectivity@MeanAtAge[1,,t,1] |> plot()
HistImport@OM@Fleet$Female@Retention@MeanAtAge[1,,t,1] |> lines()
lines(MOM@cpars$Female$SPN_1$retA[1,,t], col='blue')



d <- HistImport@Removals$Female - HistImport@Landings$Female
sum(d[1,,t,1,1])
sum(HistOld$Female$SPN_1@TSdata$Discards[1,t,])


################################################################################


# TODO - Fix Male N is Convert
yr <- 2
sum(HistImport@Number$Male[1,,yr,1])
sum(HistConvert@Number$Male[1,,yr,1])

################################################################################

SS3Removals <- replist$catch |> dplyr::filter(Yr %in% mainyrs) |>
  dplyr::select(TimeStep=Yr,  Fleet, Value=kill_bio) |>
  dplyr::mutate(Model='SS3')

SS3Removals$Fleet <- FleetNames(OMImport)[SS3Removals$Fleet]
SS3Removals$Sim <- 1

df <- dplyr::bind_rows(
  Removals(HistImport) |> dplyr::mutate(Model='Import'),
  Removals(HistConvert) |> dplyr::mutate(Model='Convert'),
  SS3Removals
) |> dplyr::filter(Sim==1) |>
  dplyr::group_by(TimeStep, Model, Fleet) |>
  dplyr::summarise(Value=sum(Value))

df <- df |> dplyr::filter(Model != 'Convert')

ggplot(df, aes(x=TimeStep, y=Value, color=Model)) +
  facet_wrap(~Fleet, ncol=3, scales='free') +
  geom_line() +
  theme_bw()


SS3Landings <- replist$catch |> dplyr::filter(Yr %in% mainyrs) |>
  dplyr::select(TimeStep=Yr,  Fleet, Value=ret_bio) |>
  dplyr::mutate(Model='SS3')

SS3Landings$Fleet <- FleetNames(OMImport)[SS3Landings$Fleet]
SS3Landings$Sim <- 1

df <- dplyr::bind_rows(
  Landings(HistImport) |> dplyr::mutate(Model='Import'),
  Landings(HistConvert) |> dplyr::mutate(Model='Convert'),
  SS3Landings
) |> dplyr::filter(Sim==1) |>
  dplyr::group_by(TimeStep, Model, Fleet) |>
  dplyr::summarise(Value=sum(Value))

df <- df |> dplyr::filter(Model != 'Convert')

ggplot(df, aes(x=TimeStep, y=Value, color=Model)) +
  facet_wrap(~Fleet, ncol=3, scales='free') +
  geom_line() +
  theme_bw()














for (st in 1:2) {
  for (fl in 1:11) {
    OM@Fleet[[st]][[fl]]@Retention <- OMTEST@Fleet[[st]][[fl]]@Retention
  }
}

Hist1 <- SimulateDEV(OM)


# ---- Multi Stock & Multi Fleet (MOM) ----



# TODO Import - new SS2MOM for new OM structure 


# Compare MOM and OM
Hist2 <- Simulate(MOM) # takes a long time!


OMTEST <- Convert(MOM)  # convert from `MOM` to `om`
Hist3 <- SimulateDEV(OMTEST)




# Biomass
b1 <- Hist1@Biomass[1,,] |> apply('TimeStep', sum, na.rm=TRUE)
b2 <- apply(Hist2$Female$SPN_1@TSdata$Biomass[1,,]+Hist2$Male$SPN_1@TSdata$Biomass[1,,], 1, sum)
b3 <- Hist3@Biomass[1,,] |> apply('TimeStep', sum, na.rm=TRUE)

b = replist$timeseries |> dplyr::filter(Yr %in% mainyrs) |>
  dplyr::select(Year=Yr, Biomass=Bio_all)

bdf <- data.frame(Year=b$Year, SS=b$Biomass, Import=b1, Old=b2, Convert=b3) |>
  tidyr::pivot_longer(cols=c('SS', 'Import', 'Old', 'Convert'))


bdf <- bdf |> dplyr::filter(name!='Convert')

ggplot(bdf, aes(x=Year, y=value, color=name)) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw()


# Removals & Landings 
rSS <-  replist$catch %>% dplyr::filter(Yr %in% mainyrs) %>%
  dplyr::select(Year=Yr, Fleet=Fleet, C=all_of('kill_bio'), Seas = Seas) %>%
  dplyr::group_by(Year, Fleet) %>% 
  dplyr::summarise(C = sum(C), .groups='drop')


rSS <- data.frame(Sim=1, TimeStep=rSS$Year, FleetNumber=rSS$Fleet, Variable='Removals', Value=rSS$C, Model='SS3')

rSS$Fleet <- FleetNames(OM)[rSS$FleetNumber]



r1 <- Removals(Hist1) |> dplyr::filter(Sim==1) |>
  dplyr::mutate(Model='Import') |>
  dplyr::group_by(TimeStep, Fleet, Variable, Model) |>
  dplyr::summarise(Value=sum(Value))

library(openMSE)
r2 <- openMSE::get_Removals(Hist2) |> dplyr::filter(Sim==1) |>
  dplyr::mutate(Model='Old') |>
  dplyr::mutate(TimeStep=Year) |>
  dplyr::group_by(TimeStep, Fleet, Variable, Model) |>
  dplyr::summarise(Value=sum(Value))



r3 <- Removals(Hist3) |> dplyr::filter(Sim==1) |>
  dplyr::mutate(Model='Convert') |>
  dplyr::group_by(TimeStep, Fleet, Variable, Model) |>
  dplyr::summarise(Value=sum(Value))


DF <- dplyr::bind_rows(rSS, r1, r2, r3) |> 
  dplyr::filter(Model!='Convert')



ggplot(DF, aes(x=TimeStep, y=Value, color=Model)) +
  facet_wrap(~Fleet, scales='free_y', ncol=3) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw()


v1 <- Hist2$Female$HRPN_10@SampPars$Fleet$V_real_2[1,,1]
v2 <- Hist1@OM@Fleet$Female@Selectivity@MeanAtAge[1,,1,10]
v3 <- Hist3@OM@Fleet$Female@Selectivity@MeanAtAge[1,,1,10]

plot(v1, type='l')
lines(v2, col='blue')
lines(v3, col='red')


r2 <- Hist1@OM@Fleet$Female@Retention@MeanAtAge[1,,71,1]
r3 <- Hist3@OM@Fleet$Female@Retention@MeanAtAge[1,,71,1]

plot(r2, type='l', ylim=c(0,1))
lines(r3, col='blue')



c1 <- rowSums(Hist$Female$HRPN_10@TSdata$Removals[1,,])
c2 <- apply(Hist1@Removals$Female[1,,,10,1], 2, sum)

plot(c1, type='l')
lines(c2, col='blue')

Hist1@Removals$Female[1,,1,1,1]  |> plot()
Hist3@Removals$Female[1,,1,1,1] |> lines()

Hist1@FDeadAtAge$Female[1,,30,1]  |> plot()
Hist3@FDeadAtAge$Female[1,,30,1] |> lines()


