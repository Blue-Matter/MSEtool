
la <- devtools::load_all
la()


dir <- "C:/Users/Admin/Documents/GitHub/SAFMC-MSE"

source(file.path(dir,'0. Specifications.R'))

OM_Dir <- file.path(dir, 'OM_Objects/Base')


OM_RS <- ImportBAM('RedSnapper', nSim=nSim, pYear=pYear)
CompareBAM('RedSnapper', OM=OM_RS)

# Red Snapper - latest


OM_GG <- ImportBAM(Stock='GagGrouper', nSim=nSim, pYear=pYear)
CompareBAM('GagGrouper', OM=OM_GG) 


DiscMortDF <- data.frame(Fleet=c('cHL', 'cPT', 'cPT', 'rHB', 'rGN'),
                         Value=c(0.19,   0.14, 0.068, 0.152, 0.137),
                         Year= c(1977, 1977, 2007, 1977, 1977))

OM_BSB <- ImportBAM(Stock='BlackSeaBass', nSim=nSim, pYear=pYear, DiscMortDF)
CompareBAM('BlackSeaBass', OM=OM_BSB, ConvertUnits = 0.453592)


OM_GA <- ImportBAM(Stock='GreaterAmberjack', nSim=nSim, pYear=pYear)
CompareBAM('GreaterAmberjack', OM=OM_GA)

OM_TF <- ImportBAM(Stock='Tilefish', nSim=nSim, pYear=pYear)
CompareBAM('Tilefish', OM=OM_TF)

OM_SCG <- ImportBAM(Stock='ScampGrouper', nSim=nSim, pYear=pYear)
CompareBAM('ScampGrouper', OM=OM_SCG)

####################### Minor Differences ######################################


## ---- SnowyGrouper -----

# Nage.spawn is calculated differently than expected in first year

OM_SG <- ImportBAM(Stock='SnowyGrouper', nSim=nSim, pYear=pYear)
CompareBAM('SnowyGrouper', OM=OM_SG)





data.frame(M_spawn_expected, M_spawn_actual)


## ---- RedGrouper -----

OM_RG <- ImportBAM(Stock='RedGrouper', nSim=nSim, pYear=pYear)
CompareBAM('RedGrouper', OM=OM_RG)

## ---- VermilionSnapper -----

OM_VS <- ImportBAM(Stock='VermilionSnapper', nSim=nSim, pYear=pYear)
CompareBAM('VermilionSnapper', OM=OM_VS)


####################### TO BE FIXED ############################################

OM_RP <- ImportBAM(Stock='RedPorgy', nSim=nSim, pYear=pYear)
CompareBAM('RedPorgy', OM=OM_RP)


DiscMortDF <- data.frame(Fleet=c('cHLs', 'rHBs', 'rGNs', 'rGNn'),
                         Value=c(0.589),
                         Year= c(1981))
OM_GT <- ImportBAM(Stock='GrayTriggerfish', nSim=nSim, pYear=pYear, DiscMortDF=DiscMortDF)
CompareBAM('GrayTriggerfish', OM=OM_GT)


### ----- WORKSHOP ------

Stock <- 'SnowyGrouper'

OM <- ImportBAM(Stock, nSim=nSim, pYear=pYear)
CompareBAM(Stock, OM)


BAMdata <- GetBAMOutput(Stock)

M_spawn_expected <- (BAMdata$a.series$M + BAMdata$F.age[1,]) * BAMdata$parms$spawn.time
M_spawn_actual <- -log(BAMdata$N.age.spawn[1,]/BAMdata$N.age[1,])


OM2 <- OM
OM2@Misc$SpawnMortality <- list()
OM2@Misc$SpawnMortality[[1]] <- as.numeric(M_spawn_actual)

CompareBAM(Stock, OM2)

Hist <- Simulate(OM, nsim=1)
Hist2 <- Simulate(OM2, nsim=1)

### TODO - Rec Devs needs to be fixed 

yr <- 1
cbind(BAMdata$N.age[yr,], Hist2@Number$`SA Snowy Grouper`[1,,yr,1])




################################################################################

data.frame(Expected, Actual)



OM@Misc$SpawnMortality <- list()
OM@Misc$SpawnMortality[[1]] <- as.numeric(M_spawn_actual)

Hist <- Simulate(OM, nsim=1)

CompareBAM(Stock, OM)



BAMdata <- GetBAMOutput(Stock)

ts <- 2
plot(BAMdata$N.age[ts,], type='l')
lines(Hist@Number[[1]][1,,ts,1], col='blue')


BAMdata$N.age[ts,1]
Hist@Number[[1]][1,1,ts,1]


plot(BAMdata$t.series$SSB, type='l')
lines(Hist@SProduction[1,1,], col='blue')

BAMdata$t.series$SSB[1]
sum(BAMdata$N.age.spawn[1,] * BAMdata$a.series$reprod)


Hist@Number$`SA Snowy Grouper`[1,,1,1] * Hist@OM@Stock$`SA Snowy Grouper`@Fecundity@MeanAtAge[1,,1]


# Match: BAMdata$N.age.spawn[1,]



yr <- 1


df <- data.frame(Age=BAMdata$a.series$age,
                 NAgeSpawn=log(BAMdata$N.age.spawn[yr,]),
                 NAgeSpawnCalc=log(BAMdata$N.age[yr,] * exp(-M_spawn))) |>
  tidyr::pivot_longer(cols=c(NAgeSpawn, NAgeSpawnCalc))

ggplot(df, aes(x=Age, y=value, color=name )) +
  geom_line()


m2 <- -log(BAMdata$N.age.spawn[1,]/BAMdata$N.age[1,])

plot(mm, type='l')
lines(m2, col='blue')





Hist@SProduction[1,1,]

cbind(BAMdata$N.age[1,], BAMdata$N.age.spawn[1,])
apply(Hist@FDeadAtAge$`SA Snowy Grouper`[1,,1,], 1, sum)



stock <- Hist@OM@Stock[[1]]

stock@Ages@Classes






# ----- Compare Reference Points -----










