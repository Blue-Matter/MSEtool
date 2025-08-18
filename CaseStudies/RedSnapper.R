library(MSEtool)
library(ggplot2)

la <- devtools::load_all

la()

# TODO - 
# - fix Stock/Fleet Allocation if missing 
# - make plots of selectivity, retention, landings, discards (below)
# - make plot of biomass under different projection scnearios:
#   - status quo
#   - all retained
#   - no discard mortality


OM <- ImportBAM('Red Snapper')

Hist <- Simulate(OM)

FixedF <- function(Data, F=0.2) {
  advice <- Advice()
  advice@apicalF <- F
  advice
}

FixedTAC <- function(Data) {
  advice <- Advice()
  advice@TAC <- 10000
  advice
}

FixedEffort <- function(Data) {
  advice <- Advice()
  advice@Effort <- 1
  advice
}

MPs <- c('FixedF', 'FixedTAC', 'FixedEffort')

# Test 
# - fixedF
# - TAC
# - TAC and Effort

MSE <- Project(Hist, MPs=MPs)

MSE@Effort |> ArrayReduceDims()


MSE@FDeadAtAge$`SA Red Snapper`[1,,1,,1] |> rowSums()

MSE@FDeadAtAge$`SA Red Snapper` |> ArrayReduceDims() |>
  apply(c('Age', 'TimeStep'), sum) |>
  apply('TimeStep', max)




FMSY(Hist)


# TODO - figures of projections at different fixed F levels and different discard mortality, retention curves


Fs <- seq(0, 0.3, by=0.01)
perRecruit <- CalcPerRecruit(apicalF=0.21, OM) 


perRecruit@SBiomass |> ArrayReduceDims()

data.frame(F=Fs, SPR=perRecruit@SPR[1,,,])
plot(Fs, perRecruit@SPR[1,,,], type='l', ylim=c(0,1))

lines(BAMdata$eq.series$F.eq, 
      BAMdata$eq.series$spr.eq/BAMdata$eq.series$spr.eq[1],
      col='blue')

abline(h=0.3, lty=3)

BAMdata <- BAMGetObject('Red Snapper')
BAMdata$spr.brps$F20
BAMdata$spr.brps$F30

BAMdata$t.series |> names()







Removals <- Removals(Hist, ByFleet=TRUE, ByAge=TRUE)
Landings <- Landings(Hist, ByFleet=TRUE, ByAge=TRUE)
Discards <- Removals
Discards$Variable <- 'Discards'
Discards$Value <- Removals$Value - Landings$Value

DF <- dplyr::bind_rows(Removals, Landings, Discards) |> 
  dplyr::filter(TimeStep==2019) 

DF$Fleet <- dplyr::case_match(DF$Fleet,
                              'cHL'~'Commercial Hook & Line',
                              'rHB'~'Recreational Head Boat',
                              'rGN'~'Recreational General')

DF$Variable <- factor(DF$Variable, 
                      levels=c('Removals', 'Landings', 'Discards'),
                      ordered = TRUE)

ggplot(DF, aes(x=Age, y=Value/1000, fill=Variable)) +
  facet_grid(~Fleet, scales='free') +
  geom_bar(stat='identity') +
  theme_bw() +
  labs(y='1000 lb') 

ggplot(DF, aes(x=Age, y=Value/1000, fill=Variable)) +
  facet_grid(Fleet~Variable, scales='free') +
  geom_bar(stat='identity') +
  theme_bw() +
  labs(y='1000 lb') +
  guides(fill='none')


Selectivity <- GetSelectivityAtAge(Hist)
Retention <- GetRetentionAtAge(Hist)

DF <- dplyr::bind_rows(Selectivity, Retention) |>
  dplyr::filter(Sim==1, TimeStep==2019)

DF$Fleet <- dplyr::case_match(DF$Fleet,
                              'cHL'~'Commercial Hook & Line',
                              'rHB'~'Recreational Head Boat',
                              'rGN'~'Recreational General')

ggplot(DF, aes(x=Age, y=Value, color=Variable)) +
  facet_grid(~Fleet,) +
  geom_line() +
  theme_bw() +
  labs(y='Probability') 










Hist@RefPoints@MSYRefPoints@FMSY


messages='default'
nSim=NULL
parallel=FALSE
silent=FALSE

Simulate


# # temporary copy over from existing
# MOM <- readRDS('../SAFMC-MSE/OM_Objects/BaseCase_RS.OM')
# MOM@nsim <- 2
# 
# OM2 <- Convert(MOM)  
# 
# OM@Stock$`SA Red Snapper`@Spatial <- OM2@Stock$`Red Snapper`@Spatial
# 
# 
