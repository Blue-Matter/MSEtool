library(MSEtool)
la()

dir <- "C:/Users/Admin/Documents/GitHub/IndonesiaHarvestStrategies"
source(file.path(dir, '0a. Settings.R'))
source(file.path(dir, "0b. HarvestStrategies.r"))

OM <-  readRDS(file.path(dir, 'Objects_OM/Emperor.om'))


Hist <- Simulate(OM, nsim=2)
MSE <- Project(Hist, MPs='StatusQuo')


MPs <- 'StatusQuo'
LoadArgs('Project_hist')

# - test Effort 
# - Make Effort and Landigngs functinos ... 
# - update Hist and MSE objects for CAA and CAL & update landings & discards




one <- function(Data, ...) {
  Advice <- StatusQuo(Data)
  
  Advice@Retention@Pars <- list(RL50=140,
                                RL50_95=0.0001)
  Advice
}
class(one) <- 'mp'

two <- function(Data, ...) {
  Advice <- StatusQuo(Data)
  
  Advice@Retention@Pars <- list(RL50=160,
                                RL50_95=0.0001)
  Advice
}
class(two) <- 'mp'

MPs <- c('one', 'two')
MSE <- Project_hist(Hist, MPs)


Landings(MSE) |> dplyr::filter(Year>=2022) |> 
  ggplot(aes(x=Year, y=Value, color=MP)) +
  facet_wrap(~Sim, scales='free') +
  geom_line() +
  expand_limits(y=0)


Ages <- MSE@OM@Stock$`Thumbprint Emperor`@Ages@Classes
Classes <- MSE@OM@Fleet$`Thumbprint Emperor`@Selectivity@Classes$`Emperor Fishers`
SelectLen <- MSE@OM@Fleet$`Thumbprint Emperor`@Selectivity@MeanAtLength[1,,1,1] 
SelectAge <- MSE@OM@Fleet$`Thumbprint Emperor`@Selectivity@MeanAtAge[1,,1,1] 

RL50 <- 150
newRet <- RetentionAtLength(Classes, RL50=RL50, RL50_95 = 0.00000001)  

par(mfrow=c(2,2))
plot(Classes, SelectLen, type='l')
lines(Classes, newRet[1,,1], col='red')
lines(Classes, SelectLen*newRet[1,,1], col='blue')
abline(v=RL50, lty=2)

ASK <- MSE@OM@Stock$`Thumbprint Emperor`@Length@ASK[1,,,1]


newRetAge <- newRet[1,,1] %*% t(ASK)
plot(Ages, SelectAge, type='l', ylim=c(0,1))
lines(Ages, newRetAge, col='red')
lines(Ages, SelectAge*newRetAge, col='blue')


MeanLen <- MSE@OM@Stock$`Thumbprint Emperor`@Length@MeanAtAge[1,,1]
CVLen <- MSE@OM@Stock$`Thumbprint Emperor`@Length@CVatAge[1,,1]
SDLen <- MeanLen * CVLen
Upper <- MeanLen + 2* SDLen
Lower <- MeanLen - 2*SDLen

plot(Ages, MeanLen, type='l', ylim=c(0, max(Upper)))
abline(h=RL50, lty=3)
abline(h=200, lty=3)
abline(v=8, lty=3)
lines(Ages, Lower, lty=2)
lines(Ages, Upper, lty=2)

plot(Classes, ASK[97,]*SelectLen, type='l')
lines(Classes, ASK[97,]* newRet*SelectLen, col='blue')



newRetAge2 <- (newRet[1,,1]) %*% t(MSE@OM@Stock$`Thumbprint Emperor`@Length@ASK[1,,,1])
cbind(newRetAge[1,], newRetAge2[1,]) |> matplot(type='l')

data.frame(Ages, Select=SelectAge, Retain=newRetAge[1,]) |> round(2)



data.frame(Length=Classes, Select=SelectLen, SelectRetain=SelectLen*newRet[1,,1]) |> round(2)

plot(Classes, SelectLen, type='l')
lines(Classes, (newRet[1,,1]*SelectLen), col='blue')


