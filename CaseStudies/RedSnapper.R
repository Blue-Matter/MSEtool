library(MSEtool)

la <- devtools::load_all

la()

OM <- ImportBAM('Red Snapper')

rdata <- bamExtras::rdat_RedSnapper


cbind(Year=rdata$t.series$year, SSB=rdata$t.series$SSB, Recruits=rdata$t.series$recruits)

n1 <- rdata$N.age[1,]
n1 <- rdata$N.age[1,]

z1 <- rdata$Z.age[1,]

ns1 <- rdata$N.age.spawn[1,] 

cbind( n1 *exp(-(z1*0.663)), ns1)

rdata$N.age.spawn[1,]
rdata$parms$spawn.time

rdata$F.age[1,]
rdata$t.series$SSB



(rdata$a.series$M[1] +0.002950622)*0.663


# temporary copy over from existing
MOM <- readRDS('../SAFMC-MSE/OM_Objects/BaseCase_RS.OM')
MOM@nsim <- 2
OM2 <- Convert(MOM)  


spatial <- OM2@Stock$`Red Snapper`@Spatial

OM@Stock$`SA Red Snapper`@Spatial <- spatial

messages='default'
nSim=NULL
parallel=FALSE
silent=FALSE

SimulateDEV


multiHist <- Simulate(MOM)

bam <- rdata$N.age |> apply(1, sum)
m1 <- apply(multiHist$`Red Snapper`$`Commercial Line`@AtAge$Number[1,2:21,,],2, sum)



t = multiHist$`Red Snapper`$`Commercial Line`@AtAge$Number[1,,1:2,] 
t <- apply(t, 1:2, sum)
t
rdata$N.age[1,]

t <- t[2:21,,]
apply(t, 2, sum)
2422237/ 2458905


# SPR 
multiHist$`Red Snapper`$`Commercial Line`@Ref$ByYear$F_SPR[1,,70]
multiHist$`Red Snapper`$`Commercial Line`@Ref$ByYear$SSB0[1,70]

multiHist$`Red Snapper`$`Commercial Line`@Ref$Dynamic_Unfished

multiHist$`Red Snapper`$`Commercial Line`@Ref$Dynamic_Unfished$N0[1,1]

multiHist$`Red Snapper`$`Commercial Line`@AtAge$Number[1,,1,]

multiHist$`Red Snapper`$`Commercial Line`@AtAge$Number[1,,1,] |> sum()


##############################################################################
CheckBAMN <- function(ts=1) {
  # Match  numbers from BAM to OM 
  ts <- 1
  BAMdata <- BAMGetObject()
  df <- data.frame(BAM=c(0,BAMdata$N.age[ts,]), 
                   OM= rowSums(Hist@Number$`SA Red Snapper`[1,,ts,]))
  plot(df$BAM)
  lines(df$OM)
  invisible(df)
}
##############################################################################
