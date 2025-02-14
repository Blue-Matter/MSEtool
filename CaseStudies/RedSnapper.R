library(MSEtool)

la <- devtools::load_all

la()

# temporary copy over from existing
MOM <- readRDS('../SAFMC-MSE/OM_Objects/BaseCase_RS.OM')
MOM@nsim <- 2
OM2 <- Convert(MOM)  
spatial <- OM2@Stock$`Red Snapper`@Spatial

OM <- ImportBAM('Red Snapper')
OM@Stock$`SA Red Snapper`@Spatial <- spatial





messages='default'
nSim=NULL
parallel=FALSE
silent=FALSE

SimulateDEV


multiHist <- Simulate(MOM)

multiHist$`Red Snapper`$`Commercial Line`@AtAge$Removals[1,,1,] |> sum()

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
