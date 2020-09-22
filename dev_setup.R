library(OMtool)

devtools::load_all()









MPs <- avail('MP')

ind <- which(MPs == "DTe40")
MPs <- MPs[ind:length(MPs)]

Hist <- Simulate(testOM)

# ---- Quick test for MPs ----
MSE <- Project(Hist, MPs)


# ---- Add MultiMOM ---- 







# ---- to add ---
# - plotting functions for MOMs 



MPs <- 'FMSYref'
FMSYref <- DLMtool::FMSYref

OM <- testOM 



MSE2 <- runMSE(OM, MPs=MPs)

MSE3 <- runMSE(Hist, MPs=MPs)



plot(MSE@B_BMSY[1,1,])
lines(MSE2@B_BMSY[1,1,])
lines(MSE3@B_BMSY[1,1,], col='blue')


silent=FALSE
parallel <- FALSE 



silent=FALSE
MPs <- NA
Hist=FALSE



testOM <- tinyErr(testOM)

testOM@interval <- 1
testOM@maxF <- 10
testOM@M <- c(0.1,0.1)

MPs <- 'FMSYref'
FMSYref <- DLMtool::FMSYref


MSE <- runMSE(testOM, MPs=MPs)

Hist <- runMSE(testOM, Hist=TRUE)

MSE2 <- runMSE(Hist, MPs=MPs)

matplot(t(MSE@F_FMSY[,1,]), type="l")

matplot(t(MSE2@F_FMSY[,1,]), type="l")



myHist2 <- runMSE_single(myHist, Hist=TRUE)



# TO DO 
# - finish runMSE 
# - write SS2OM for SWO
# - start SWO analysis


# Add Pinitdist calcs 







# fls <- list.files("R")
# fls <- fls[!fls=="sysdata.rda"]
# for (fl in fls) source(file.path('R', fl))
# 
# fls <- list.files("../MSEtool/R")
# fls <- fls[!fls=="sysdata.rda"]
# for (fl in fls) source(file.path("../MSEtool/R", fl))
# cpars_info <- DLMtool:::cpars_info
# library(dplyr)
# silent=FALSE
