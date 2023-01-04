
testthat::context("Compare historical output of single-stock/single-fleet MOM vs. OM")

library(MSEtool)
library(dplyr)

OM <- MSEtool::testOM
OM@interval <- 1
OM@qinc <- OM@qcv <- rep(0, 2)
MOM <- new("MOM", 
           nsim = OM@nsim,
           proyears = OM@proyears,
           interval = OM@interval,
           Stocks = list(SubOM(OM, "Stock")), 
           Fleets = list(list(SubOM(OM, "Fleet"))),
           Obs = list(list(SubOM(OM, "Obs"))),
           Imp = list(list(SubOM(OM, "Imp"))),
           cpars = list(list(OM@cpars)),
           maxF = OM@maxF)

MOM@seed <- OM@seed

Hist <- Simulate(OM, silent = TRUE)
multiHist <- SimulateMOM(MOM, silent = TRUE, parallel = FALSE)


####### Compare Hist vs multiHist
compare_MOM <- function(x = "SBiomass", type = c("TSdata", "AtAge"), rel = FALSE) {
  type <- match.arg(type)
  MOM <- slot(multiHist[[1]][[1]], type)[[x]]
  OM <- slot(Hist, type)[[x]]
  if(rel) {
    out <- MOM/OM - 1
  } else {
    out <- MOM - OM
  }
  if(all(is.na(out)) || all(is.infinite(out))) out <- 0
  out
}


r <- lapply(names(Hist@AtAge), compare_MOM, type = "AtAge", rel = TRUE) %>% 
  sapply(function(x) max(abs(x[!is.infinite(x)]), na.rm = TRUE)) %>% structure(names = names(Hist@AtAge))



testthat::expect_s3_class(multiHist, "multiHist")
testthat::expect_lte(round(r["Number"], 2), 1) # Less than 1% discrepancy between AtAge$Number arrays


#fleetvars <- names(Hist@SampPars$Fleet)
#sapply(fleetvars, function(i) abs(multiHist[[1]][[1]]@SampPars$Fleet[[i]] - Hist@SampPars$Fleet[[i]]) %>% max()) %>%
#  structure(names = fleetvars)
#
#stockvars <- names(Hist@SampPars$Stock)
#sapply(stockvars, function(i) {
#  dif <- multiHist[[1]][[1]]@SampPars$Stock[[i]] - Hist@SampPars$Stock[[i]]
#  dif[!is.infinite(dif)] %>% abs() %>% max(na.rm = TRUE)
#}) %>% structure(names = stockvars)
#
#refpt <- names(Hist@Ref$ByYear)[-15]
#sapply(refpt, function(i) {
#  dif <- multiHist[[1]][[1]]@Ref$ByYear[[i]][, 1:OM@nyears] - Hist@Ref$ByYear[[i]][, 1:OM@nyears]
#  dif[!is.infinite(dif)] %>% abs() %>% max(na.rm = TRUE)
#}) %>% structure(names = refpt)
#
#
#refpt2 <- names(Hist@Ref$ReferencePoints)
#sapply(refpt2, function(i) {
#  dif <- multiHist[[1]][[1]]@Ref$ReferencePoints[[i]] - Hist@Ref$ReferencePoints[[i]]
#  dif[!is.infinite(dif)] %>% abs() %>% max(na.rm = TRUE)
#}) %>% structure(names = refpt2)


MSE <- Project(Hist, MPs = c('NFref', 'FMSYref', 'curEref'))
MMSE <- ProjectMOM(multiHist, MPs = c('NFref', 'FMSYref', 'curEref'))

testthat::expect_s4_class(MMSE, 'MMSE')



# curEref should result in projection Fs = last historical F
histF <- array(MSE@FM_hist[,OM@nyears], dim=dim(MSE@FM[,2,]))
testthat::expect_equal(prod(round(MMSE@FM[,1,1,3,]/histF,2)), 1)

# FMSYref should result in F/FMSY = 1
testthat::expect_equal(MMSE@F_FMSY[,1,1,2,] %>% round(2), array(1, c(MMSE@nsim, MMSE@proyears)))



#compare_MMSE <- function(x = "SSB", MPind = 1, rel = TRUE) {
#  m <- slot(MMSE, x)
#  if(length(dim(m)) == 4) {
#    multi <- m[, 1, MPind, ]
#  } else {
#    multi <- m[, 1, 1, MPind, ]
#  }
#  single <- slot(MSE, x)[, MPind, ]
#  if (rel) {
#    multi/single - 1
#  } else {
#    multi - single
#  }
#}
#
#vars <- c("SB_SBMSY", "F_FMSY", "N", "B", "SSB", "VB", "FM", "Catch", "Removals", "Effort", "TAC")
#
## Maximum discrepancy (%) between MMSE and MSE objects
#lapply(1:length(MSE@MPs), function(i) { 
#  lapply(vars, compare_MMSE, MPind = i, rel = FALSE) %>% sapply(max) %>% round(4) %>% structure(names = vars)
#}) %>% structure(names = MSE@MPs)

