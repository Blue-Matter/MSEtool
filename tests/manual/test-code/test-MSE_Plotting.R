testthat::context("Test MSE Plotting functions")
dev.new()

library(openMSE)

rm(list=ls())
# setup()

MPs <- avail('MP', 'DLMtool')[1:10]
MSE <- runMSE(testOM, MPs=MPs)


COSEWICobj <- runCOSEWIC(testOM, silent=TRUE)
funs <- plotFun(msg=FALSE)
funs <- funs[!funs == "plotOM"]
funs <- funs[!funs == "plot_SS2OM"]
funs <- funs[!funs == "plotmulti"]

for (ff in funs) {
  testthat::test_that("main plot MSE functions", {
    fun <- get(ff)
    if (grepl("COSEWIC", ff)) {
      testthat::expect_error(fun(COSEWICobj), NA, info=ff)
    } else {
      if ('Show' %in%names(formals(fun)) | '...' %in%names(formals(fun))) {
        testthat::expect_error(suppressMessages(suppressWarnings(fun(MSE, Show=FALSE))), NA, info=ff)
      } else {
        testthat::expect_error(fun(MSE), NA, info=ff)
      }
    }
  })
}

if(!is.null(dev.list()))  dev.off()


# COSEWIC_Hplot(COSEWICobj) # error in qs subscript

# COSEWIC_Dplot(COSEWICobj)
# COSEWIC_Pplot(COSEWICobj)
#Cplot(MSE)
#DFO_plot(MSE)
#DFO_plot2(MSE)
#DFO_proj(MSE)
#Kplot(MSE)
#NOAA_plot(MSE)
#NOAA_plot2(MSE)
#Pplot(MSE)

# Pplot2(MSE)
# PWhisker(MSE)
# SSBrefplot(MSE)
# Tplot(MSE)
# Tplot2(MSE)
# Tplot3(MSE)
# TradePlot(MSE)
# VOI(MSE) # error
# VOI2(MSE)
# VOIplot(MSE)
# wormplot(MSE)

